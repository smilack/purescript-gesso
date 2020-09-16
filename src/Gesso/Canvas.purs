module Gesso.Canvas where

import Prelude
import Effect (Effect)
import Effect.Ref as Ref
import Graphics.Canvas (Context2D)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as HS
import CSS as CSS
import Color as Color
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Halogen.Query.EventSource as ES
import Data.Traversable (traverse_, sequence)
import Data.Foldable (sequence_)
import Gesso.Window as Window
import Web.HTML (window)
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas as Canvas
import Data.Newtype (class Newtype, unwrap)

type BoundingBox
  = { x :: Number, y :: Number, w :: Number, h :: Number }

newtype ViewBox
  = ViewBox BoundingBox

newtype ClientRect
  = ClientRect BoundingBox

data RenderStyle state
  = NoRender
  | Continuous (Number -> state -> Context2D -> Effect Unit)
  | OnChange (state -> Context2D -> Effect Unit)

data Action state
  = Start
  | Stop
  | Tick
  | Receive (Input state)

newtype Input state
  = Input { origin :: Origin
    , dimensions :: Dimensions
    , renderFn :: RenderStyle state
    , externalState :: state
    }

data Dimensions
  = WH { width :: Number, height :: Number }
  | WAR { width :: Number, aspectRatio :: AspectRatio }

data AspectRatio
  = AspectRatio Number Number

newtype State state
  = State { viewBox :: ViewBox
    , clientRect :: ClientRect
    , renderFn :: RenderStyle state
    , subscription :: Maybe H.SubscriptionId
    , time :: Number
    , frames :: Number
    , context :: Maybe Canvas.Context2D
    , name :: String
    , externalState :: Maybe state
    }

derive instance newtypeState :: Newtype (State state) _

type Point
  = { x :: Number, y :: Number }

newtype Origin
  = Origin Point

component ::
  forall query state output m.
  MonadAff m =>
  H.Component HH.HTML query (Input state) output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: eval
    }
  

eval ::
  forall state slots output query m.
  MonadAff m =>
  H.HalogenQ query (Action state) (Input state) ~>
  H.HalogenM (State state) (Action state) slots output m
eval = H.mkEval
  (H.defaultEval
    { handleAction = handleAction
    , initialize = Just Start
    , receive = receive })
  where
    receive :: Input state -> Maybe (Action state)
    receive = Just <<< Receive
        

initialState :: forall state. Input state -> State state
initialState (Input { origin: (Origin o), dimensions, renderFn, externalState }) =
  State { viewBox: ViewBox { x: o.x, y: o.y, w, h }
  , clientRect: ClientRect { x: 0.0, y: 0.0, w: 0.0, h: 0.0 }
  , renderFn
  , subscription: Nothing
  , time: 0.0
  , frames: 0.0
  , context: Nothing
  , name: "screen"
  , externalState: Just externalState
  }
  where
  { w, h } = case dimensions of
    WH { width, height } -> { w: width, h: height }
    WAR { width, aspectRatio: (AspectRatio w h) } -> { w: width, h: width * h / w }

render ::
  forall state slots m.
  State state -> H.ComponentHTML (Action state) slots m
render (State { viewBox: (ViewBox viewBox), name }) =
  HH.canvas
    [ HP.id_ name
    , HP.width $ round viewBox.w
    , HP.height $ round viewBox.h
    , HS.style do
        CSS.border CSS.double (CSS.px 5.0) Color.black
        CSS.width $ CSS.px viewBox.w
        CSS.height $ CSS.px viewBox.h
    ]

handleAction ::
  forall state slots output m.
  MonadAff m =>
  Action state ->
  H.HalogenM (State state) (Action state) slots output m Unit
handleAction = case _ of
  Start -> do
    handleAction Stop
    renderFn <- H.gets $ _.renderFn <<< unwrap
    name <- H.gets $ _.name <<< unwrap
    stateContext <- H.gets $ _.context <<< unwrap
    mcontext <- H.liftEffect $ pickContext stateContext name
    mexternalState <- H.gets $ _.externalState <<< unwrap
    subscription <-
      H.subscribe
        $ ES.effectEventSource \emitter -> do
            ref <- Ref.new Nothing
            let
              rAFLoop :: Number -> Effect Unit
              rAFLoop time = do
                case renderFn of
                  NoRender -> pure unit
                  Continuous fn -> do
                    ES.emit emitter Tick
                    sequence_ $ fn time <$> mexternalState <*> mcontext
                  OnChange fn -> do
                    sequence_ $ fn <$> mexternalState <*> mcontext
                id <- Window.requestAnimationFrame rAFLoop =<< window
                Ref.write (Just id) ref
            id1 <- Window.requestAnimationFrame rAFLoop =<< window
            Ref.write (Just id1) ref
            ES.emit emitter Tick
            pure
              $ ES.Finalizer do
                  Ref.read ref
                    >>= traverse_ \id ->
                        Window.cancelAnimationFrame id =<< window
    let
      updateSub :: H.SubscriptionId -> State state -> State state
      updateSub subId (State state) =
        (State state { subscription = Just subId })
    H.modify_ (updateSub subscription)
  Stop -> do
    subscription <- H.gets $ _.subscription <<< unwrap
    traverse_ unsubscribe subscription
  Tick -> H.modify_ $ (\st -> State $ st { frames = st.frames + 1.0 }) <<< unwrap
  Receive (Input i) -> H.modify_ $ (\st -> State $ st { externalState = Just i.externalState }) <<< unwrap

unsubscribe ::
  forall state slots output m.
  MonadAff m =>
  H.SubscriptionId ->
  H.HalogenM (State state) (Action state) slots output m Unit
unsubscribe subscription = do
  H.unsubscribe subscription
  H.modify_ updateSub
  where
    updateSub :: State state -> State state
    updateSub (State state) =
      (State state { subscription = Nothing })

getContext :: String -> Effect (Maybe Canvas.Context2D)
getContext name = do
  mcanvas <- Canvas.getCanvasElementById name
  mcontext <- sequence $ Canvas.getContext2D <$> mcanvas
  pure mcontext

pickContext ::
  Maybe Canvas.Context2D -> String -> Effect (Maybe Canvas.Context2D)
pickContext stateContext name = case stateContext of
  Nothing -> do
    mcontext <- getContext name
    pure mcontext
  Just context -> do
    pure $ Just context

-- handleQuery ::
--   forall state slots output m.
--   MonadAff m =>
--   Query state -> H.HalogenM (State state) Action slots output m (Maybe state)
-- handleQuery = case _ of
--   UpdateExternalState extSt -> do
--     H.modify_ (_ { externalState = extSt })
--     pure Nothing
