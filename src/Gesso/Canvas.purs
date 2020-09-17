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
import Data.Maybe (Maybe(..))
import Halogen.Query.EventSource as ES
import Data.Traversable (traverse_, sequence)
import Data.Foldable (sequence_)
import Gesso.Window as Window
import Web.HTML (window)
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas as Canvas
import Data.Newtype (class Newtype, unwrap)
import Gesso.Dimensions as Dims

data RenderStyle appState
  = NoRender
  | Continuous (Number -> appState -> Context2D -> Effect Unit)
  | OnChange (appState -> Context2D -> Effect Unit)

data Action appState
  = Start
  | Stop
  | Tick

newtype Input appState
  = Input (InitialInput appState)

type InitialInput appState
  = { boundingBox :: Dims.Dimensions
    , renderFn :: RenderStyle appState
    , appState :: appState
    }

newtype State appState
  = State
  { viewBox :: Dims.Dimensions
  , clientRect :: Maybe Dims.ClientRect
  , renderFn :: RenderStyle appState
  , tickSub :: Maybe H.SubscriptionId
  , time :: Number
  , frames :: Number
  , context :: Maybe Canvas.Context2D
  , name :: String
  , appState :: appState
  }

derive instance newtypeState :: Newtype (State appState) _

component ::
  forall query appState output m.
  MonadAff m =>
  H.Component HH.HTML query (Input appState) output m
component = H.mkComponent { initialState, render, eval }
  where
  eval ::
    forall slots.
    MonadAff m =>
    H.HalogenQ query (Action appState) (Input appState)
      ~> H.HalogenM (State appState) (Action appState) slots output m
  eval =
    H.mkEval
      ( H.defaultEval
          { handleAction = handleAction
          , initialize = Just Start
          }
      )

initialState :: forall appState. Input appState -> State appState
initialState (Input { boundingBox, renderFn, appState }) =
  State
    { viewBox: boundingBox
    , clientRect: Nothing
    , renderFn
    , tickSub: Nothing
    , time: 0.0
    , frames: 0.0
    , context: Nothing
    , name: "screen"
    , appState
    }

render ::
  forall appState slots m.
  State appState -> H.ComponentHTML (Action appState) slots m
render (State { viewBox, name }) =
  HH.canvas
    $ [ HP.id_ name
      , HS.style do
          CSS.border CSS.double (CSS.px 5.0) Color.black
          Dims.toSizeCss viewBox
      ]
    <> (Dims.toSizeProps viewBox)

handleAction ::
  forall appState slots output m.
  MonadAff m =>
  Action appState ->
  H.HalogenM (State appState) (Action appState) slots output m Unit
handleAction = case _ of
  Start -> do
    handleAction Stop
    renderFn <- H.gets $ _.renderFn <<< unwrap
    name <- H.gets $ _.name <<< unwrap
    stateContext <- H.gets $ _.context <<< unwrap
    mcontext <- H.liftEffect $ pickContext stateContext name
    appState <- H.gets $ _.appState <<< unwrap
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
                    sequence_ $ fn time appState <$> mcontext
                  OnChange fn -> do
                    sequence_ $ fn appState <$> mcontext
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
      updateSub :: H.SubscriptionId -> State appState -> State appState
      updateSub subId (State state) = (State state { tickSub = Just subId })
    H.modify_ (updateSub subscription)
  Stop -> do
    subscription <- H.gets $ _.tickSub <<< unwrap
    traverse_ unsubscribe subscription
  Tick -> pure unit --H.modify_ $ (\st -> State $ st { frames = st.frames + 1.0 }) <<< unwrap

unsubscribe ::
  forall appState slots output m.
  MonadAff m =>
  H.SubscriptionId ->
  H.HalogenM (State appState) (Action appState) slots output m Unit
unsubscribe subscription = do
  H.unsubscribe subscription
  H.modify_ updateSub
  where
  updateSub :: State appState -> State appState
  updateSub (State state) = (State state { tickSub = Nothing })

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
