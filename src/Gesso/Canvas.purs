module Gesso.Canvas where

import Prelude
import Color as Color
import CSS as CSS
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Gesso.Dimensions as Dims
import Gesso.Window (requestAnimationFrame)
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getContext2D)
import Halogen as H
import Halogen.HTML (canvas, HTML)
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties (id_)
import Halogen.Query.EventSource as ES
import Web.DOM.Element (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode, HTMLDocument)
import Web.HTML.HTMLElement (getBoundingClientRect, fromElement, HTMLElement, DOMRect)
import Web.HTML.Window (toEventTarget, document)

data RenderStyle appState
  = NoRender
  | Continuous (Number -> Number -> appState -> Context2D -> Effect Unit)
  | OnChange (appState -> Context2D -> Effect Unit)

data Action
  = Initialize
  | Finalize
  | HandleResize
  | Tick (Maybe Number)

data Query appState a
  = UpdateAppState appState a

newtype Input appState
  = Input
  { boundingBox :: Dims.Dimensions
  , renderFn :: RenderStyle appState
  , appState :: appState
  }

type State appState
  = { viewBox :: Dims.Dimensions
    , clientRect :: Maybe Dims.ClientRect
    , renderFn :: RenderStyle appState
    , frames :: Number
    , canvas :: Maybe CanvasElement
    , context :: Maybe Context2D
    , resizeSub :: Maybe H.SubscriptionId
    , name :: String
    , appState :: appState
    }

component ::
  forall appState output m.
  MonadAff m =>
  H.Component HTML (Query appState) (Input appState) output m
component = H.mkComponent { initialState, render, eval }
  where
  eval ::
    forall slots.
    MonadAff m =>
    H.HalogenQ (Query appState) Action (Input appState)
      ~> H.HalogenM (State appState) Action slots output m
  eval =
    H.mkEval
      ( H.defaultEval
          { handleAction = handleAction
          , initialize = Just Initialize
          }
      )

initialState :: forall appState. Input appState -> State appState
initialState (Input { boundingBox, renderFn, appState }) =
  { viewBox: boundingBox
  , clientRect: Nothing
  , renderFn
  , frames: 0.0
  , resizeSub: Nothing
  , canvas: Nothing
  , context: Nothing
  , name: "screen"
  , appState
  }

render ::
  forall appState slots m.
  State appState -> H.ComponentHTML Action slots m
render { viewBox, name } =
  canvas
    $ [ id_ name
      , style do
          CSS.border CSS.double (CSS.px 5.0) Color.black
          Dims.toSizeCss viewBox
      ]
    <> (Dims.toSizeProps viewBox)

handleAction ::
  forall appState slots output m.
  MonadAff m =>
  Action -> H.HalogenM (State appState) Action slots output m Unit
handleAction = case _ of
  Finalize -> pure unit
  Initialize -> do
    name <- H.gets _.name
    clientRect <- H.liftEffect $ getCanvasClientRect name
    mcontext <- H.liftEffect $ getContext name
    wnd <- H.liftEffect window
    resizeSub <-
      H.subscribe
        $ ES.eventListenerEventSource
            (EventType "resize")
            (toEventTarget wnd)
            (const $ Just HandleResize)
    H.modify_ \state -> state { context = mcontext, resizeSub = Just resizeSub, clientRect = clientRect }
    handleAction $ Tick Nothing
  HandleResize -> do
    name <- H.gets _.name
    clientRect <- H.liftEffect $ getCanvasClientRect name
    H.modify_ \state -> state { clientRect = clientRect }
  Tick mLastTime -> do
    animationFrame mLastTime

animationFrame ::
  forall appState slots output m.
  MonadAff m =>
  Maybe Number -> H.HalogenM (State appState) Action slots output m Unit
animationFrame mLastTime = do
  mcontext <- H.gets _.context
  appState <- H.gets _.appState
  renderFn <- H.gets _.renderFn
  _ <-
    H.subscribe' \_ ->
      ES.effectEventSource \emitter -> do
        _ <-
          window
            >>= requestAnimationFrame \timestamp -> do
                case renderFn of
                  NoRender -> pure unit
                  OnChange fn -> sequence_ $ fn appState <$> mcontext
                  Continuous fn -> do
                    case mLastTime of
                      Nothing -> pure unit
                      Just lastTime -> do
                        let
                          delta = timestamp - lastTime
                        sequence_ $ fn timestamp delta appState <$> mcontext
                    ES.emit emitter $ Tick $ Just timestamp
                ES.close emitter
        mempty
  pure unit

getContext :: String -> Effect (Maybe Context2D)
getContext name = do
  mcanvas <- getCanvasElementById name
  mcontext <- sequence $ getContext2D <$> mcanvas
  pure mcontext

getCanvasClientRect :: String -> Effect (Maybe Dims.ClientRect)
getCanvasClientRect name = do
  (doc :: HTMLDocument) <- document =<< window
  (mcanvas :: Maybe Element) <- getElementById name $ toNonElementParentNode doc
  (mcanvasEl :: Maybe HTMLElement) <- pure $ mcanvas >>= fromElement
  (mbounding :: Maybe DOMRect) <- traverse getBoundingClientRect mcanvasEl
  pure $ Dims.fromDOMRect <$> mbounding
