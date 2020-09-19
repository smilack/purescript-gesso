module Gesso.Canvas where

import Prelude
import CSS as CSS
import Data.Foldable (sequence_, traverse_)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Gesso.Dimensions as Dims
import Gesso.Window (requestAnimationFrame)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import Halogen as H
import Halogen.HTML (canvas, HTML, memoized)
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties (id_)
import Halogen.Query.EventSource as ES
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
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
    , canvas :: Maybe HTMLElement
    , context :: Maybe Context2D
    , resizeSub :: Maybe H.SubscriptionId
    , name :: String
    , appState :: appState
    }

component ::
  forall appState output m.
  MonadAff m =>
  H.Component HTML (Query appState) (Input appState) output m
component =
  H.mkComponent
    { initialState
    , render: memoized (eq `on` _.clientRect) render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              , finalize = Just Finalize
              }
    }

initialState :: forall appState. Input appState -> State appState
initialState (Input { boundingBox, renderFn, appState }) =
  { viewBox: boundingBox
  , clientRect: Nothing
  , renderFn
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
      , style fullscreenStyle
      ]
    <> Dims.toSizeProps viewBox

handleAction ::
  forall appState slots output m.
  MonadAff m =>
  Action -> H.HalogenM (State appState) Action slots output m Unit
handleAction = case _ of
  Initialize -> do
    name <- H.gets _.name
    mcontext <- H.liftEffect $ getContext name
    wnd <- H.liftEffect window
    resizeSub <-
      H.subscribe
        $ ES.eventListenerEventSource
            (EventType "resize")
            (toEventTarget wnd)
            (const $ Just HandleResize)
    mcanvas <- H.liftEffect $ getCanvasElement name
    clientRect <- H.liftEffect $ getCanvasClientRect mcanvas
    H.modify_ (_ { context = mcontext, resizeSub = Just resizeSub, clientRect = clientRect, canvas = mcanvas })
    handleAction $ Tick Nothing
  HandleResize -> updateClientRect
  Tick mLastTime -> animationFrame mLastTime
  Finalize -> unsubscribeResize

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
  mcontext <- traverse getContext2D mcanvas
  pure mcontext

getCanvasElement :: String -> Effect (Maybe HTMLElement)
getCanvasElement name = do
  doc <- document =<< window
  mcanvas <- getElementById name $ toNonElementParentNode doc
  pure $ mcanvas >>= fromElement

getCanvasClientRect :: Maybe HTMLElement -> Effect (Maybe Dims.ClientRect)
getCanvasClientRect mcanvas = do
  (mbounding :: Maybe DOMRect) <- traverse getBoundingClientRect mcanvas
  pure $ Dims.fromDOMRect <$> mbounding

updateClientRect ::
  forall appState action slots output m.
  MonadAff m =>
  H.HalogenM (State appState) action slots output m Unit
updateClientRect = do
  mcanvas <- H.gets _.canvas
  clientRect <- H.liftEffect $ getCanvasClientRect mcanvas
  H.modify_ (_ { clientRect = clientRect })

fullscreenStyle :: CSS.CSS
fullscreenStyle = do
  CSS.width $ CSS.pct 100.0
  CSS.height $ CSS.pct 100.0
  CSS.position CSS.absolute
  CSS.left $ CSS.pct 50.0
  CSS.top $ CSS.pct 50.0
  CSS.transform $ CSS.translate (CSS.pct $ -50.0) (CSS.pct $ -50.0)

unsubscribeResize ::
  forall appState action slots output m.
  MonadAff m =>
  H.HalogenM (State appState) action slots output m Unit
unsubscribeResize = do
  mresizeSub <- H.gets _.resizeSub
  traverse_ H.unsubscribe mresizeSub
