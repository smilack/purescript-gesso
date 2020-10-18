module Gesso.Application
  ( Application
  , AppSpec
  , defaultApp
  , mkApplication
  , WindowMode
  , fixed
  , stretch
  , fullscreen
  , RenderMode
  , RenderFunction
  , onChange
  , continuous
  , Update
  , UpdateFunction
  , updateFn
  , OutputMode
  , noOutput
  , outputFn
  , globalState
  , updateLocal
  , handleOutput
  , windowCss
  , updateAppState
  , RequestFrame(..)
  , renderApp
  , renderOnUpdate
  ) where

import Prelude
import CSS as CSS
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Gesso.Dimensions as D
import Gesso.GessoM (class ManageState)
import Gesso.GessoM as GM
import Gesso.Time as T
import Graphics.Canvas as C
import Halogen (liftEffect)

newtype Application state output global
  = Application (AppSpec state output global)

derive instance newtypeApplication :: Newtype (Application state output global) _

type AppSpec state output global
  = { window :: WindowMode
    , render :: Maybe (RenderMode state)
    , update :: Maybe (Update state)
    , output :: OutputMode state output
    , global ::
        { toLocal :: global -> state -> state
        , fromLocal :: state -> global -> global
        }
    }

defaultApp :: forall state output global. AppSpec state output global
defaultApp =
  { window: fixed D.sizeless
  , render: Nothing
  , update: Nothing
  , output: noOutput
  , global: { toLocal: const identity, fromLocal: const identity }
  }

mkApplication :: forall state output global. AppSpec state output global -> Application state output global
mkApplication = Application

data WindowMode
  = Fixed D.Size
  | Stretch
  | Fullscreen

fixed :: D.Size -> WindowMode
fixed = Fixed

stretch :: WindowMode
stretch = Stretch

fullscreen :: WindowMode
fullscreen = Fullscreen

data RenderMode state
  = OnChange (RenderFunction state)
  | Continuous (RenderFunction state)

type RenderFunction state
  = state -> T.Delta -> D.Scaler -> C.Context2D -> Effect Unit

onChange :: forall state. RenderFunction state -> RenderMode state
onChange = OnChange

continuous :: forall state. RenderFunction state -> RenderMode state
continuous = Continuous

-- Considering making an Effectful variant, i.e.
-- data Update state = Pure (Delta -> state -> state) | Effectful (Delta -> state -> Effect state)
newtype Update state
  = Update (UpdateFunction state)

type UpdateFunction state
  = T.Delta -> state -> state

derive instance newtypeUpdate :: Newtype (Update state) _

updateFn :: forall state. UpdateFunction state -> Update state
updateFn = Update

data OutputMode state output
  = NoOutput
  | OutputFn (OutputProducer state output)
  | GlobalState

type OutputProducer state output
  = state -> state -> Maybe output

noOutput :: forall state output. OutputMode state output
noOutput = NoOutput

outputFn :: forall state output. OutputProducer state output -> OutputMode state output
outputFn = OutputFn

globalState :: forall state output. OutputMode state output
globalState = GlobalState

handleOutput ::
  forall state output global m.
  MonadAff m =>
  ManageState m global =>
  (state -> Maybe output -> m Unit) ->
  state ->
  state ->
  Application state output global ->
  m Unit
handleOutput sendOutput state state' (Application { output, global }) = go output
  where
  go (OutputFn fn) = do
    sendOutput state' $ fn state state'

  go GlobalState = do
    gState <- GM.getState
    GM.putState $ global.fromLocal state' gState

  go NoOutput = pure unit

updateLocal ::
  forall state output global.
  state ->
  global ->
  Application state output global ->
  state
updateLocal state globalState' (Application { global }) = global.toLocal globalState' state

windowCss :: forall state output global. Application state output global -> CSS.CSS
windowCss (Application { window }) = case window of
  Fixed size -> D.toSizeCss size
  Stretch -> stretched
  Fullscreen -> full
  where
  stretched = do
    CSS.width $ CSS.pct 100.0
    CSS.height $ CSS.pct 100.0

  full = do
    CSS.width $ CSS.pct 100.0
    CSS.height $ CSS.pct 100.0
    CSS.position CSS.absolute
    CSS.left $ CSS.pct 50.0
    CSS.top $ CSS.pct 50.0
    CSS.transform $ CSS.translate (CSS.pct $ -50.0) (CSS.pct $ -50.0)

--return Nothing if there's no update function
updateAppState :: forall state output global. T.Delta -> state -> Application state output global -> Maybe state
updateAppState delta appState (Application { update }) = update >>= \(Update fn) -> Just $ fn delta appState

data RequestFrame
  = Continue
  | Stop

renderApp ::
  forall state output global.
  state ->
  T.Delta ->
  D.Scaler ->
  C.Context2D ->
  Application state output global ->
  Maybe (Effect RequestFrame)
renderApp appState delta scaler context (Application { render }) = go <$> render
  where
  go = case _ of
    OnChange fn -> do
      run fn
      pure Stop
    Continuous fn -> do
      run fn
      pure Continue

  run fn = liftEffect $ fn appState delta scaler context

renderOnUpdate :: forall state output global. Application state output global -> RequestFrame
renderOnUpdate (Application { render }) = case render of
  Just (OnChange _) -> Continue
  Just (Continuous _) -> Stop
  Nothing -> Stop
