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
  , OutputProducer
  , InputReceiver
  , noOutput
  , outputFn
  , globalState
  , receiveInput
  , receiveGlobal
  , handleOutput
  , windowCss
  , updateLocalState
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

newtype Application local global input output
  = Application (AppSpec local global input output)

derive instance newtypeApplication :: Newtype (Application local global input output) _

type AppSpec local global input output
  = { window :: WindowMode
    , render :: Maybe (RenderMode local)
    , update :: Maybe (Update local)
    , output :: OutputMode local output
    , input :: InputReceiver local input
    , global ::
        { toLocal :: global -> local -> local
        , fromLocal :: local -> global -> global
        }
    }

defaultApp :: forall local global input output. AppSpec local global input output
defaultApp =
  { window: fixed D.sizeless
  , render: Nothing
  , update: Nothing
  , output: noOutput
  , input: const identity
  , global: { toLocal: const identity, fromLocal: const identity }
  }

mkApplication :: forall local global input output. AppSpec local global input output -> Application local global input output
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

data RenderMode local
  = OnChange (RenderFunction local)
  | Continuous (RenderFunction local)

type RenderFunction local
  = local -> T.Delta -> D.Scaler -> C.Context2D -> Effect Unit

onChange :: forall local. RenderFunction local -> RenderMode local
onChange = OnChange

continuous :: forall local. RenderFunction local -> RenderMode local
continuous = Continuous

-- Considering making an Effectful variant, i.e.
-- data Update state = Pure (Delta -> state -> state) | Effectful (Delta -> state -> Effect state)
newtype Update local
  = Update (UpdateFunction local)

type UpdateFunction local
  = T.Delta -> local -> local

derive instance newtypeUpdate :: Newtype (Update local) _

updateFn :: forall local. UpdateFunction local -> Update local
updateFn = Update

data OutputMode local output
  = NoOutput
  | OutputFn (OutputProducer local output)
  | GlobalState

type InputReceiver local input
  = input -> local -> local

type OutputProducer local output
  = local -> local -> Maybe output

noOutput :: forall local output. OutputMode local output
noOutput = NoOutput

outputFn :: forall local output. OutputProducer local output -> OutputMode local output
outputFn = OutputFn

globalState :: forall local output. OutputMode local output
globalState = GlobalState

handleOutput ::
  forall local global input output m.
  MonadAff m =>
  ManageState m global =>
  (local -> m Unit) ->
  (Maybe output -> m Unit) ->
  local ->
  local ->
  Application local global input output ->
  m Unit
handleOutput saveLocal sendOutput local local' (Application { output, global }) = go output
  where
  go (OutputFn fn) = do
    saveLocal local'
    sendOutput $ fn local local'

  go GlobalState = do
    gState <- GM.getState
    GM.putState $ global.fromLocal local' gState

  go NoOutput = saveLocal local'

receiveInput ::
  forall local global input output m.
  MonadAff m =>
  ManageState m global =>
  (local -> m Unit) ->
  local ->
  input ->
  Application local global input output ->
  m Unit
receiveInput saveLocal local inData (Application { input }) = saveLocal $ input inData local

receiveGlobal ::
  forall local global input output m.
  MonadAff m =>
  ManageState m global =>
  (local -> m Unit) ->
  local ->
  global ->
  Application local global input output ->
  m Unit
receiveGlobal saveLocal local globalState' (Application { global }) = saveLocal $ global.toLocal globalState' local

windowCss :: forall local global input output. Application local global input output -> CSS.CSS
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
updateLocalState :: forall local global input output. T.Delta -> local -> Application local global input output -> Maybe local
updateLocalState delta localState (Application { update }) = update >>= \(Update fn) -> Just $ fn delta localState

data RequestFrame
  = Continue
  | Stop

renderApp ::
  forall local global input output.
  local ->
  T.Delta ->
  D.Scaler ->
  C.Context2D ->
  Application local global input output ->
  Maybe (Effect RequestFrame)
renderApp localState delta scaler context (Application { render }) = go <$> render
  where
  go = case _ of
    OnChange fn -> do
      run fn
      pure Stop
    Continuous fn -> do
      run fn
      pure Continue

  run fn = liftEffect $ fn localState delta scaler context

renderOnUpdate :: forall local global input output. Application local global input output -> RequestFrame
renderOnUpdate (Application { render }) = case render of
  Just (OnChange _) -> Continue
  Just (Continuous _) -> Stop
  Nothing -> Stop
