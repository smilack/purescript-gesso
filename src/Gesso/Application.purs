module Gesso.Application
  ( Application
  , AppSpec
  , defaultApp
  , mkApplication
  , WindowStyle
  , fixed
  , stretch
  , fullscreen
  , RenderStyle
  , RenderFunction
  , onChange
  , continuous
  , Update
  , UpdateFunction
  , updateFn
  , windowCss
  , updateAppState
  , RequestFrame(..)
  , renderApp
  , renderOnUpdate
  ) where

import Prelude
import CSS as CSS
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Gesso.Dimensions as D
import Gesso.Time as T
import Graphics.Canvas as C
import Halogen (liftEffect)

newtype Application state
  = Application (AppSpec state)

derive instance newtypeApplication :: Newtype (Application state) _

type AppSpec state
  = { window :: WindowStyle
    , render :: Maybe (RenderStyle state)
    , update :: Maybe (Update state)
    }

defaultApp :: forall state. AppSpec state
defaultApp =
  { window: Fixed D.sizeless
  , render: Nothing
  , update: Nothing
  }

mkApplication :: forall state. AppSpec state -> Application state
mkApplication = Application

data WindowStyle
  = Fixed D.Size
  | Stretch
  | Fullscreen

fixed :: D.Size -> WindowStyle
fixed = Fixed

stretch :: WindowStyle
stretch = Stretch

fullscreen :: WindowStyle
fullscreen = Fullscreen

data RenderStyle state
  = OnChange (RenderFunction state)
  | Continuous (RenderFunction state)

type RenderFunction state
  = state -> T.Delta -> D.Scaler -> C.Context2D -> Effect Unit

onChange :: forall state. RenderFunction state -> RenderStyle state
onChange = OnChange

continuous :: forall state. RenderFunction state -> RenderStyle state
continuous = Continuous

newtype Update state
  = Update (UpdateFunction state)

type UpdateFunction state
  = T.Delta -> state -> state

derive instance newtypeUpdate :: Newtype (Update state) _

updateFn :: forall state. UpdateFunction state -> Update state
updateFn = Update

windowCss :: forall state. Application state -> CSS.CSS
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
updateAppState :: forall state. T.Delta -> state -> Application state -> Maybe state
updateAppState delta appState (Application app@{ update }) =
  unwrap
    <$> update
    <*> pure delta
    <*> pure appState

data RequestFrame
  = Continue
  | Stop

renderApp ::
  forall state.
  state ->
  T.Delta ->
  D.Scaler ->
  C.Context2D ->
  Application state ->
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

renderOnUpdate :: forall state. Application state -> RequestFrame
renderOnUpdate (Application { render }) = case render of
  Just (OnChange _) -> Continue
  Just (Continuous _) -> Stop
  Nothing -> Stop
