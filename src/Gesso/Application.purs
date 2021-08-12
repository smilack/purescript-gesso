-- | `Application` houses functions and configuration that are shared between
-- | all Gesso applications, regardless of the rendering component.
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
  , continuous
  , Update
  , UpdateFunction
  , updateFn
  , OutputMode
  , OutputProducer
  , InputReceiver
  , noOutput
  , outputFn
  , receiveInput
  , handleOutput
  , windowCss
  , updateLocalState
  , renderApp
  ) where

import Prelude
import CSS as CSS
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Gesso.Dimensions as D
import Gesso.Time as T
import Graphics.Canvas as C
import Halogen (liftEffect)

-- | A newtype wrapper for an [`AppSpec`](#t:AppSpec)
newtype Application local input output
  = Application (AppSpec local input output)

-- | An `AppSpec` is a record consisting of the following fields:
-- |
-- | - `window` is a [`WindowMode`](#t:WindowMode) that defines how the screen
-- |   element should size and position itself.
-- | - `render` is a `Maybe [RenderMode](#t:RenderMode)` which defines how the
-- |   component should render itself (or `Nothing` if it should not render).
-- | - `update` is a `Maybe [Update](#t:Update)` which contains an update
-- |   function run on each frame.
-- | - `output` is an [`OutputMode`](#t:OutputMode) which defines how the
-- |   component should send information out to the host application.
-- | - `input` is an [`InputReceiver`](#t:InputReceiver) that defines how the
-- |   state should change in response to receiving input from the host
-- |   application.
type AppSpec local input output
  =
  { window :: WindowMode
  , render :: Maybe (RenderMode local)
  , update :: Maybe (Update local)
  , output :: OutputMode local output
  , input :: InputReceiver local input
  }

-- | A default [`AppSpec`](#t:AppSpec) which can be modified piecemeal like
-- | Halogen's `EvalSpec`.
defaultApp :: forall local input output. AppSpec local input output
defaultApp =
  { window: fixed D.sizeless
  , render: Nothing
  , update: Nothing
  , output: noOutput
  , input: const identity
  }

-- | Convert an [`AppSpec`](#t:AppSpec) into an [`Application`](#t:Application).
mkApplication
  :: forall local input output
   . AppSpec local input output
  -> Application local input output
mkApplication = Application

-- | There are three modes that determine the size and position of a Gesso
-- | component:
-- |
-- | - `Fixed` creates a screen of the specified size
-- | - `Stretch` expands to fill its containing element
-- | - `FullScreen` takes up the entire page from the top left corner to the
-- |   bottom right.
data WindowMode
  = Fixed D.Size
  | Stretch
  | Fullscreen

-- | Create a `Fixed` [`WindowMode`](#t:WindowMode)
fixed :: D.Size -> WindowMode
fixed = Fixed

-- | Create a `Stretch` [`WindowMode`](#t:WindowMode)
stretch :: WindowMode
stretch = Stretch

-- | Create a `Fullscreen` [`WindowMode`](#t:WindowMode)
fullscreen :: WindowMode
fullscreen = Fullscreen

-- | A `Continuous` render re-renders on every frame. It is useful for
-- | applications or games with lots of animations or changes that happen
-- | outside of user control.
data RenderMode local
  = Continuous (RenderFunction local)

-- | An alias for a `Context2D` rendering function.
-- |
-- | - `local` is the local state of the application
-- | - `T.Delta` is a record containing current and previous timestamps and the
-- |   time elapsed since the previous frame.
-- | - `D.Scaler` is a record containg scaling functions for converting drawing
-- |   coordinates to pixel coordinates.
-- | - `C.Context2D` is the `Context2D` for the canvas element
-- |
-- | The render function may run any operations in `Effect`, not just functions
-- | related to drawing on the canvas.
type RenderFunction local
  = local -> T.Delta -> D.Scaler -> C.Context2D -> Effect Unit

-- | Create a `Continuous` [`RenderMode`](#t:RenderMode)
continuous :: forall local. RenderFunction local -> RenderMode local
continuous = Continuous

-- | A newtype wrapper for an [`UpdateFunction`](#t:UpdateFunction)
newtype Update local
  -- Considering making an Effectful variant, like
  --   data Update state
  --     = Pure (Delta -> state -> Maybe state)
  --     | Effectful (Delta -> state -> Effect (Maybe state))
  = Update (UpdateFunction local)

-- | An `UpdateFunction` gets a `Delta` record from `Gesso.Time`, a `Scaler`
-- | from `Gesso.Dimensions`, and the current local state, and may return an
-- | updated local state if changes are necessary.
type UpdateFunction local
  = T.Delta -> D.Scaler -> local -> Maybe local

-- | Create an `Update` from an `UpdateFunction`
updateFn :: forall local. UpdateFunction local -> Update local
updateFn = Update

-- | `OutputMode` controls how the Gesso component communicates with its host
-- | application.
-- |
-- | - `NoOutput` does not send any information out from the component
-- | - `OutputFn` has an `OutputProducer` function which, when the local state
-- |   is changed, compares the old and new states and may send output through
-- |   the component's Halogen `Output` type.
data OutputMode local output
  = NoOutput
  | OutputFn (OutputProducer local output)

-- | An alias for a function that receives input from the host application and
-- | may update the local state in response to the input.
type InputReceiver local input
  = input -> local -> local

-- | An alias for a function that compares old and new local states and may
-- | send output based on the difference.
type OutputProducer local output
  = local -> local -> Maybe output

-- | Create a `NoOutput` `OutputMode`
noOutput :: forall local output. OutputMode local output
noOutput = NoOutput

-- | Create an `OutputFn` `OutputMode`
outputFn
  :: forall local output
   . OutputProducer local output
  -> OutputMode local output
outputFn = OutputFn

-- | When the component's state changes, it calls `handleOutput` to use the
-- | appropriate `OutputMode` to deal with the change.
handleOutput
  :: forall local input output m
   . MonadAff m
  => (Maybe output -> m Unit)
  -> local
  -> local
  -> Application local input output
  -> m Unit
handleOutput sendOutput local local' (Application { output }) = case output of
  OutputFn fn -> sendOutput $ fn local local'
  NoOutput -> pure unit

-- | When a component receives input through a Halogen `Query`, it calls
-- | `receiveInput` to process it and update the local state.
receiveInput
  :: forall local input output m
   . MonadAff m
  => (local -> m Unit)
  -> local
  -> input
  -> Application local input output
  -> m Unit
receiveInput saveLocal local inData (Application { input }) =
  saveLocal $ input inData local

-- | Get the appropriate CSS for the screen element based on the `WindowMode`.
windowCss
  :: forall local input output
   . Application local input output
  -> CSS.CSS
windowCss (Application { window }) = case window of
  Fixed size -> fix size
  Stretch -> stretched
  Fullscreen -> full
  where
  common = do
    CSS.key (CSS.fromString "outline") "none"

  fix size = do
    D.toSizeCss size
    common

  stretched = do
    CSS.width $ CSS.pct 100.0
    CSS.height $ CSS.pct 100.0
    common

  full = do
    CSS.width $ CSS.pct 100.0
    CSS.height $ CSS.pct 100.0
    CSS.position CSS.absolute
    CSS.left $ CSS.pct 50.0
    CSS.top $ CSS.pct 50.0
    CSS.transform $ CSS.translate (CSS.pct $ -50.0) (CSS.pct $ -50.0)
    common

-- | Calls the application's update function, returning `Nothing` if it does not
-- | have one or should not update.
updateLocalState
  :: forall local input output
   . T.Delta
  -> D.Scaler
  -> local
  -> Application local input output
  -> Maybe local
updateLocalState delta scaler localState (Application { update }) =
  update >>= \(Update fn) -> fn delta scaler localState

-- | Render the application, given the current state, delta record, scaler,
-- | canvas context, and application spec.
renderApp
  :: forall local input output
   . local
  -> T.Delta
  -> D.Scaler
  -> C.Context2D
  -> Application local input output
  -> Maybe (Effect Unit)
renderApp localState delta scaler context (Application { render }) =
  go <$> render
  where
  go (Continuous fn) = liftEffect $ fn localState delta scaler context
