-- | `Application` houses functions and configuration that are shared between
-- | all Gesso applications, regardless of the rendering component.
module Gesso.Application
  ( AppSpec
  , InputReceiver
  , OutputProducer
  , RenderFunction
  , TimestampedUpdate
  , UpdateFunction
  , WindowMode(..)
  , defaultApp
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gesso.Dimensions as D
import Gesso.Time as T

-- | `AppSpec` holds information about the setup and behavior of a Gesso
-- | component.
-- |
-- | - `window` defines how the screen element should size and position itself.
-- | - `render` draws on the component every animation frame.
-- | - `update` runs on each animation frame, just before `render`.
-- | - `output` defines how (or if) the component should send information out to
-- |   the host application.
-- | - `input` defines how the component's state should change in response to
-- |   receiving input from the host application.
type AppSpec context local input output =
  { window :: WindowMode
  , render :: RenderFunction context local
  , update :: UpdateFunction local
  , output :: OutputProducer local output
  , input :: InputReceiver local input
  }

-- | A default `AppSpec` which can be modified piecemeal like Halogen's
-- | `EvalSpec`. It does nothing on its own.
defaultApp
  :: forall context local input output
   . AppSpec context local input output
defaultApp =
  { window: Fixed D.sizeless
  , render: \_ _ _ _ -> pure unit
  , update: \_ _ _ -> pure Nothing
  , output: \_ _ _ _ -> pure Nothing
  , input: \_ _ _ _ -> pure Nothing
  }

-- | There are three modes that determine the size and position of a Gesso
-- | component:
-- |
-- | - `Fixed` creates a screen of the specified size.
-- | - `Stretch` expands to fill its containing element.
-- | - `FullScreen` takes up the entire page from the top left corner to the
-- |   bottom right.
data WindowMode
  = Fixed D.Size
  | Stretch
  | Fullscreen

-- | A function that draws on the component. It knows the following:
-- |
-- | - `local` is the local state of the application
-- | - `Delta` is a record containing current and previous timestamps and the
-- |   time elapsed since the previous frame.
-- | - `Scaler` is a record containg scaling functions for converting canvas
-- |   coordinates to screen coordinates.
-- | - `context` is the drawing context of canvas element, like `Context2D`
-- |
-- | The render function may run any operations in `Effect`, not just functions
-- | related to drawing on the canvas.
type RenderFunction context local =
  local -> T.Delta -> D.Scaler -> context -> Effect Unit

-- | An function that may update the application state. It runs on every frame,
-- | before the render function. It knows the following:
-- |
-- | - `Delta` is a record containing current and previous timestamps and the
-- |   time elapsed since the previous frame.
-- | - `Scaler` is a record containg scaling functions for converting canvas
-- |   coordinates to screen coordinates.
-- | - `local` is the local state of the application
-- |
-- | The update function may return a new local state if changes are necessary
-- | (or `Nothing` if not).
-- |
-- | This type is also used by Interaction handlers and when receiving input
-- | from a host application.
type UpdateFunction local =
  T.Delta -> TimestampedUpdate local

-- | A partially applied `UpdateFunction` that already has the `Delta` record.
type TimestampedUpdate local =
  D.Scaler -> local -> Effect (Maybe local)

-- | An input receiver is a variant of an update function that can receive
-- | information from the component's parent and produce an update function
-- | in response.
type InputReceiver local input = input -> UpdateFunction local

-- | When the local state of an application changes, an output producer compares
-- | the old and new local states and may send output to the component's parent
-- | based on the difference. The old state is first and the new state is
-- | second.
type OutputProducer local output =
  T.Delta -> D.Scaler -> local -> local -> Effect (Maybe output)
