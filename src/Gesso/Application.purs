-- | `Application` houses functions and configuration that are shared between
-- | all Gesso applications, regardless of the rendering component.
module Gesso.Application
  ( AppBehavior
  , AppSpec
  , WindowMode(..)
  , defaultBehavior
  , module Exports
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Gesso.Application.Behavior (FixedUpdate, InputReceiver, OutputProducer, RenderFunction, UpdateFunction)
import Gesso.Application.Behavior (FixedUpdate, InputReceiver, OutputProducer, RenderFunction, TimestampedUpdate, UpdateFunction) as Exports
import Gesso.Geometry (Area, Rect)
import Gesso.Interactions (Interactions)
import Gesso.Interactions (default) as Interactions
import Gesso.Time (never)

-- | `AppSpec` holds information about the setup and behavior of a Gesso
-- | component.
-- |
-- | - `name` is the name of the application, which doubles as the HTML `id` for
-- |   the canvas element.
-- | - `initialState` is the initial local state for the application.
-- | - `viewBox` is the desired dimensions for the drawing area.
-- | - `window` defines how the screen element should size and position itself.
-- | - `behavior` contains functions that control i/o, updates, and rendering.
type AppSpec state input output =
  { name :: String
  , initialState :: state
  , viewBox :: Rect
  , window :: WindowMode
  , behavior :: AppBehavior state input output
  }

-- | `AppBehavior` holds the functions that make an application run.
-- |
-- | - `render` draws on the component every animation frame.
-- | - `update` runs on each animation frame, just before `render`.
-- | - `fixed` runs at a set interval of time.
-- | - `interactions` are events which will be attached to the canvas element.
-- | - `output` defines how (or if) the component should send information out to
-- |   the host application.
-- | - `input` defines how the component's state should change in response to
-- |   receiving input from the host application.
type AppBehavior state input output =
  { render :: RenderFunction state
  , update :: UpdateFunction state
  , fixed :: FixedUpdate state
  , interactions :: Interactions state
  , output :: OutputProducer state output
  , input :: InputReceiver state input
  }

-- | A default `AppBehavior` which can be modified piecemeal like Halogen's
-- | `EvalSpec`. It does nothing on its own.
defaultBehavior
  :: forall state input output
   . AppBehavior state input output
defaultBehavior =
  { render: \_ _ _ _ -> pure unit
  , update: \_ _ _ -> pure Nothing
  , fixed:
      { interval: never
      , function: \_ _ _ -> pure Nothing
      }
  , interactions: Interactions.default
  , output: \_ _ _ -> pure Nothing
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
  = Fixed Area
  | Stretch
  | Fullscreen
