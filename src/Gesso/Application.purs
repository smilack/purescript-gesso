-- | `Application` houses functions and configuration that are shared between
-- | all Gesso applications, regardless of the rendering component.
module Gesso.Application
  ( AppSpec
  , WindowMode(..)
  , defaultApp
  , module Exports
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gesso.Geometry (Area, Rect, sizeless)
import Gesso.Application.Behavior (FixedUpdate, InputReceiver, OutputProducer, RenderFunction, UpdateFunction)
import Gesso.Application.Behavior (FixedUpdate, InputReceiver, OutputProducer, RenderFunction, TimestampedUpdate, UpdateFunction) as Exports
import Gesso.Interactions (Interactions)
import Gesso.Time (never)

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
type AppSpec local input output =
  { window :: WindowMode
  , render :: RenderFunction local
  , fixed :: FixedUpdate local
  , update :: UpdateFunction local
  , output :: OutputProducer local output
  , input :: InputReceiver local input
  }

-- | A default `AppSpec` which can be modified piecemeal like Halogen's
-- | `EvalSpec`. It does nothing on its own.
defaultApp
  :: forall local input output
   . AppSpec local input output
defaultApp =
  { window: Fixed sizeless
  , render: \_ _ _ _ -> pure unit
  , fixed:
      { interval: never
      , function: \_ _ _ -> pure Nothing
      }
  , update: \_ _ _ -> pure Nothing
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
