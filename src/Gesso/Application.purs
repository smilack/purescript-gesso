-- | Functions and configuration necessary to start a Gesso application.
module Gesso.Application
  ( AppBehavior
  , AppSpec
  , defaultBehavior
  , nominalBehavior
  , module Exports
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gesso.Application.Behavior (FixedUpdate, InitFunction, InputReceiver, OutputProducer, RenderFunction, TimestampedUpdate, UpdateFunction) as Exports
import Gesso.Application.Behavior (FixedUpdate, InitFunction, InputReceiver, OutputProducer, RenderFunction, UpdateFunction)
import Gesso.Canvas.Element (WindowMode)
import Gesso.Canvas.Element (WindowMode(..)) as Exports
import Gesso.Canvas.Context (getDefault2D)
import Gesso.Geometry (Rect)
import Gesso.Interactions (Interactions)
import Gesso.Interactions (default) as Interactions
import Gesso.Time (never)
import Graphics.Canvas (Context2D)

-- | `AppSpec` holds information about the setup and behavior of a Gesso
-- | component.
-- |
-- | - `name` is the name of the application, which doubles as the HTML `id` for
-- |   the canvas element.
-- | - `initialState` is the initial state for the application.
-- | - `viewBox` is the desired dimensions for the drawing area.
-- | - `window` defines how the screen element should size and position itself.
-- | - `behavior` contains functions that control i/o, updates, and rendering.
type AppSpec state input output context =
  { name :: String
  , initialState :: state
  , viewBox :: Rect
  , window :: WindowMode
  , behavior :: AppBehavior state input output context
  }

-- | `AppBehavior` holds the functions that make an application run.
-- |
-- | - `render` draws on the component every animation frame.
-- | - `init` runs once, after the context is created but before the first
-- |   animation frame.
-- | - `update` runs on each animation frame, just before `render`.
-- | - `fixed` runs at a set interval of time.
-- | - `interactions` are events which will be attached to the canvas element.
-- | - `output` defines how (or if) the component should send information out to
-- |   a parent component.
-- | - `input` defines how the component's state should change in response to
-- |   receiving input from a parent component.
-- | - `getContext` gets the rendering context from the canvas element.
type AppBehavior state input output context =
  { render :: RenderFunction context state
  , init :: InitFunction context state
  , update :: UpdateFunction state
  , fixed :: FixedUpdate state
  , interactions :: Interactions state
  , output :: OutputProducer state output
  , input :: InputReceiver state input
  , getContext :: String -> Effect (Maybe context)
  }

-- | A default `AppBehavior` which can be modified piecemeal like Halogen's
-- | `EvalSpec`. It gets a `Context2D` with default options, but otherwise does
-- | nothing unless modified. If you need to change the `getContext` function
-- | and don't need a `render` and/or `init` function, it's slightly cleaner to
-- | use `nominalBehavior` instead.
defaultBehavior
  :: forall state input output
   . AppBehavior state input output Context2D
defaultBehavior =
  { render: \_ _ _ _ -> pure unit
  , init: \_ _ _ _ -> pure Nothing
  , update: \_ _ _ -> pure Nothing
  , fixed:
      { interval: never
      , function: \_ _ _ -> pure Nothing
      }
  , interactions: Interactions.default
  , output: \_ _ _ -> pure Nothing
  , input: \_ _ _ _ -> pure Nothing
  , getContext: getDefault2D
  }

-- | A totally blank `AppBehavior` which can be modified piecemeal like
-- | Halogen's `EvalSpec`. More general than `defaultBehavior` because that
-- | provides a working value for `getContext`, which this does not. Using
-- | `defaultBehavior` with a context other than `Context2D` requires updating
-- | both `init` and `render` in order to unify the `context` type variable.
nominalBehavior
  :: forall state input output context
   . AppBehavior state input output context
nominalBehavior =
  { render: \_ _ _ _ -> pure unit
  , init: \_ _ _ _ -> pure Nothing
  , update: \_ _ _ -> pure Nothing
  , fixed:
      { interval: never
      , function: \_ _ _ -> pure Nothing
      }
  , interactions: Interactions.default
  , output: \_ _ _ -> pure Nothing
  , input: \_ _ _ _ -> pure Nothing
  , getContext: \_ -> pure Nothing
  }
