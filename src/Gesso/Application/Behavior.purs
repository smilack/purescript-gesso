module Gesso.Application.Behavior
  ( FixedUpdate
  , InputReceiver
  , OutputProducer
  , RenderFunction
  , TimestampedUpdate
  , UpdateFunction
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Gesso.Geometry (Scalers)
import Gesso.State (Compare, States)
import Gesso.Time (Delta, Interval)
import Graphics.Canvas (Context2D)

-- | A function that draws on the component. It knows the following:
-- |
-- | - `Context2D` is the drawing context of the canvas element
-- | - `Delta` is a record containing current and previous timestamps and the
-- |   time elapsed since the previous frame.
-- | - `Scalers` is a record containing scaling information for transforming
-- |   coordinates between the drawing and the canvas.
-- | - `local` is the local state of the application, with `States` containing
-- |   the two most recent states and the time progress between them (on the
-- |   interval `[0, 1]`).
-- |
-- | The render function may run any operations in `Effect`, not just functions
-- | related to drawing on the canvas.
type RenderFunction local =
  Context2D -> Delta -> Scalers -> States local -> Effect Unit

-- | An function that may update the application state. It runs on every frame,
-- | before the render function. It knows the following:
-- |
-- | - `Delta` is a record containing current and previous timestamps and the
-- |   time elapsed since the previous frame.
-- | - `Scalers` is a record containing scaling information for transforming
-- |   coordinates between the drawing and the canvas.
-- | - `local` is the local state of the application
-- |
-- | The update function may return a new local state if changes are necessary
-- | (or `Nothing` if not).
-- |
-- | This type is also used by Interaction handlers and when receiving input
-- | from a host application.
type UpdateFunction local =
  Delta -> TimestampedUpdate local

-- | A partially applied `UpdateFunction` that already has the `Delta` record.
type TimestampedUpdate local =
  Scalers -> local -> Effect (Maybe local)

-- | An update function that occurs at a fixed, regular interval, rather than on
-- | every animation frame.
type FixedUpdate local =
  { interval :: Interval
  , function :: UpdateFunction local
  }

-- | An input receiver is a variant of an update function that can receive
-- | information from the component's parent and produce an update function
-- | in response.
type InputReceiver local input = input -> UpdateFunction local

-- | When the local state of an application changes, an output producer compares
-- | the old and new local states and may send output to the component's parent
-- | based on the difference.
type OutputProducer local output =
  Delta -> Scalers -> Compare local -> Effect (Maybe output)
