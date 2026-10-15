module Gesso.Application.Behavior
  ( FixedUpdate
  , InitFunction
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

-- | A function that draws on the component. It knows the following:
-- |
-- | - `context` is the drawing context of the canvas element, determined by the
-- |   context selection in the `AppBehavior`
-- | - `Delta` is a record containing current and previous timestamps and the
-- |   time elapsed since the previous frame.
-- | - `Scalers` is a record containing scaling information for transforming
-- |   coordinates between the drawing and the canvas.
-- | - `state` is the state of the application, with `States` containing the two
-- |   most recent states and the time progress between them (on the interval
-- |   `[0, 1]`).
-- |
-- | The render function may run any operations in `Effect`, not just functions
-- | related to drawing on the canvas.
type RenderFunction context state =
  context -> Delta -> Scalers -> States state -> Effect Unit

-- | A function that runs once, after the context is gotten from the canvas, but
-- | before the first rendering frame. It knows the rendering context in
-- | addition to all of the arguments of an `UpdateFunction`.
type InitFunction context state =
  context -> UpdateFunction state

-- | An function that may update the application state. It runs on every frame,
-- | before the render function. It knows the following:
-- |
-- | - `Delta` is a record containing current and previous timestamps and the
-- |   time elapsed since the previous frame.
-- | - `Scalers` is a record containing scaling information for transforming
-- |   coordinates between the drawing and the canvas.
-- | - `state` is the state of the application
-- |
-- | The update function may return a new state if changes are necessary (or
-- | `Nothing` if not).
-- |
-- | This type is also used by Interaction handlers and when receiving input
-- | from a host application.
type UpdateFunction state =
  Delta -> TimestampedUpdate state

-- | A partially applied `UpdateFunction` that already has the `Delta` record.
type TimestampedUpdate state =
  Scalers -> state -> Effect (Maybe state)

-- | An update function that occurs at a fixed, regular interval, rather than on
-- | every animation frame.
type FixedUpdate state =
  { interval :: Interval
  , function :: UpdateFunction state
  }

-- | An input receiver is a variant of an update function that can receive
-- | information from the component's parent and produce an update function
-- | in response.
type InputReceiver state input = input -> UpdateFunction state

-- | When the state of an application changes, an output producer compares the
-- | old and new states and may send output to the component's parent based on
-- | the difference.
type OutputProducer state output =
  Delta -> Scalers -> Compare state -> Effect (Maybe output)
