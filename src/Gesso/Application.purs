-- | `Application` houses functions and configuration that are shared between
-- | all Gesso applications, regardless of the rendering component.
module Gesso.Application
  ( AppSpec
  , defaultApp
  , WindowMode
  , fixed
  , stretch
  , fullscreen
  , RenderFunction
  , UpdateFunction
  , TimestampedUpdate
  , OutputProducer
  , InputReceiver
  , windowCss
  ) where

import Prelude
import CSS as CSS
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gesso.Dimensions as D
import Gesso.Time as T

-- | An `AppSpec` is a record consisting of the following fields:
-- |
-- | - `window` is a [`WindowMode`](#t:WindowMode) that defines how the screen
-- |   element should size and position itself.
-- | - `render` is a `[RenderFunction](#t:RenderFunction)` which defines what is
-- |   drawn on the component.
-- | - `update` is an `[UpdateFunction](#t:UpdateFunction)` which runs on each
-- |   frame, just before `render`.
-- | - `output` is an [`OutputProducer`](#t:OutputProducer) which defines how
-- |   (or if) the component should send information out to the host
-- |   application.
-- | - `input` is an [`InputReceiver`](#t:InputReceiver) that defines how the
-- |   state should change in response to receiving input from the host
-- |   application.
type AppSpec context local input output =
  { window :: WindowMode
  , render :: RenderFunction context local
  , update :: UpdateFunction local
  , output :: OutputProducer local output
  , input :: InputReceiver local input
  }

-- | A default [`AppSpec`](#t:AppSpec) which can be modified piecemeal like
-- | Halogen's `EvalSpec`. It does nothing on its own.
defaultApp
  :: forall context local input output
   . AppSpec context local input output
defaultApp =
  { window: fixed D.sizeless
  , render: \_ _ _ _ -> pure unit
  , update: \_ _ _ -> pure Nothing
  , output: \_ _ _ _ -> pure Nothing
  , input: \_ _ _ _ -> pure Nothing
  }

-- | There are three modes that determine the size and position of a Gesso
-- | canvas component:
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

-- | An alias for a canvas rendering function.
-- |
-- | - `local` is the local state of the application
-- | - `T.Delta` is a record containing current and previous timestamps and the
-- |   time elapsed since the previous frame.
-- | - `D.Scaler` is a record containg scaling functions for converting drawing
-- |   coordinates to pixel coordinates.
-- | - `context` is the context type for the canvas element, like `Context2D`
-- |
-- | The render function may run any operations in `Effect`, not just functions
-- | related to drawing on the canvas.
type RenderFunction context local =
  local -> T.Delta -> D.Scaler -> context -> Effect Unit

-- | An `UpdateFunction` gets a `Delta` record from `Gesso.Time`, a `Scaler`
-- | from `Gesso.Dimensions`, and the current local state, and may return an
-- | updated local state if changes are necessary (or `Nothing` if there was no
-- | change). This type is also used by Interaction handlers and when receiving
-- | input from a host application.
type UpdateFunction local =
  T.Delta -> TimestampedUpdate local

-- | A partially applied [`UpdateFunction`](#t:UpdateFunction) that already has
-- | a `Delta` record.
type TimestampedUpdate local =
  D.Scaler -> local -> Effect (Maybe local)

-- | An alias for a function that receives input from the host application and
-- | produces an update function in response.
type InputReceiver local input = input -> UpdateFunction local

-- | An alias for a function that compares old and new local states and may
-- | send output based on the difference. The old state comes first and the new
-- | state comes second.
type OutputProducer local output =
  T.Delta -> D.Scaler -> local -> local -> Effect (Maybe output)

-- | Get the appropriate CSS for the screen element based on the `WindowMode`.
windowCss :: WindowMode -> CSS.CSS
windowCss = case _ of
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
