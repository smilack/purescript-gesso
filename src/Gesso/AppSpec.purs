module Gesso.AppSpec
  ( AppSpec
  , defaultApp
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
  , update
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gesso.Dimensions as D
import Gesso.Time as T
import Graphics.Canvas as C

type AppSpec state
  = { name :: String
    , window :: WindowStyle
    , viewBox :: D.Dimensions D.ViewBox
    , render :: Maybe (RenderStyle state)
    , update :: Maybe (Update state)
    }

defaultApp :: forall state. AppSpec state
defaultApp =
  { name: "screen"
  , window: Fixed D.sizeless
  , viewBox: D.null
  , render: Nothing
  , update: Nothing
  }

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

update :: forall state. UpdateFunction state -> Update state
update = Update
