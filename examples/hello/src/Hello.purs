module Gesso.Example.Hello where

import Effect (Effect)
import Gesso (launch)
import Gesso.Application (WindowMode(..), defaultBehavior)
import Gesso.Geometry (Scalers, null)
import Gesso.State (States)
import Gesso.Time (Delta)
import Graphics.Canvas (Context2D, fillText)
import Prelude (Unit, unit)

main :: Effect Unit
main = launch
  { name: "hello"
  , initialState: unit
  , viewBox: null
  , window: Fullscreen
  , behavior: defaultBehavior { render = render }
  }

render :: Context2D -> Delta -> Scalers -> States Unit -> Effect Unit
render context _ _ _ = fillText context "hello world" 500.0 500.0
