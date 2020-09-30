module ColorButton where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)

type Slot slot
  = forall q. H.Slot q Output slot

_colorButton = SProxy :: SProxy "colorButton"

type State
  = String

type Input
  = String

data Output
  = Clicked State

data Action
  = ClickedAction State

component :: forall q m. H.Component HH.HTML q Input Output m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction }
    }

render :: forall s m. State -> H.ComponentHTML Action s m
render state =
  HH.button
    [ onClick \_ -> Just $ ClickedAction state ]
    [ HH.text state ]

handleAction :: forall s m. Action -> H.HalogenM State Action s Output m Unit
handleAction (ClickedAction state) = H.raise $ Clicked state
