module ColorButton where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties as HP

type Slot slot
  = forall q. H.Slot q Output slot

_colorButton = SProxy :: SProxy "colorButton"

type State
  = { color :: String, selected :: String }

type Input
  = State

data Output
  = Clicked String

data Action
  = ClickedAction String
  | Update Input

component :: forall q m. H.Component HH.HTML q Input Output m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction, receive = Just <<< Update }
    }

render :: forall s m. State -> H.ComponentHTML Action s m
render { color, selected } =
  HH.button
    [ onClick \_ -> Just $ ClickedAction color
    , style
        $ ("background-color: " <> color <> ";")
        <> "width: 72px;"
        <> "height: 72px;"
        <> "margin: 6px 12px;"
        <> "cursor: pointer;"
        <> selectedStyle
    ]
    []
  where
  selectedStyle =
    if selected == color then
      "outline: 4px #2196f3 solid;"
    else
      ""

style :: forall r i. String -> HP.IProp r i
style = HP.attr (HH.AttrName "style")

handleAction :: forall s m. Action -> H.HalogenM State Action s Output m Unit
handleAction = case _ of
  ClickedAction state -> H.raise $ Clicked state
  Update state -> H.put state
