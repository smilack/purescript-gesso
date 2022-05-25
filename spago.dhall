{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-gesso"
, dependencies =
  [ "aff"
  , "canvas"
  , "console"
  , "css"
  , "dom-indexed"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-subscriptions"
  , "integers"
  , "lists"
  , "maybe"
  , "prelude"
  , "safe-coerce"
  , "tuples"
  , "web-clipboard"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-touchevents"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
