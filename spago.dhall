{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-bus"
  , "canvas"
  , "console"
  , "css"
  , "debug"
  , "dom-indexed"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-subscriptions"
  , "integers"
  , "lists"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "record"
  , "refs"
  , "safe-coerce"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "type-equality"
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
