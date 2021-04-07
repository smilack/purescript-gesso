{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-bus"
  , "canvas"
  , "console"
  , "css"
  , "debug"
  , "effect"
  , "halogen"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "newtype"
  , "psci-support"
  , "record"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
