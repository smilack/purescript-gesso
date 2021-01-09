let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/paint-global/**/*.purs" ],
  dependencies = config.dependencies
}
