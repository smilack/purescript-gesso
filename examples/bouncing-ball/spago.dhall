let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/bouncing-ball/**/*.purs" ],
  dependencies = config.dependencies # [ "numbers" ]
}
