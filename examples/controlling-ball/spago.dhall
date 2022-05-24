let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/controlling-ball/**/*.purs" ]
}
