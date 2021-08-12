let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/unit-grid/**/*.purs" ],
  dependencies =
    config.dependencies #
    [ "arrays"
    , "math"
    ]
}
