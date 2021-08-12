let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/clock/**/*.purs" ],
  dependencies =
    config.dependencies #
    [ "arrays"
    , "datetime"
    , "enums"
    , "math"
    , "now"
    ]
}
