let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/paint/**/*.purs" ],
  dependencies =
    config.dependencies #
    [ "arrays"
    , "record"
    ]
}
