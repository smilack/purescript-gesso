let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/paint-io/**/*.purs" ],
  dependencies =
    config.dependencies #
    [ "arrays"
    , "record"
    ]
}
