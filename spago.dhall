let dev = env:TICKLEPALOOZA_DEV ? False
in
  { name = "ticklepalooza"
  , dependencies =
    [ "aff-promise"
    , "aff"
    , "effect"
    , "exceptions"
    , "halogen"
    , "foldable-traversable"
    , "maybe"
    , "prelude"
    , "transformers"
    , "tuples"
    , "web-dom"
    , "web-html"
    ]
  , packages = ./packages.dhall
  , sources =
    [ "src/**/*.purs"
    , "${if   dev
        then  "env/Dev.purs"
        else  "env/Prod.purs"}"
    , "test/**/*.purs"
    ]
  }
