{ name = "global-sequence-alignment-ps"
, dependencies =
    [ "arrays"
    , "console"
    , "effect"
    , "either"
    , "exceptions"
    , "foldable-traversable"
    , "maybe"
    , "prelude"
    , "react-basic"
    , "react-basic-dom"
    , "react-basic-hooks"
    , "read"
    , "strings"
    , "tuples"
    , "validation"
    , "web-dom"
    , "web-html"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
