{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "my-project"
, dependencies =
    [ "profunctor-lenses"
    , "effect"
    , "console"
    , "psci-support"
    , "react-basic"
    , "spec"
    ]
, packages =
    ./packages.dhall
}
