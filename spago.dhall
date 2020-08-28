{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "argonaut"
  , "argonaut-core"
  , "arrays"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "generics-rep"
  , "psci-support"
  , "string-parsers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
