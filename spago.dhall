{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-typelevel-graph"
, dependencies =
  [ "console", "effect", "psci-support", "record-extra", "typelevel-peano" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
