{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "typelevel-graph"
, dependencies = [ "psci-support", "typelevel-peano", "typelevel-prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
