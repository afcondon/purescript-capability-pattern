{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "assert"
  , "checked-exceptions"
  , "console"
  , "effect"
  , "node-fs"
  , "node-fs-aff"
  , "node-readline"
  , "psci-support"
  , "transformers"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
