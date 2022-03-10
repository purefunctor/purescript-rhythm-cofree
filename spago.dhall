let dependencies = [ "control", "effect", "free", "prelude" ]

let test-dependencies = [ "aff", "spec" ]

in  { name = "rhythm-cofree"
    , dependencies = dependencies # test-dependencies
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs", "test/**/*.purs" ]
    }
