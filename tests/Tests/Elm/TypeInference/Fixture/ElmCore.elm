module Tests.Elm.TypeInference.Fixture.ElmCore exposing (core)

{-| Believable `elm/core` docs.json contents.

Only functions, values and types we exercise in the tests, there could be more.

Maybe it would be worth it to put a real docs.json here and parse it from JSON...

-}

import Elm.Docs
import Elm.Type exposing (Type(..))
import Elm.TypeInference.Dependencies exposing (DependencyPackage)


core : DependencyPackage
core =
    { name = "elm/core"
    , dependencies = []
    , modules =
        [ basics
        , maybe
        , list
        , platformCmd
        , char
        , dict
        ]
    }


basics : Elm.Docs.Module
basics =
    { name = "Basics"
    , comment = ""
    , unions =
        [ { name = "Bool"
          , comment = ""
          , args = []
          , tags = [ ( "True", [] ), ( "False", [] ) ]
          }
        , { name = "Order"
          , comment = ""
          , args = []
          , tags = [ ( "LT", [] ), ( "EQ", [] ), ( "GT", [] ) ]
          }
        , { name = "Int"
          , comment = ""
          , args = []
          , tags = [] -- opaque, matching real elm/core docs.json
          }
        ]
    , aliases = []
    , values =
        [ { name = "identity", comment = "", tipe = Lambda (Var "a") (Var "a") }
        , { name = "e", comment = "", tipe = Type "Basics.Float" [] }
        ]
    , binops =
        [ { name = "+"
          , comment = ""
          , tipe = Lambda (Var "number") (Lambda (Var "number") (Var "number"))
          , associativity = Elm.Docs.Left
          , precedence = 6
          }
        , { name = "-"
          , comment = ""
          , tipe = Lambda (Var "number") (Lambda (Var "number") (Var "number"))
          , associativity = Elm.Docs.Left
          , precedence = 6
          }
        , { name = "=="
          , comment = ""
          , tipe = Lambda (Var "a") (Lambda (Var "a") (Type "Basics.Bool" []))
          , associativity = Elm.Docs.None
          , precedence = 4
          }
        ]
    }


maybe : Elm.Docs.Module
maybe =
    { name = "Maybe"
    , comment = ""
    , unions =
        [ { name = "Maybe"
          , comment = ""
          , args = [ "a" ]
          , tags = [ ( "Just", [ Var "a" ] ), ( "Nothing", [] ) ]
          }
        ]
    , aliases = []
    , values =
        [ { name = "map"
          , comment = ""
          , tipe =
                Lambda
                    (Lambda (Var "a") (Var "b"))
                    (Lambda (Type "Maybe.Maybe" [ Var "a" ]) (Type "Maybe.Maybe" [ Var "b" ]))
          }
        ]
    , binops = []
    }


list : Elm.Docs.Module
list =
    { name = "List"
    , comment = ""
    , unions = [] -- No `List` union here! elm/core doesn't have one, it's compiler builtin
    , aliases = []
    , values =
        [ { name = "map"
          , comment = ""
          , tipe =
                Lambda
                    (Lambda (Var "a") (Var "b"))
                    (Lambda (Type "List.List" [ Var "a" ]) (Type "List.List" [ Var "b" ]))
          }
        ]
    , binops =
        [ { name = "::"
          , comment = ""
          , tipe =
                Lambda
                    (Var "a")
                    (Lambda (Type "List.List" [ Var "a" ]) (Type "List.List" [ Var "a" ]))
          , associativity = Elm.Docs.Right
          , precedence = 5
          }
        ]
    }


platformCmd : Elm.Docs.Module
platformCmd =
    { name = "Platform.Cmd"
    , comment = ""
    , unions =
        [ { name = "Cmd"
          , comment = ""
          , args = [ "msg" ]
          , tags = []
          }
        ]
    , aliases = []
    , values =
        [ { name = "none"
          , comment = ""
          , tipe = Type "Platform.Cmd.Cmd" [ Var "msg" ]
          }
        ]
    , binops = []
    }


char : Elm.Docs.Module
char =
    { name = "Char"
    , comment = ""
    , unions =
        [ { name = "Char"
          , comment = ""
          , args = []
          , tags = [] -- opaque, matching real elm/core docs.json
          }
        ]
    , aliases = []
    , values = []
    , binops = []
    }


dict : Elm.Docs.Module
dict =
    { name = "Dict"
    , comment = ""
    , unions =
        [ { name = "Dict"
          , comment = ""
          , args = [ "k", "v" ]
          , tags = [] -- opaque, matching real elm/core docs.json
          }
        ]
    , aliases = []
    , values =
        [ { name = "map"
          , comment = ""
          , tipe =
                Lambda
                    (Lambda
                        (Var "k")
                        (Lambda
                            (Var "v1")
                            (Var "v2")
                        )
                    )
                    (Lambda
                        (Type "Dict" [ Var "k", Var "v1" ])
                        (Type "Dict" [ Var "k", Var "v2" ])
                    )
          }
        ]
    , binops = []
    }
