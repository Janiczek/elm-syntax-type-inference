module RigidTypeVarTests exposing (suite)

{-| Type variables in annotations are rigid: the annotation promises the
function works for any `a`, so the body can't make `a` more specific.
-}

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.TypeInference.InferError exposing (InferErrorDetails(..))
import Elm.TypeInference.Type as Type exposing (Type)
import Expect
import String.ExtraExtra
import Test exposing (Test)
import Tests.Elm.TypeInference.Fixture.ElmCore as CoreFixture
import Tests.Elm.TypeInference.Helpers exposing (TestError(..), getDeclTypeWithDeps)


suite : Test
suite =
    Test.describe "Rigid type variables"
        [ Test.describe "annotations more general than their body are rejected" (List.map rejected rejectedCases)
        , Test.describe "annotations the body satisfies are accepted" (List.map accepted acceptedCases)
        , Test.describe "unsound annotations don't leak to callers" (List.map rejected cascadeCases)
        , Test.describe "scoped type variables (rejected)" (List.map rejected scopedRejectedCases)
        , Test.describe "scoped type variables (accepted)" (List.map accepted scopedAcceptedCases)
        ]


{-| `( declaration to look at, source )`
-}
rejectedCases : List ( String, String )
rejectedCases =
    [ ( "addPair", """
        addPair : ( a, a ) -> a
        addPair ( x, y ) = x + y
        """ )
    , ( "f", """
        f : a -> b
        f x = x
        """ )
    , ( "f", """
        f : a -> a
        f x = x + 1
        """ )
    , ( "f", """
        f : a -> a
        f x = x ++ x
        """ )
    , ( "f", """
        f : a -> a
        f x = 1
        """ )
    , ( "f", """
        f : a -> Int
        f x = x
        """ )
    , ( "f", """
        f : a -> a -> Bool
        f x y = x < y
        """ )
    , ( "f", """
        f : a -> b -> ( a, b )
        f x y = ( y, x )
        """ )
    , ( "comparePair", """
        comparePair : comparable -> comparable -> comparable1 -> comparable2 -> Bool
        comparePair a b x y =
          a < b || x < y
        """ )
    , ( "getY", """
        getY : { r | x : Int } -> Int
        getY rec = rec.y
        """ )
    , ( "addY", """
        addY : { r | x : Int } -> { r | x : Int }
        addY rec = { rec | y = 1 }
        """ )
    , ( "view", """
        type Html msg = Html String

        view : model -> Html msg
        view m = Html m
        """ )
    , ( "unbox", """
        type Box a = Box a

        unbox : Box a -> b
        unbox (Box x) = x
        """ )
    , ( "x", """
        x : number
        x = 1.0
        """ )
    , ( "f", """
        f : number -> String
        f x = String.fromInt x
        """ )
    , ( "f", """
        f : Int -> number
        f x = toFloat x
        """ )
    , ( "f", """
        f : comparable -> comparable
        f x = x ++ x
        """ )
    , ( "f", """
        f : comparable -> comparable
        f x = x + 1
        """ )
    , ( "f", """
        f : appendable -> appendable -> Bool
        f x y = x < y
        """ )
    , ( "f", """
        f : a -> List a
        f x = [ x, 1 ]
        """ )
    , ( "f", """
        f : Maybe a -> a
        f m =
          case m of
            Just x -> x
            Nothing -> ""
        """ )
    , ( "f", """
        f : a -> ( a, a )
        f x =
          let
            dup y = ( y, y + 1 )
          in
          dup x
        """ )
    ]


cascadeCases : List ( String, String )
cascadeCases =
    [ ( "n", """
        coerce : a -> b
        coerce x = x

        n : Int
        n = coerce "str"
        """ )
    , ( "v", """
        getY : { r | x : Int } -> Int
        getY rec = rec.y

        v = getY { x = 1 }
        """ )
    ]


acceptedCases : List ( String, String, String )
acceptedCases =
    [ ( "f", """
        f : a -> a
        f x = x
        """, "a -> a" )
    , ( "f", """
        f : a -> b -> ( b, a )
        f x y = ( y, x )
        """, "a -> b -> ( b, a )" )
    , ( "f", """
        f : comparable -> comparable -> Bool
        f a b = a < b
        """, "comparable -> comparable -> Bool" )
    , ( "f", """
        f : appendable -> appendable
        f x = x ++ x
        """, "appendable -> appendable" )
    , ( "f", """
        f : compappend -> compappend -> compappend
        f x y = if x < y then x ++ y else y
        """, "compappend -> compappend -> compappend" )
    , -- a rigid `number` is comparable
      ( "f", """
        f : number -> number -> Bool
        f a b = a < b
        """, "number -> number -> Bool" )
    , ( "f", """
        f : number -> number
        f x = x * 2 + 1
        """, "number -> number" )
    , ( "x", """
        x : Float
        x = 1
        """, "Float" )
    , ( "getX", """
        getX : { r | x : Int } -> Int
        getX rec = rec.x
        """, "{ r | x : Int } -> Int" )
    , ( "setX", """
        setX : { r | x : Int } -> { r | x : Int }
        setX rec = { rec | x = 0 }
        """, "{ r | x : Int } -> { r | x : Int }" )
    , ( "greet", """
        type alias Named r = { r | name : String }

        greet : Named r -> String
        greet r = r.name
        """, "Main.Named r -> String" )
    , ( "unbox", """
        type Box a = Box a

        unbox : Box a -> a
        unbox (Box x) = x
        """, "Main.Box a -> a" )
    , ( "size", """
        type Tree a = Leaf | Node (Tree a) a (Tree a)

        size : Tree a -> Int
        size t =
          case t of
            Leaf -> 0
            Node l _ r -> size l + 1 + size r
        """, "Main.Tree a -> Int" )
    , ( "g", """
        f : a -> a
        f x = x

        g = ( f 1, f "a" )
        """, "( number, String )" )
    , ( "h", """
        f : a -> a
        f x = x

        g : a -> Int
        g _ = 1

        h = ( f 1, f "a", g True )
        """, "( number, String, Int )" )
    , ( "f", """
        f : a -> ( a, a )
        f x =
          let
            dup y = ( y, y )
          in
          dup x
        """, "a -> ( a, a )" )
    , ( "g", """
        f : a -> a
        f x = g x

        g y = f y
        """, "a -> a" )
    , ( "f", """
        f : (a -> b) -> List a -> List b
        f = List.map
        """, "(a -> b) -> List a -> List b" )
    ]


scopedRejectedCases : List ( String, String )
scopedRejectedCases =
    [ ( "f", """
        f : a -> List a
        f x =
          let
            g : b -> b
            g y = x
          in
          [ g x ]
        """ )
    , ( "f", """
        f : a -> a
        f x =
          let
            g : a -> Int
            g y = 1
          in
          if g "str" > 0 then x else x
        """ )
    , ( "f", """
        f : a -> Int
        f x =
          let
            g : a -> a
            g y = y
          in
          g 1
        """ )
    , ( "f", """
        f x =
          let
            g : a -> a
            g y = y + 1
          in
          g x
        """ )
    ]


scopedAcceptedCases : List ( String, String, String )
scopedAcceptedCases =
    [ ( "f", """
        f : a -> List a
        f x =
          let
            g : a -> a
            g y = y
          in
          [ g x ]
        """, "a -> List a" )
    , ( "f", """
        f : a -> b -> ( b, a )
        f x y =
          let
            swap : ( a, b ) -> ( b, a )
            swap ( p, q ) = ( q, p )
          in
          swap ( x, y )
        """, "a -> b -> ( b, a )" )
    , ( "f", """
        f : a -> a -> a
        f x y =
          let
            pick : a -> a -> a
            pick p q = p
          in
          pick x y
        """, "a -> a -> a" )
    , ( "f", """
        f =
          let
            g : b -> b
            g y = y
          in
          ( g 1, g "a" )
        """, "( number, String )" )
    , ( "f", """
        f : a -> Maybe a
        f x =
          let
            wrap : a -> Maybe a
            wrap y = Just y
          in
          wrap x
        """, "a -> Maybe.Maybe a" )
    ]



-- HELPERS


module_ : String -> Dict ModuleName String
module_ body =
    Dict.singleton [ "Main" ]
        ("module Main exposing (..)\n\n" ++ String.ExtraExtra.multilineInput body)


infer : String -> String -> Result TestError Type
infer declName body =
    getDeclTypeWithDeps [ CoreFixture.core ] (module_ body) [ "Main" ] declName


rejected : ( String, String ) -> Test
rejected ( declName, body ) =
    Test.test (String.trim body) <|
        \() ->
            case infer declName body of
                Err (CouldntInfer { details }) ->
                    case details of
                        TypeMismatch _ _ ->
                            Expect.pass

                        ConstraintMismatch _ _ ->
                            Expect.pass

                        _ ->
                            Expect.fail ("Failed for the wrong reason: " ++ Debug.toString details)

                Err other ->
                    Expect.fail ("Failed for the wrong reason: " ++ Debug.toString other)

                Ok type_ ->
                    Expect.fail ("Should have been rejected, but inferred: " ++ Type.toString type_)


accepted : ( String, String, String ) -> Test
accepted ( declName, body, expected ) =
    Test.test (String.trim body) <|
        \() ->
            infer declName body
                |> Result.map Type.toString
                |> Expect.equal (Ok expected)
