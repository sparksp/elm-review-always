module Tests.NoAlways exposing (all)

import NoAlways exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoAlways"
        [ describe "locations" testLocations
        , describe "types" testTypes
        ]


testLocations : List Test
testLocations =
    [ test "always in brackets" <|
        \_ ->
            """
module Main exposing (main)
import Foo exposing (foo)
main = foo (always "foo")
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysError
                        |> Review.Test.whenFixed
                            """
module Main exposing (main)
import Foo exposing (foo)
main = foo ((\\_ -> "foo"))
"""
                    ]
    , test "always in a list" <|
        \_ ->
            """
module Foo exposing (foo)
foo =
    [ "foo", always "bar" ]
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysError
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo =
    [ "foo", (\\_ -> "bar") ]
"""
                    ]
    , test "always in a map" <|
        \_ ->
            """
module Foo exposing (foo)
foo =
    List.map (always 0) [ 1, 2, 3, 4 ]
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysError
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo =
    List.map ((\\_ -> 0)) [ 1, 2, 3, 4 ]
"""
                    ]
    , test "always in a record" <|
        \_ ->
            """
module Foo exposing (foo)
foo =
    { foo = "foo"
    , bar = always "bar"
    }
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysError
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo =
    { foo = "foo"
    , bar = (\\_ -> "bar")
    }
"""
                    ]
    , test "always in a tuple" <|
        \_ ->
            """
module Foo exposing (foo)
foo =
    ( "foo", always "bar" )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysError
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo =
    ( "foo", (\\_ -> "bar") )
"""
                    ]
    , test "always pipe right" <|
        \_ ->
            """
module Foo exposing (foo)
foo = "foo" |> always
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "always pipe left" <|
        \_ ->
            """
module Foo exposing (foo)
foo = always <| heavyComputation "foo"
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "Basics.always" <|
        \() ->
            """
module A exposing (..)
foo = Basics.always 1
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysErrorUnder "Basics.always"
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo = (\\_ -> 1)
"""
                    ]
    ]


testTypes : List Test
testTypes =
    [ test "always always" <|
        \_ ->
            """
module Foo exposing (foo)
foo = always (always True)
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysError
                        |> Review.Test.atExactly { start = { row = 3, column = 7 }, end = { row = 3, column = 13 } }
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo = (\\_ -> (always True))
"""
                    , alwaysError
                        |> Review.Test.atExactly { start = { row = 3, column = 15 }, end = { row = 3, column = 21 } }
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo = always ((\\_ -> True))
"""
                    ]
    , test "always Bool" <|
        \_ ->
            """
module Foo exposing (foo)
foo = always True
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysError
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo = (\\_ -> True)
"""
                    ]
    , test "always Float" <|
        \_ ->
            """
module Foo exposing (foo)
foo = always 4.2
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysError
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo = (\\_ -> 4.2)
"""
                    ]
    , test "always Int" <|
        \_ ->
            """
module Foo exposing (foo)
foo = always 42
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysError
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo = (\\_ -> 42)
"""
                    ]
    , test "always List" <|
        \_ ->
            """
module Foo exposing (foo)
foo = always [1, 2, 3]
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysError
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo = (\\_ -> [1, 2, 3])
"""
                    ]
    , test "always Just" <|
        \_ ->
            """
module Foo exposing (foo)
foo = always (Just 42)
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysError
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo = (\\_ -> (Just 42))
"""
                    ]
    , test "always String" <|
        \_ ->
            """
module Foo exposing (foo)
foo = always "foo"
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysError
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo = (\\_ -> "foo")
"""
                    ]
    , test "always Tuple" <|
        \_ ->
            """
module Foo exposing (foo)
foo = always ( "foo", "bar" )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysError
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo = (\\_ -> ("foo", "bar"))
"""
                    ]
    , test "always Unit" <|
        \_ ->
            """
module Foo exposing (foo)
foo = always ()
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysError
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo = (\\_ -> ())
"""
                    ]
    ]


alwaysError : Review.Test.ExpectedError
alwaysError =
    alwaysErrorUnder "always"


alwaysErrorUnder : String -> Review.Test.ExpectedError
alwaysErrorUnder under =
    Review.Test.error
        { message = "`always` is not allowed."
        , details =
            [ "You should replace this `always` with an anonymous function `\\_ ->`."
            , "It's more concise, more recognizable as a function, and makes it easier to change your mind later and name the argument."
            ]
        , under = under
        }
