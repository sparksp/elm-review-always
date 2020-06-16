module Tests.NoAlways exposing (all)

import NoAlways exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoAlways"
        [ describe "locations" locationTests
        , describe "types" typeTests
        , describe "functions" functionTests
        ]


locationTests : List Test
locationTests =
    [ test "always in brackets" <|
        \() ->
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
        \() ->
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
        \() ->
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
        \() ->
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
        \() ->
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
        \() ->
            """
module Foo exposing (foo)
foo = "foo" |> always
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "always pipe left" <|
        \() ->
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


typeTests : List Test
typeTests =
    [ test "always always" <|
        \() ->
            """
module Foo exposing (foo)
foo = always (always True)
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysErrorWithWarning
                        |> Review.Test.atExactly { start = { row = 3, column = 7 }, end = { row = 3, column = 13 } }
                    , alwaysError
                        |> Review.Test.atExactly { start = { row = 3, column = 15 }, end = { row = 3, column = 21 } }
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo = always ((\\_ -> True))
"""
                    ]
    , test "always Bool" <|
        \() ->
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
        \() ->
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
        \() ->
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
    , test "always Hex" <|
        \() ->
            """
module Foo exposing (foo)
foo = always 0x42
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysError
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo = (\\_ -> 0x42)
"""
                    ]
    , test "always negative" <|
        \() ->
            """
module Foo exposing (foo)
foo = always -42
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysError
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo = (\\_ -> -42)
"""
                    ]
    , test "always List" <|
        \() ->
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
        \() ->
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
        \() ->
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
        \() ->
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
        \() ->
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
    , test "always Operator" <|
        \() ->
            """
module Foo exposing (foo)
foo = always (1 + 2)
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysError
                        |> Review.Test.whenFixed
                            """
module Foo exposing (foo)
foo = (\\_ -> (1 + 2))
"""
                    ]
    ]


functionTests : List Test
functionTests =
    [ test "always function includes extra warning" <|
        \() ->
            """
module Foo exposing (foo)
foo = always heavyComputation
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysErrorWithWarning
                    ]
    , test "always qualified function includes extra warning" <|
        \() ->
            """
module Foo exposing (foo)
import Bar
foo = always Bar.heavyComputation
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysErrorWithWarning
                    ]
    , test "always typed function includes extra warning" <|
        \() ->
            """
module Foo exposing (foo)
foo = always (Just heavyComputation)
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysErrorWithWarning
                    ]
    , test "always function in a list includes extra warning" <|
        \() ->
            """
module Foo exposing (foo)
foo = always [ heavyComputation ]
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysErrorWithWarning
                    ]
    , test "always function in a tuple includes extra warning" <|
        \() ->
            """
module Foo exposing (foo)
foo = always ( heavyComputation, 1.0 )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysErrorWithWarning
                    ]
    , test "always negative function includes extra warning" <|
        \() ->
            """
module Foo exposing (foo)
foo = always -bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ alwaysErrorWithWarning
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


alwaysErrorWithWarning : Review.Test.ExpectedError
alwaysErrorWithWarning =
    Review.Test.error
        { message = "`always` is not allowed."
        , details =
            [ "You should replace this `always` with an anonymous function `\\_ ->`."
            , "It's more concise, more recognizable as a function, and makes it easier to change your mind later and name the argument."
            , "Caution: If this always does some heavy computation then you may not want that within an anonymous function as the work will be done every time. Instead, do the calculation in a nearby let..in block first."
            ]
        , under = "always"
        }
