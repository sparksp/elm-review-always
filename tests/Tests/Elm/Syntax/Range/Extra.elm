module Tests.Elm.Syntax.Range.Extra exposing (all)

import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Range.Extra as RangeExtra
import Expect
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Elm.Syntax.Range.Extra"
        [ describe "compare" compareTests
        ]


compareTests : List Test
compareTests =
    [ describe "EQ"
        [ test "when ranges are equal" <|
            \() ->
                let
                    range : Range
                    range =
                        { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }
                in
                RangeExtra.compare range range
                    |> Expect.equal EQ
        ]
    , describe "LT"
        [ test "when left start row < right start row" <|
            \() ->
                let
                    left : Range
                    left =
                        { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }

                    right : Range
                    right =
                        { start = { row = 2, column = 1 }, end = { row = 2, column = 2 } }
                in
                RangeExtra.compare left right
                    |> Expect.equal LT
        , test "when both start on the same row, with left column < right column" <|
            \() ->
                let
                    left : Range
                    left =
                        { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }

                    right : Range
                    right =
                        { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } }
                in
                RangeExtra.compare left right
                    |> Expect.equal LT
        , test "when both start the same, with left end row < right end row" <|
            \() ->
                let
                    left : Range
                    left =
                        { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }

                    right : Range
                    right =
                        { start = { row = 1, column = 1 }, end = { row = 2, column = 3 } }
                in
                RangeExtra.compare left right
                    |> Expect.equal LT
        , test "when both start the same, end on the same row, with left column < right column" <|
            \() ->
                let
                    left : Range
                    left =
                        { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }

                    right : Range
                    right =
                        { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }
                in
                RangeExtra.compare left right
                    |> Expect.equal LT
        ]
    , describe "GT"
        [ test "when left start row > right start row" <|
            \() ->
                let
                    left : Range
                    left =
                        { start = { row = 2, column = 1 }, end = { row = 2, column = 2 } }

                    right : Range
                    right =
                        { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }
                in
                RangeExtra.compare left right
                    |> Expect.equal GT
        , test "when both start on the same row, with left column > right column" <|
            \() ->
                let
                    left : Range
                    left =
                        { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } }

                    right : Range
                    right =
                        { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                in
                RangeExtra.compare left right
                    |> Expect.equal GT
        , test "when both start the same, with left end row > right end row" <|
            \() ->
                let
                    left : Range
                    left =
                        { start = { row = 1, column = 1 }, end = { row = 3, column = 2 } }

                    right : Range
                    right =
                        { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }
                in
                RangeExtra.compare left right
                    |> Expect.equal GT
        , test "when both start the same, end on the same row, with left column > right column" <|
            \() ->
                let
                    left : Range
                    left =
                        { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }

                    right : Range
                    right =
                        { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }
                in
                RangeExtra.compare left right
                    |> Expect.equal GT
        ]
    ]
