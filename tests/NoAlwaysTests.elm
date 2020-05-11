module NoAlwaysTests exposing (all)

import NoAlways exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoAlways"
        [ test "reports uses of always" <|
            \_ ->
                """
module Main exposing (main)
main = foo
foo = always "foo"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error "always"
                        ]
        ]


error : String -> Review.Test.ExpectedError
error under =
    Review.Test.error
        { message = "`always` is not allowed."
        , details =
            [ "You should replace this `always` with an anonymous function `\\_ ->`."
            , "It's more concise, more recognizable as a function, and makes it easier to change your mind later and name the argument."
            ]
        , under = under
        }
