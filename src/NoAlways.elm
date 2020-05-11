module NoAlways exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)


{-| Use anonymous function `\_ ->` over `always`.

    config : List Rule
    config =
        [ NoAlways.rule
        ]

It's more concise, more recognizable as a function, and makes it easier to change your mind later and name the argument.


## Failure

    -- Don't do this --
    Node.map (always "string") node


## Success

    -- Instead do this --
    Node.map (\_ -> "string") node

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoAlways" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Expression.FunctionOrValue [] "always" ->
            [ error (Node.range node) ]

        _ ->
            []


error : Range -> Error {}
error range =
    Rule.error
        { message = "`always` is not allowed."
        , details =
            [ "You should replace this `always` with an anonymous function `\\_ ->`."
            , "It's more concise, more recognizable as a function, and makes it easier to change your mind later and name the argument."
            ]
        }
        range
