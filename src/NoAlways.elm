module NoAlways exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Writer
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Error, Rule)


{-| Forbid the use of [`always`](https://package.elm-lang.org/packages/elm/core/latest/Basics#always).

    config : List Rule
    config =
        [ NoAlways.rule
        ]

Use an [anonymous function] `\_ ->` instead of `always`.

It's more concise, more recognizable as a function, and makes it easier to change your mind later and name the argument.

    -- Don't do this --
    List.map (always 0) [ 1, 2, 3, 4 ]

    -- Instead do this --
    List.map (\_ -> 0) [ 1, 2, 3, 4 ]

[anonymous function]: https://elm-lang.org/docs/syntax#functions


## When (not) to use this rule

If you are in a team then other members may have strong opinions about `always` - make sure that everyone is on board before you decide to adopt this rule.


## Caution: Heavy Computation

If the value you always want is the result of some heavy computation then you will not want that within an anonymous function as the work will be done every time. Instead, do the calculation in a nearby [`let..in`][let-expression] block first.

    -- Don't do this --
    List.map (always (heavyComputation arg1 arg2)) [ 1, 2, 3, 4 ]

    -- Don't do this either --
    List.map (\_ -> heavyComputation arg1 arg2) [ 1, 2, 3, 4 ]

    -- Instead do this --
    let
        heavyComputationResult =
            heavyComputation arg1 arg2
    in
    List.map (\_ -> heavyComputationResult) [ 1, 2, 3, 4 ]

    -- This works too (but is less readable) --
    List.map ((\value _ -> value) (heavyComputation arg1 arg2)) [ 1, 2, 3, 4 ]

[let-expression]: https://elm-lang.org/docs/syntax#let-expressions

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoAlways" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor (Node range node) =
    case node of
        Expression.Application [ Node alwaysRange (Expression.FunctionOrValue [] "always"), expression ] ->
            [ error alwaysRange [ fixAlways range expression ] ]

        Expression.Application [ Node alwaysRange (Expression.FunctionOrValue [ "Basics" ] "always"), expression ] ->
            [ error alwaysRange [ fixAlways range expression ] ]

        _ ->
            []


fixAlways : Range -> Node Expression -> Fix
fixAlways range expression =
    expression
        |> applyLambda
        |> applyBrackets
        |> expressionToString
        |> Fix.replaceRangeBy range


error : Range -> List Fix -> Error {}
error range fix =
    Rule.errorWithFix
        { message = "`always` is not allowed."
        , details =
            [ "You should replace this `always` with an anonymous function `\\_ ->`."
            , "It's more concise, more recognizable as a function, and makes it easier to change your mind later and name the argument."
            ]
        }
        range
        fix


applyLambda : Node Expression -> Node Expression
applyLambda expression =
    Expression.LambdaExpression
        { args = [ Node.Node Range.emptyRange Pattern.AllPattern ]
        , expression = expression
        }
        |> Node.Node Range.emptyRange


applyBrackets : Node Expression -> Node Expression
applyBrackets expression =
    expression
        |> Expression.ParenthesizedExpression
        |> Node.Node Range.emptyRange


expressionToString : Node Expression -> String
expressionToString expression =
    expression
        |> Elm.Writer.writeExpression
        |> Elm.Writer.write
