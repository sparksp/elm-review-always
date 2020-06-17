module NoAlways exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
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
            [ alwaysExpressionError { always = alwaysRange, application = range } expression
            ]

        Expression.Application [ Node alwaysRange (Expression.FunctionOrValue [ "Basics" ] "always"), expression ] ->
            [ alwaysExpressionError { always = alwaysRange, application = range } expression
            ]

        _ ->
            []


alwaysExpressionError : { always : Range, application : Range } -> Node Expression -> Error {}
alwaysExpressionError ranges expression =
    if containsFunctionOrValue expression then
        errorWithWarning ranges.always

    else
        errorWithFix ranges.always (fixAlways ranges expression)


containsFunctionOrValue : Node Expression -> Bool
containsFunctionOrValue (Node _ expression) =
    case expression of
        Expression.FunctionOrValue _ name ->
            case String.uncons name of
                Just ( start, _ ) ->
                    Char.isLower start

                Nothing ->
                    False

        Expression.IfBlock _ _ _ ->
            True

        Expression.CaseExpression _ ->
            True

        Expression.LetExpression _ ->
            True

        Expression.RecordUpdateExpression _ _ ->
            True

        Expression.GLSLExpression _ ->
            True

        Expression.ParenthesizedExpression next ->
            containsFunctionOrValue next

        Expression.Application list ->
            List.any containsFunctionOrValue list

        Expression.TupledExpression list ->
            List.any containsFunctionOrValue list

        Expression.ListExpr list ->
            List.any containsFunctionOrValue list

        Expression.OperatorApplication _ _ left right ->
            List.any containsFunctionOrValue [ left, right ]

        Expression.RecordExpr list ->
            List.any (Node.value >> Tuple.second >> containsFunctionOrValue) list

        Expression.UnitExpr ->
            False

        Expression.Floatable _ ->
            False

        Expression.Integer _ ->
            False

        Expression.Literal _ ->
            False

        Expression.CharLiteral _ ->
            False

        Expression.Hex _ ->
            False

        Expression.RecordAccess _ _ ->
            False

        Expression.RecordAccessFunction _ ->
            False

        Expression.Negation next ->
            containsFunctionOrValue next

        Expression.LambdaExpression _ ->
            False

        Expression.PrefixOperator _ ->
            False

        Expression.Operator _ ->
            False


fixAlways : { always : Range, application : Range } -> Node Expression -> List Fix
fixAlways ranges expression =
    [ Fix.insertAt ranges.application.end ")"
    , Fix.replaceRangeBy ranges.always "\\_ ->"
    , Fix.insertAt ranges.application.start "("
    ]


errorWithFix : Range -> List Fix -> Error {}
errorWithFix range fix =
    Rule.errorWithFix
        { message = "`always` is not allowed."
        , details =
            [ "You should replace this `always` with an anonymous function `\\_ ->`."
            , "It's more concise, more recognizable as a function, and makes it easier to change your mind later and name the argument."
            ]
        }
        range
        fix


errorWithWarning : Range -> Error {}
errorWithWarning range =
    Rule.error
        { message = "`always` is not allowed."
        , details =
            [ "You should replace this `always` with an anonymous function `\\_ ->`."
            , "It's more concise, more recognizable as a function, and makes it easier to change your mind later and name the argument."
            , "Caution: If this always does some heavy computation then you may not want that within an anonymous function as the work will be done every time. Instead, do the calculation in a nearby let..in block first."
            ]
        }
        range
