module NoAlways exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
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
        Expression.Application [ Node alwaysRange (Expression.FunctionOrValue moduleName "always"), expression ] ->
            if isAlwaysFunction moduleName "always" then
                [ alwaysExpressionError { always = alwaysRange, application = range } expression
                ]

            else
                []

        Expression.OperatorApplication "<|" _ (Node alwaysRange (Expression.FunctionOrValue moduleName "always")) expression ->
            if isAlwaysFunction moduleName "always" then
                [ alwaysExpressionError { always = alwaysRange, application = range } expression
                ]

            else
                []

        _ ->
            []


isAlwaysFunction : ModuleName -> String -> Bool
isAlwaysFunction moduleName functionName =
    case moduleName of
        [] ->
            True

        [ "Basics" ] ->
            True

        _ ->
            False


alwaysExpressionError : { always : Range, application : Range } -> Node Expression -> Error {}
alwaysExpressionError ranges expression =
    if isConstantExpression expression then
        errorWithFix ranges.always
            (fixAlways
                { always = ranges.always
                , application = ranges.application
                , expression = expressionRange expression
                }
            )

    else
        errorWithWarning ranges.always


isConstantExpression : Node Expression -> Bool
isConstantExpression (Node _ expression) =
    case expression of
        Expression.UnitExpr ->
            True

        Expression.Floatable _ ->
            True

        Expression.Integer _ ->
            True

        Expression.Literal _ ->
            True

        Expression.CharLiteral _ ->
            True

        Expression.Hex _ ->
            True

        Expression.RecordAccess _ _ ->
            True

        Expression.RecordAccessFunction _ ->
            True

        Expression.LambdaExpression _ ->
            True

        Expression.PrefixOperator _ ->
            True

        Expression.Operator _ ->
            True

        Expression.FunctionOrValue _ name ->
            case String.uncons name of
                Just ( start, _ ) ->
                    Char.isUpper start

                Nothing ->
                    True

        Expression.Negation next ->
            isConstantExpression next

        Expression.ParenthesizedExpression next ->
            isConstantExpression next

        Expression.Application list ->
            List.all isConstantExpression list

        Expression.TupledExpression list ->
            List.all isConstantExpression list

        Expression.ListExpr list ->
            List.all isConstantExpression list

        Expression.OperatorApplication _ _ left right ->
            List.all isConstantExpression [ left, right ]

        Expression.RecordExpr list ->
            List.all (Node.value >> Tuple.second >> isConstantExpression) list

        Expression.IfBlock _ _ _ ->
            False

        Expression.CaseExpression _ ->
            False

        Expression.LetExpression _ ->
            False

        Expression.RecordUpdateExpression _ _ ->
            False

        Expression.GLSLExpression _ ->
            False


fixAlways : { always : Range, application : Range, expression : Range } -> List Fix
fixAlways ranges =
    let
        replaceRange =
            { start = ranges.always.start
            , end = ranges.expression.start
            }
    in
    [ Fix.insertAt ranges.application.end ")"
    , Fix.replaceRangeBy replaceRange "\\_ -> "
    , Fix.insertAt ranges.application.start "("
    ]


expressionRange : Node Expression -> Range
expressionRange expression =
    case expression of
        -- [ ] Remove this when elm-syntax v7.1.3 is released
        Node _ (Expression.RecordAccess (Node startRange _) (Node endRange _)) ->
            Range.combine [ startRange, endRange ]

        Node range _ ->
            range


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
