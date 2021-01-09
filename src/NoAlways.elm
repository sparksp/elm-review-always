module NoAlways exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Range.Extra as RangeExtra
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)


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
    Rule.newModuleRuleSchemaUsingContextCreator "NoNothingToNothing" contextCreator
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type Context
    = Context ModuleNameLookupTable ()


contextCreator : Rule.ContextCreator () Context
contextCreator =
    Rule.initContextCreator Context
        |> Rule.withModuleNameLookupTable


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor (Node range node) ((Context lookupTable ()) as context) =
    case node of
        Expression.Application [ maybeAlwaysExpression, expression ] ->
            ( alwaysExpressionErrors lookupTable maybeAlwaysExpression expression range, context )

        Expression.OperatorApplication "<|" _ maybeAlwaysExpression expression ->
            ( alwaysExpressionErrors lookupTable maybeAlwaysExpression expression range, context )

        Expression.OperatorApplication "|>" _ expression maybeAlwaysExpression ->
            ( alwaysExpressionErrors lookupTable maybeAlwaysExpression expression range, context )

        _ ->
            ( [], context )


alwaysExpressionErrors : ModuleNameLookupTable -> Node Expression -> Node Expression -> Range -> List (Rule.Error {})
alwaysExpressionErrors lookupTable alwaysExpression expression range =
    case Node.value alwaysExpression of
        Expression.FunctionOrValue _ "always" ->
            case ModuleNameLookupTable.moduleNameFor lookupTable alwaysExpression of
                Just [ "Basics" ] ->
                    [ alwaysExpressionError { always = Node.range alwaysExpression, application = range } expression ]

                _ ->
                    []

        _ ->
            []


alwaysExpressionError : { always : Range, application : Range } -> Node Expression -> Rule.Error {}
alwaysExpressionError ranges expression =
    case getConstantExpressionRange expression of
        Just expressionRange ->
            errorWithFix ranges.always
                (fixAlways
                    { always = ranges.always
                    , application = ranges.application
                    , expression = expressionRange
                    }
                )

        Nothing ->
            errorWithWarning ranges.always


getConstantExpressionRange : Node Expression -> Maybe Range
getConstantExpressionRange (Node range expression) =
    case expression of
        Expression.UnitExpr ->
            Just range

        Expression.Floatable _ ->
            Just range

        Expression.Integer _ ->
            Just range

        Expression.Literal _ ->
            Just range

        Expression.CharLiteral _ ->
            Just range

        Expression.Hex _ ->
            Just range

        Expression.RecordAccess _ _ ->
            Just range

        Expression.RecordAccessFunction _ ->
            Just range

        Expression.LambdaExpression _ ->
            Just range

        Expression.PrefixOperator _ ->
            Just range

        Expression.Operator _ ->
            Just range

        Expression.FunctionOrValue _ name ->
            String.uncons name
                |> Maybe.andThen
                    (\( start, _ ) ->
                        if Char.isUpper start then
                            Just range

                        else
                            Nothing
                    )

        Expression.Application list ->
            case list of
                (Node _ (Expression.FunctionOrValue _ _)) :: _ ->
                    List.foldr foldConstantExpression (Just range) list

                _ ->
                    Nothing

        Expression.Negation next ->
            getConstantExpressionRange next
                |> Maybe.map (\_ -> range)

        Expression.ParenthesizedExpression next ->
            getConstantExpressionRange next
                |> Maybe.map (\_ -> range)

        Expression.TupledExpression list ->
            List.foldr foldConstantExpression (Just range) list

        Expression.ListExpr list ->
            List.foldr foldConstantExpression (Just range) list

        Expression.RecordExpr list ->
            List.map (Node.value >> Tuple.second) list
                |> List.foldr foldConstantExpression (Just range)

        Expression.OperatorApplication _ _ _ _ ->
            Nothing

        Expression.IfBlock _ _ _ ->
            Nothing

        Expression.CaseExpression _ ->
            Nothing

        Expression.LetExpression _ ->
            Nothing

        Expression.RecordUpdateExpression _ _ ->
            Nothing

        Expression.GLSLExpression _ ->
            Nothing


foldConstantExpression : Node Expression -> Maybe Range -> Maybe Range
foldConstantExpression node acc =
    getConstantExpressionRange node
        |> Maybe.andThen (\_ -> acc)


fixAlways : { always : Range, application : Range, expression : Range } -> List Fix
fixAlways ranges =
    case RangeExtra.compare ranges.always ranges.expression of
        LT ->
            let
                replaceRange : Range
                replaceRange =
                    { start = ranges.always.start
                    , end = ranges.expression.start
                    }
            in
            [ Fix.insertAt ranges.application.end ")"
            , Fix.replaceRangeBy replaceRange "(\\_ -> "
            ]

        _ ->
            let
                replaceRange : Range
                replaceRange =
                    { start = ranges.expression.end
                    , end = ranges.always.end
                    }
            in
            [ Fix.replaceRangeBy replaceRange ")"
            , Fix.insertAt ranges.application.start "(\\_ -> "
            ]


errorWithFix : Range -> List Fix -> Rule.Error {}
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


errorWithWarning : Range -> Rule.Error {}
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
