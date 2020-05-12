module NoAlways exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Writer
import NoAlways.Context as Context
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Error, Rule)


{-| Forbid the use of `always`.

    config : List Rule
    config =
        [ NoAlways.rule
        ]

Use anonymous function `\_ ->` over `always`.

It's more concise, more recognizable as a function, and makes it easier to change your mind later and name the argument.


## Failure

    -- Don't do this --
    List.map (always 0) [ 1, 2, 3, 4 ]


## Success

    -- Instead do this --
    List.map (\_ -> 0) [ 1, 2, 3, 4 ]

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoAlways" Context.initial
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> Rule.Direction -> Context.Module -> ( List (Error {}), Context.Module )
expressionVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Expression.ParenthesizedExpression _ ) ->
            ( [], Context.clearNeedBrackets context )

        ( Rule.OnEnter, Expression.Application (function :: [ expression ]) ) ->
            case Node.value function of
                Expression.FunctionOrValue [] "always" ->
                    let
                        fix : Fix
                        fix =
                            expression
                                |> applyLambda
                                |> applyBrackets context
                                |> expressionToString
                                |> Fix.replaceRangeBy (Node.range node)
                    in
                    ( [ error (Node.range function) [ fix ] ]
                    , Context.resetNeedBrackets context
                    )

                _ ->
                    ( [], Context.resetNeedBrackets context )

        ( Rule.OnEnter, _ ) ->
            ( [], Context.resetNeedBrackets context )

        ( Rule.OnExit, _ ) ->
            ( [], context )


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
        , expression = stripBrackets expression
        }
        |> Node.Node Range.emptyRange


stripBrackets : Node Expression -> Node Expression
stripBrackets expression =
    case Node.value expression of
        Expression.ParenthesizedExpression inner ->
            inner

        _ ->
            expression


applyBrackets : Context.Module -> Node Expression -> Node Expression
applyBrackets context expression =
    if Context.needBrackets context then
        expression
            |> Expression.ParenthesizedExpression
            |> Node.Node Range.emptyRange

    else
        expression


expressionToString : Node Expression -> String
expressionToString expression =
    expression
        |> Elm.Writer.writeExpression
        |> Elm.Writer.write
