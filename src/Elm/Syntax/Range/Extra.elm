module Elm.Syntax.Range.Extra exposing (compare)

import Elm.Syntax.Range as Range exposing (Range)


compare : Range -> Range -> Order
compare left right =
    case compareLocation left.start right.start of
        EQ ->
            compareLocation left.end right.end

        order ->
            order


compareLocation : Range.Location -> Range.Location -> Order
compareLocation left right =
    case Basics.compare left.row right.row of
        EQ ->
            Basics.compare left.column right.column

        order ->
            order
