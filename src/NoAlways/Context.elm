module NoAlways.Context exposing
    ( initial
    , needBrackets, clearNeedBrackets, resetNeedBrackets
    , Module
    )

{-|

@docs Context, initial


## Brackets

@docs needBrackets, clearNeedBrackets, resetNeedBrackets

-}


{-| Module Context for NoAlways
-}
type Module
    = Brackets Bool


{-| New Module Context
-}
initial : Module
initial =
    Brackets True


{-| If I turn the next expression into a lambda would I need to wrap it with brackets?

    -- Brackets needed
    foo = always "bar"
    -> foo = (\_ -> "bar")

    -- Brackets not needed
    foo = bar (always "bar")
    -> foo = bar (\_ -> "bar")

-}
needBrackets : Module -> Bool
needBrackets (Brackets brackets) =
    brackets


{-| Mark that the next expression **does not** want brackets.
-}
clearNeedBrackets : Module -> Module
clearNeedBrackets _ =
    Brackets False


{-| Mark that the next expression **does want** brackets.
-}
resetNeedBrackets : Module -> Module
resetNeedBrackets _ =
    Brackets True
