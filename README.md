# elm-review-always

![elm package](https://img.shields.io/elm-package/v/sparksp/elm-review-always)
![elm-review 2.0](https://img.shields.io/badge/elm--review-2.0-%231293D8)
![elm 0.19](https://img.shields.io/badge/elm-0.19-%231293D8)
![Tests](https://github.com/sparksp/elm-review-always/workflows/Tests/badge.svg)

Provides an [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule to forbid the use of `always`.


## Use an [anonymous function] `\_ ->` instead of [`always`][always].

It's more concise, more recognizable as a function, and makes it easier to change your mind later and name the argument.

```elm
-- Don't do this --
List.map (always 0) [ 1, 2, 3, 4 ]

-- Instead do this --
List.map (\_ -> 0) [ 1, 2, 3, 4 ]
```

[always]: https://package.elm-lang.org/packages/elm/core/latest/Basics#always
[anonymous function]: https://elm-lang.org/docs/syntax#functions


## Example configuration

```elm
import NoAlways
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoAlways.rule
    ]
```


## Caution: Heavy Computation

If the value you always want is the result of some heavy computation then you will not want that within an anonymous function as the work will be done every time. Instead, do the calculation in a nearby [`let..in`][let-expression] block first.

```elm
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
```

[let-expression]: https://elm-lang.org/docs/syntax#let-expressions


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template sparksp/elm-review-always/example
```
