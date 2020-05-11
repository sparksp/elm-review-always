# review-always

![elm package](https://img.shields.io/elm-package/v/sparksp/elm-review-always)
![elm-review 2.0](https://img.shields.io/badge/elm--review-2.0-%231293D8)
![elm 0.19](https://img.shields.io/badge/elm-0.19-%231293D8)
![Tests](https://github.com/sparksp/elm-review-always/workflows/Tests/badge.svg)

Provides an [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule to forbid the use of `always`.

## Provided rule

- [`NoAlways`](https://package.elm-lang.org/packages/sparksp/elm-review-always/latest/NoAlways) - forbid the use of `always`.

## Example configuration

```elm
module ReviewConfig exposing (config)

import Review.Rule exposing (Rule)
import NoAlways


config : List Rule
config =
    [ NoAlways.rule
    ]
```
