module Dimension exposing (Dimension(..), default, resolve)


type Dimension
    = Undefined
    | Auto
    | Points Float
    | Percent Float


default =
    Undefined


resolve parentWidth dimension =
    case dimension of
        Points points ->
            Just points

        Percent percent ->
            Just (parentWidth * percent)

        _ ->
            Nothing


isDefined dimension =
    case dimension of
        Points points ->
            True

        Percent percent ->
            True

        _ ->
            False


defaultRect =
    { start = default, end = default, top = default, bottom = default }


defaultSize =
    { width = Auto, height = Auto }
