module Dimension exposing (Dimension(..), default, isDefined, resolve, resolveNumber)


type Dimension
    = Undefined
    | Auto
    | Points Float
    | Percent Float


default =
    Undefined


type alias Number =
    Maybe Float


resolveNumber : Number -> Dimension -> Maybe Float
resolveNumber maybeParentWidth dimension =
    case maybeParentWidth of
        Just parentWidth ->
            resolve parentWidth dimension

        Nothing ->
            Nothing


resolve : Float -> Dimension -> Maybe Float
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
