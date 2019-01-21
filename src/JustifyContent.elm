module JustifyContent exposing (JustifyContent(..), default)


type JustifyContent
    = FlexStart
    | FlexEnd
    | Center
    | SpaceBetween
    | SpaceAround
    | SpaceEvenly


default =
    FlexStart
