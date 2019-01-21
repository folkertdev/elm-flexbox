module AlignContent exposing (AlignContent(..), default)


type AlignContent
    = FlexStart
    | FlexEnd
    | Center
    | Stretch
    | SpaceBetween
    | SpaceAround


default =
    Stretch
