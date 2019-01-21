module AlignItems exposing (AlignItems(..), default)


type AlignItems
    = FlexStart
    | FlexEnd
    | Center
    | Baseline
    | Stretch


default =
    Stretch
