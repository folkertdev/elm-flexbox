module AlignSelf exposing (AlignSelf(..), default)


type AlignSelf
    = Auto
    | FlexStart
    | FlexEnd
    | Center
    | Baseline
    | Stretch


default =
    Auto
