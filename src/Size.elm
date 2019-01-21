module Size exposing (Size)


type alias Size a =
    { width : a, height : a }


undefined =
    { width = Nothing, height = Nothing }
