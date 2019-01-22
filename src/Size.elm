module Size exposing (Size, crossAxis, mainAxis, map, setCrossAxis, setMainAxis, undefined)

import FlexDirection exposing (FlexDirection)


type alias Size a =
    { width : a, height : a }


undefined : Size (Maybe a)
undefined =
    { width = Nothing, height = Nothing }


map : (a -> b) -> Size a -> Size b
map f { width, height } =
    { width = f width, height = f height }


mainAxis : FlexDirection -> Size a -> a
mainAxis direction { width, height } =
    if FlexDirection.isRow direction then
        width

    else
        height


crossAxis : FlexDirection -> Size a -> a
crossAxis direction { width, height } =
    if FlexDirection.isRow direction then
        height

    else
        width


setMainAxis : FlexDirection -> a -> Size a -> Size a
setMainAxis direction newValue size =
    if FlexDirection.isRow direction then
        { size | width = newValue }

    else
        { size | height = newValue }


setCrossAxis : FlexDirection -> a -> Size a -> Size a
setCrossAxis direction newValue size =
    if FlexDirection.isRow direction then
        { size | height = newValue }

    else
        { size | width = newValue }
