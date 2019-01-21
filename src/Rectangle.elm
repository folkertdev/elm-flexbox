module Rectangle exposing (Rectangle)

import FlexDirection


type alias Rectangle a =
    { start : a, end : a, top : a, bottom : a }


map : (a -> b) -> Rectangle a -> Rectangle b
map f rectangle =
    { start = f rectangle.start
    , end = f rectangle.end
    , top = f rectangle.top
    , bottom = f rectangle.bottom
    }


horizontal : Rectangle number -> number
horizontal rectangle =
    rectangle.start + rectangle.end


vertical : Rectangle number -> number
vertical rectangle =
    rectangle.top + rectangle.bottom


mainAxis : FlexDirection.FlexDirection -> Rectangle number -> number
mainAxis direction rectangle =
    if FlexDirection.isRow direction then
        horizontal rectangle

    else
        vertical rectangle


crossAxis : FlexDirection.FlexDirection -> Rectangle number -> number
crossAxis direction rectangle =
    if FlexDirection.isRow direction then
        vertical rectangle

    else
        horizontal rectangle


mainAxisStart : FlexDirection.FlexDirection -> Rectangle number -> number
mainAxisStart direction rectangle =
    if FlexDirection.isRow direction then
        rectangle.start

    else
        rectangle.top


mainAxisEnd : FlexDirection.FlexDirection -> Rectangle number -> number
mainAxisEnd direction rectangle =
    if FlexDirection.isRow direction then
        rectangle.end

    else
        rectangle.bottom


crossAxisStart : FlexDirection.FlexDirection -> Rectangle number -> number
crossAxisStart direction rectangle =
    if FlexDirection.isRow direction then
        rectangle.top

    else
        rectangle.start


crossAxisEnd : FlexDirection.FlexDirection -> Rectangle number -> number
crossAxisEnd direction rectangle =
    if FlexDirection.isRow direction then
        rectangle.bottom

    else
        rectangle.end
