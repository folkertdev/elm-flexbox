module Rectangle exposing (Rectangle, crossAxis, crossAxisEnd, crossAxisStart, horizontal, mainAxis, mainAxisEnd, mainAxisNumber, mainAxisStart, map, vertical)

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


type alias Number =
    Maybe Float


horizontalNumber : Rectangle Number -> Number
horizontalNumber rectangle =
    Maybe.map2 (+) rectangle.start rectangle.end


verticalNumber : Rectangle Number -> Number
verticalNumber rectangle =
    Maybe.map2 (+) rectangle.top rectangle.bottom


mainAxis : FlexDirection.FlexDirection -> Rectangle number -> number
mainAxis direction rectangle =
    if FlexDirection.isRow direction then
        horizontal rectangle

    else
        vertical rectangle


mainAxisNumber : FlexDirection.FlexDirection -> Rectangle Number -> Number
mainAxisNumber direction rectangle =
    if FlexDirection.isRow direction then
        horizontalNumber rectangle

    else
        verticalNumber rectangle


crossAxis : FlexDirection.FlexDirection -> Rectangle number -> number
crossAxis direction rectangle =
    if FlexDirection.isRow direction then
        vertical rectangle

    else
        horizontal rectangle


mainAxisStart : FlexDirection.FlexDirection -> Rectangle a -> a
mainAxisStart direction rectangle =
    if FlexDirection.isRow direction then
        rectangle.start

    else
        rectangle.top


mainAxisEnd : FlexDirection.FlexDirection -> Rectangle a -> a
mainAxisEnd direction rectangle =
    if FlexDirection.isRow direction then
        rectangle.end

    else
        rectangle.bottom


crossAxisStart : FlexDirection.FlexDirection -> Rectangle a -> a
crossAxisStart direction rectangle =
    if FlexDirection.isRow direction then
        rectangle.top

    else
        rectangle.start


crossAxisEnd : FlexDirection.FlexDirection -> Rectangle a -> a
crossAxisEnd direction rectangle =
    if FlexDirection.isRow direction then
        rectangle.bottom

    else
        rectangle.end
