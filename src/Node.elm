module Node exposing (Node)

import AlignContent exposing (AlignContent)
import AlignItems exposing (AlignItems)
import AlignSelf exposing (AlignSelf)
import Dimension exposing (Dimension)
import FlexDirection exposing (FlexDirection)
import JustifyContent exposing (JustifyContent)
import Rectangle exposing (Rectangle)
import Size exposing (Size)


type Direction
    = Inherit
    | LTR
    | RTL


defaultDirection =
    Inherit


type Display
    = Flex
    | None


defaultDisplay =
    Flex


type Overflow
    = Visible
    | Hidden
    | Scroll


defaultOverflow =
    Visible


type PositionType
    = Relative
    | Absolute


defaultPositionType =
    Relative


type FlexWrap
    = NoWrap
    | Wrap
    | WrapReverse


defaultFlexWrap =
    NoWrap


type alias Point a =
    { x : a, y : a }


{-| TODO implement this
-}
type alias ComputeResult =
    ()


type alias LayoutCache =
    { nodeSize : Size Number
    , parentSize : Size Number
    , performLayout : Bool
    , result : ComputeResult
    }


type Node
    = Node
        { display : Display
        , position_type : PositionType
        , direction : Direction
        , flex_direction : FlexDirection
        , flex_wrap : FlexWrap
        , overflow : Overflow
        , align_items : AlignItems
        , align_self : AlignSelf
        , align_content : AlignContent
        , justify_content : JustifyContent
        , position : Rectangle Dimension
        , margin : Rectangle Dimension
        , padding : Rectangle Dimension
        , border : Rectangle Dimension
        , flex_grow : Float
        , flex_shrink : Float
        , flex_basis : Dimension
        , size : Size Dimension
        , min_size : Size Dimension
        , max_size : Size Dimension
        , aspect_ratio : Number
        , measure : Maybe (Size Number -> Size Float)
        , children : List Node
        , layout_cache : Maybe LayoutCache
        }


type alias Number =
    Maybe Float
