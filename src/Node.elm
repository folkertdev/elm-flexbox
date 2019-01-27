module Node exposing
    ( ComputeResult
    , Display(..)
    , FlexWrap(..)
    , LayoutCache
    , LayoutNode(..)
    , Node(..)
    , Point
    , PositionType(..)
    , align
    , crossMarginEnd
    , crossMarginStart
    , crossSize
    , default
    , mainMarginEnd
    , mainMarginStart
    , maxCrossSize
    , maxMainSize
    , minCrossSize
    , minMainSize
    )

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


type LayoutNode
    = LayoutNode { order : Int, size : Size Float, location : Point Float, children : List LayoutNode }


type alias ComputeResult =
    { size : Size Float
    , children : List LayoutNode
    }


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


defaultRectangleDimension =
    { start = Dimension.Undefined
    , end = Dimension.Undefined
    , top = Dimension.Undefined
    , bottom = Dimension.Undefined
    }


defaultSizeDimension =
    { width = Dimension.Auto, height = Dimension.Auto }


default : Node
default =
    Node
        { display = defaultDisplay
        , position_type = defaultPositionType
        , direction = defaultDirection
        , flex_direction = FlexDirection.default
        , flex_wrap = defaultFlexWrap
        , overflow = defaultOverflow
        , align_items = AlignItems.default
        , align_self = AlignSelf.default
        , align_content = AlignContent.default
        , justify_content = JustifyContent.default
        , position = defaultRectangleDimension
        , margin = defaultRectangleDimension
        , padding = defaultRectangleDimension
        , border = defaultRectangleDimension
        , flex_grow = 0
        , flex_shrink = 1
        , flex_basis = Dimension.default
        , size = defaultSizeDimension
        , min_size = defaultSizeDimension
        , max_size = defaultSizeDimension

        -- v check that
        , aspect_ratio = Nothing
        , measure = Nothing
        , children = []
        , layout_cache = Nothing
        }


align : { node : Node, parent : Node } -> AlignSelf
align r =
    let
        (Node node) =
            r.node

        (Node parent) =
            r.parent
    in
    if node.align_self == AlignSelf.Auto then
        case parent.align_items of
            AlignItems.FlexStart ->
                AlignSelf.FlexStart

            AlignItems.FlexEnd ->
                AlignSelf.FlexEnd

            AlignItems.Center ->
                AlignSelf.Center

            AlignItems.Baseline ->
                AlignSelf.Baseline

            AlignItems.Stretch ->
                AlignSelf.Stretch

    else
        node.align_self


crossMarginStart : FlexDirection -> Node -> Dimension
crossMarginStart direction (Node node) =
    if FlexDirection.isRow direction then
        node.margin.top

    else
        node.margin.bottom


crossMarginEnd : FlexDirection -> Node -> Dimension
crossMarginEnd direction (Node node) =
    if FlexDirection.isRow direction then
        node.margin.bottom

    else
        node.margin.top


crossSize : FlexDirection -> Node -> Dimension
crossSize direction (Node node) =
    if FlexDirection.isRow direction then
        node.size.height

    else
        node.size.width


mainMarginStart : FlexDirection -> Node -> Dimension
mainMarginStart direction (Node node) =
    if FlexDirection.isRow direction then
        node.margin.start

    else
        node.margin.top


mainMarginEnd : FlexDirection -> Node -> Dimension
mainMarginEnd direction (Node node) =
    if FlexDirection.isRow direction then
        node.margin.end

    else
        node.margin.bottom


minMainSize : FlexDirection -> Node -> Dimension
minMainSize direction (Node node) =
    if FlexDirection.isRow direction then
        node.min_size.width

    else
        node.min_size.height


maxMainSize : FlexDirection -> Node -> Dimension
maxMainSize direction (Node node) =
    if FlexDirection.isRow direction then
        node.max_size.width

    else
        node.max_size.height


minCrossSize : FlexDirection -> Node -> Dimension
minCrossSize direction (Node node) =
    if FlexDirection.isRow direction then
        node.min_size.height

    else
        node.min_size.width


maxCrossSize : FlexDirection -> Node -> Dimension
maxCrossSize direction (Node node) =
    if FlexDirection.isRow direction then
        node.max_size.height

    else
        node.max_size.width
