module Algorithm exposing (FlexItem)

import AlignContent
import AlignSelf
import Dimension
import FlexDirection exposing (FlexDirection)
import JustifyContent
import Node exposing (ComputeResult, FlexWrap(..), LayoutCache, LayoutNode(..), Node(..), Point)
import Rectangle exposing (Rectangle)
import Size exposing (Size)


{-| TODO find out what rust's f32::EPSILON is
-}
epsilon =
    1.0e-6


type alias Number =
    Maybe Float


type alias FlexItem =
    { node : Node.Node
    , size : Size Number
    , min_size : Size Number
    , max_size : Size Number
    , position : Rectangle Number
    , margin : Rectangle Float
    , padding : Rectangle Float
    , border : Rectangle Float
    , flex_basis : Float
    , inner_flex_basis : Float
    , violation : Float
    , frozen : Bool
    , hypothetical_inner_size : Size Float
    , hypothetical_outer_size : Size Float
    , target_size : Size Float
    , outer_target_size : Size Float
    , baseline : Float
    , -- temporary values for holding offset in the main / cross direction.
      -- offset is the relative position from the item's natural flow position based on
      -- relative position values, alignment, and justification. Does not include margin/padding/border.
      offset_main : Float
    , offset_cross : Float
    }


type alias FlexLine =
    { items : List FlexItem
    , cross_size : Float
    , offset_cross : Float
    }



-- Compute


has_min_max : Node -> Bool
has_min_max (Node node) =
    Dimension.isDefined node.min_size.width
        || Dimension.isDefined node.min_size.height
        || Dimension.isDefined node.max_size.width
        || Dimension.isDefined node.max_size.height


compute : Node -> Size Number -> LayoutNode
compute (Node root) size =
    let
        result =
            if has_min_max (Node root) then
                let
                    first_pass =
                        compute_internal (Node root)
                            { width = Maybe.andThen (\w -> Dimension.resolve w root.size.width) size.width
                            , height = Maybe.andThen (\w -> Dimension.resolve w root.size.height) size.height
                            }
                            size
                            False
                in
                compute_internal (Node root)
                    { width =
                        let
                            initial : Float
                            initial =
                                first_pass.size.width

                            upperBound =
                                Maybe.andThen (\w -> Dimension.resolve w root.min_size.width) size.width

                            lowerBound =
                                Maybe.andThen (\w -> Dimension.resolve w root.max_size.width) size.width
                        in
                        initial
                            |> Just
                            |> Maybe.map2 max upperBound
                            |> Maybe.map2 min lowerBound
                            |> Maybe.withDefault 0
                            |> Just
                    , height =
                        let
                            initial : Float
                            initial =
                                first_pass.size.height

                            upperBound =
                                Maybe.andThen (\w -> Dimension.resolve w root.min_size.height) size.height

                            lowerBound =
                                Maybe.andThen (\w -> Dimension.resolve w root.max_size.height) size.height
                        in
                        initial
                            |> Just
                            |> Maybe.map2 max upperBound
                            |> Maybe.map2 min lowerBound
                            |> Maybe.withDefault 0
                            |> Just
                    }
                    size
                    True

            else
                compute_internal (Node root)
                    { width = Maybe.andThen (\w -> Dimension.resolve w root.size.width) size.width
                    , height = Maybe.andThen (\w -> Dimension.resolve w root.size.height) size.height
                    }
                    size
                    True

        layout =
            LayoutNode
                { order = 0
                , size = { width = result.size.width, height = result.size.height }
                , location = { x = 0, y = 0 }
                , children = result.children
                }
    in
    round_layout layout 0.0 0.0


round_layout : LayoutNode -> Float -> Float -> LayoutNode
round_layout (LayoutNode layout) abs_x abs_y =
    let
        x =
            abs_x + layout.location.x

        y =
            abs_y + layout.location.y

        location =
            { x = toFloat <| round layout.location.x
            , y = toFloat <| round layout.location.y
            }

        size =
            { width = toFloat <| round (x + layout.size.width) + round x
            , height = toFloat <| round (y + layout.size.height) + round y
            }
    in
    LayoutNode
        { order = layout.order
        , location = location
        , size = size
        , children =
            List.map (\child -> round_layout child x y) layout.children
        }


compute_internal : Node -> Size Number -> Size Number -> Bool -> ComputeResult
compute_internal (Node node) node_size parent_size perform_layout =
    let
        helper : Maybe ComputeResult
        helper =
            case node.layout_cache of
                Nothing ->
                    Nothing

                Just cache ->
                    let
                        width_compatible =
                            case node_size.width of
                                Just width ->
                                    abs (width - cache.result.size.width) < epsilon

                                Nothing ->
                                    isUndefined cache.nodeSize.width

                        height_compatible =
                            case node_size.height of
                                Just height ->
                                    abs (height - cache.result.size.height) < epsilon

                                Nothing ->
                                    isUndefined cache.nodeSize.height
                    in
                    if width_compatible && height_compatible then
                        Just cache.result

                    else if cache.nodeSize == node_size && cache.parentSize == parent_size then
                        Just cache.result

                    else
                        Nothing

        helper2 =
            if List.isEmpty node.children then
                if isDefined node_size.width && isDefined node_size.height then
                    Just ( Node node, { size = Size.map (Maybe.withDefault 0) node_size, children = [] } )

                else
                    case node.measure of
                        Just measure ->
                            let
                                result =
                                    { size = measure node_size, children = [] }
                            in
                            Just
                                ( Node { node | layout_cache = Just { nodeSize = node_size, parentSize = parent_size, performLayout = perform_layout, result = result } }
                                , result
                                )

                        _ ->
                            Nothing

            else
                Nothing

        direction =
            node.flex_direction

        dir =
            direction

        is_row =
            FlexDirection.isRow direction

        is_column =
            FlexDirection.isColumn direction

        is_wrap_reverse =
            node.flex_wrap == WrapReverse

        margin : Rectangle Float
        margin =
            node.margin
                |> Rectangle.map (\n -> Dimension.resolveNumber parent_size.width n |> Maybe.withDefault 0)

        padding : Rectangle Float
        padding =
            node.padding
                |> Rectangle.map (\n -> Dimension.resolveNumber parent_size.width n |> Maybe.withDefault 0)

        border : Rectangle Float
        border =
            node.border
                |> Rectangle.map (\n -> Dimension.resolveNumber parent_size.width n |> Maybe.withDefault 0)

        paddingBorder : Rectangle Float
        paddingBorder =
            { start = padding.start + border.start
            , end = padding.end + border.end
            , top = padding.top + border.top
            , bottom = padding.bottom + border.bottom
            }

        nodeInnerSize : Size Number
        nodeInnerSize =
            { width = Maybe.map (\w -> w - Rectangle.horizontal paddingBorder) node_size.width
            , height = Maybe.map (\w -> w - Rectangle.horizontal paddingBorder) node_size.height
            }

        containerSize =
            { width = 0, height = 0 }

        innerContainerSize =
            { width = 0, height = 0 }

        availableSpace : Size Number
        availableSpace =
            { width =
                subtract (node_size.width |> orElse (subtract parent_size.width (Rectangle.horizontal margin))) (Rectangle.horizontal paddingBorder)
            , height =
                subtract (node_size.height |> orElse (subtract parent_size.height (Rectangle.vertical margin))) (Rectangle.vertical paddingBorder)
            }

        flexItems : List FlexItem
        flexItems =
            node.children
                |> List.filter (\(Node child) -> child.position_type /= Node.Absolute)
                |> List.filter (\(Node child) -> child.display /= Node.None)
                |> List.map
                    (\(Node child) ->
                        { node = Node child
                        , size =
                            { width = Dimension.resolveNumber nodeInnerSize.width child.size.width
                            , height = Dimension.resolveNumber nodeInnerSize.height child.size.height
                            }
                        , min_size =
                            { width = Dimension.resolveNumber nodeInnerSize.width child.min_size.width
                            , height = Dimension.resolveNumber nodeInnerSize.height child.min_size.height
                            }
                        , max_size =
                            { width = Dimension.resolveNumber nodeInnerSize.width child.max_size.width
                            , height = Dimension.resolveNumber nodeInnerSize.height child.max_size.height
                            }
                        , position = Rectangle.map (\p -> Dimension.resolveNumber nodeInnerSize.width p) child.position
                        , margin = Rectangle.map (\p -> Dimension.resolveNumber nodeInnerSize.width p |> Maybe.withDefault 0) child.margin
                        , padding = Rectangle.map (\p -> Dimension.resolveNumber nodeInnerSize.width p |> Maybe.withDefault 0) child.padding
                        , border = Rectangle.map (\p -> Dimension.resolveNumber nodeInnerSize.width p |> Maybe.withDefault 0) child.border
                        , flex_basis = 0
                        , inner_flex_basis = 0
                        , violation = 0
                        , frozen = False
                        , hypothetical_inner_size = { width = 0, height = 0 }
                        , hypothetical_outer_size = { width = 0, height = 0 }
                        , target_size = { width = 0, height = 0 }
                        , outer_target_size = { width = 0, height = 0 }
                        , baseline = 0
                        , offset_main = 0
                        , offset_cross = 0
                        }
                    )

        flexItems2 : List FlexItem
        flexItems2 =
            flexItems
                |> List.map
                    (\child ->
                        let
                            (Node childNode) =
                                child.node

                            flex_basis =
                                Dimension.resolveNumber (Size.mainAxis direction nodeInnerSize) childNode.flex_basis

                            attempt =
                                if isDefined flex_basis then
                                    Just { child | flex_basis = flex_basis |> Maybe.withDefault 0 }

                                else
                                    case ( childNode.aspect_ratio, Size.crossAxis direction node_size ) of
                                        ( Just ratio, Just cross ) ->
                                            if childNode.flex_basis == Dimension.Auto then
                                                Just { child | flex_basis = cross * ratio }

                                            else
                                                Nothing

                                        _ ->
                                            Nothing
                        in
                        case attempt of
                            Just success ->
                                success

                            Nothing ->
                                let
                                    width : Number
                                    width =
                                        if not (isDefined child.size.width) && Node.align { node = Node childNode, parent = Node node } == AlignSelf.Stretch && is_column then
                                            availableSpace.width

                                        else
                                            child.size.width

                                    height : Number
                                    height =
                                        if not (isDefined child.size.height) && Node.align { node = Node childNode, parent = Node node } == AlignSelf.Stretch && is_row then
                                            availableSpace.height

                                        else
                                            child.size.height

                                    newFlexBasis =
                                        compute_internal child.node
                                            { width =
                                                width
                                                    |> maybeMax child.min_size.width
                                                    |> maybeMin child.max_size.width
                                            , height =
                                                height
                                                    |> maybeMax child.min_size.height
                                                    |> maybeMin child.max_size.height
                                            }
                                            availableSpace
                                            False
                                            |> .size
                                            |> Size.mainAxis direction
                                            |> Just
                                            |> maybeMax (Size.mainAxis direction child.min_size)
                                            |> maybeMin (Size.mainAxis direction child.max_size)
                                            -- TODO v is this correct
                                            |> Maybe.withDefault 0
                                in
                                { child | flex_basis = newFlexBasis }
                    )

        flexItems3 : List FlexItem
        flexItems3 =
            let
                mapper : FlexItem -> FlexItem
                mapper child =
                    let
                        min_main =
                            if is_row then
                                compute_internal child.node Size.undefined availableSpace False
                                    |> .size
                                    |> .width
                                    |> Just
                                    |> maybeMax child.min_size.width
                                    |> maybeMin child.size.width

                            else
                                Size.mainAxis direction child.min_size

                        inner_flex_basis : Float
                        inner_flex_basis =
                            child.flex_basis - Rectangle.mainAxis direction child.padding - Rectangle.mainAxis direction child.border

                        hypothetical_inner_size : Size Float
                        hypothetical_inner_size =
                            child.hypothetical_inner_size
                                |> Size.map Just
                                |> Size.setMainAxis direction
                                    (child.flex_basis |> Just |> maybeMax min_main |> maybeMin (Size.mainAxis direction child.max_size))
                                |> Size.map (Maybe.withDefault 0)

                        hypothetical_outer_size =
                            child.hypothetical_outer_size
                                |> Size.setMainAxis direction (Size.mainAxis direction hypothetical_inner_size + Rectangle.mainAxis direction child.margin)
                    in
                    { child
                        | inner_flex_basis = inner_flex_basis
                        , hypothetical_inner_size = hypothetical_inner_size
                        , hypothetical_outer_size = hypothetical_outer_size
                    }
            in
            List.map mapper flexItems2

        flexLines : List FlexLine
        flexLines =
            if node.flex_wrap == NoWrap then
                [ { items = flexItems, cross_size = 0, offset_cross = 0 } ]

            else
                let
                    initialLine =
                        { items = flexItems, cross_size = 0, offset_cross = 0 }

                    folder child ( line, lines, line_length ) =
                        case Size.mainAxis direction availableSpace of
                            Just main ->
                                if line_length > main && not (List.isEmpty line.items) then
                                    ( { items = [ child ], cross_size = 0, offset_cross = 0 }
                                    , lines ++ [ line ]
                                    , Size.mainAxis direction child.hypothetical_outer_size
                                    )

                                else
                                    let
                                        newLine =
                                            { line | items = line.items ++ [ child ] }
                                    in
                                    ( newLine, lines, line_length + Size.mainAxis direction child.hypothetical_outer_size )

                            Nothing ->
                                let
                                    newLine =
                                        { line | items = line.items ++ [ child ] }
                                in
                                ( newLine, lines, line_length + Size.mainAxis direction child.hypothetical_outer_size )
                in
                List.foldl folder ( initialLine, [], 0 ) flexItems3
                    |> (\( l, ls, _ ) -> ls ++ [ l ])

        flexLines2 =
            let
                mapper : FlexLine -> FlexLine
                mapper line =
                    let
                        used_flex_factor =
                            List.foldl (\child accum -> Size.mainAxis direction child.hypothetical_outer_size + accum) 0 line.items

                        growing =
                            used_flex_factor < (Size.mainAxis direction nodeInnerSize |> Maybe.withDefault 0)

                        shrinking =
                            not growing

                        sizeInflexibleItem : FlexItem -> FlexItem
                        sizeInflexibleItem child =
                            let
                                (Node childNode) =
                                    child.node

                                freeze =
                                    (childNode.flex_grow == 0.0 && childNode.flex_shrink == 0.0)
                                        || (growing && child.flex_basis > Size.mainAxis direction child.hypothetical_inner_size)
                                        || (shrinking && child.flex_basis < Size.mainAxis direction child.hypothetical_inner_size)

                                outer_target_size =
                                    child.outer_target_size
                                        |> Size.setMainAxis direction (Size.mainAxis direction child.target_size + Rectangle.mainAxis direction child.margin)
                            in
                            if isUndefined (Size.mainAxis direction nodeInnerSize) && is_row then
                                { child
                                    | target_size =
                                        child.target_size
                                            |> Size.map Just
                                            |> Size.setMainAxis direction
                                                (compute_internal child.node
                                                    { width =
                                                        child.size.width
                                                            |> maybeMax child.min_size.width
                                                            |> maybeMin child.max_size.width
                                                    , height =
                                                        child.size.height
                                                            |> maybeMax child.min_size.height
                                                            |> maybeMin child.max_size.height
                                                    }
                                                    availableSpace
                                                    False
                                                    |> .size
                                                    |> Size.mainAxis direction
                                                    |> Just
                                                    |> maybeMax (Size.mainAxis direction child.min_size)
                                                    |> maybeMin (Size.mainAxis direction child.max_size)
                                                )
                                            |> Size.map (Maybe.withDefault 0)
                                    , frozen = freeze
                                    , outer_target_size = outer_target_size
                                }

                            else
                                { child
                                    | target_size = Size.setMainAxis direction (Size.mainAxis direction child.hypothetical_inner_size) child.target_size
                                    , frozen = freeze
                                    , outer_target_size = outer_target_size
                                }

                        newLine =
                            { line | items = List.map sizeInflexibleItem line.items }

                        initial_used_space =
                            newLine.items
                                |> List.map
                                    (\child ->
                                        Rectangle.mainAxis direction child.margin
                                            + (if child.frozen then
                                                Size.mainAxis direction child.target_size

                                               else
                                                child.flex_basis
                                              )
                                    )
                                |> List.sum

                        initial_free_space =
                            subtract (Size.mainAxis direction nodeInnerSize) initial_used_space |> Maybe.withDefault 0

                        -- TODO does only one loop at the moment
                        ( updatedFrozen, updatedUnfrozen ) =
                            let
                                { frozen, unfrozen } =
                                    partitionFrozenHelper line.items [] []
                            in
                            if List.isEmpty unfrozen then
                                ( frozen, unfrozen )

                            else
                                let
                                    used_space =
                                        (frozen ++ unfrozen)
                                            |> List.map
                                                (\child ->
                                                    Rectangle.mainAxis direction child.margin
                                                        + (if child.frozen then
                                                            Size.mainAxis direction child.target_size

                                                           else
                                                            child.flex_basis
                                                          )
                                                )
                                            |> List.sum

                                    sum_flex_grow =
                                        unfrozen
                                            |> List.map (.node >> unwrap >> .flex_grow)
                                            |> List.sum

                                    sum_flex_shrink =
                                        frozen
                                            |> List.map (.node >> unwrap >> .flex_shrink)
                                            |> List.sum

                                    free_space =
                                        Maybe.withDefault 0 <|
                                            if growing && sum_flex_grow < 1.0 then
                                                maybeMin (subtract (Size.mainAxis direction nodeInnerSize) used_space) (Just <| initial_free_space * sum_flex_grow)

                                            else if shrinking && sum_flex_shrink < 1.0 then
                                                maybeMax (subtract (Size.mainAxis direction nodeInnerSize) used_space) (Just <| initial_free_space * sum_flex_shrink)

                                            else
                                                subtract (Size.mainAxis direction nodeInnerSize) used_space

                                    newUnfrozen =
                                        if isNormalFloat free_space then
                                            if growing && sum_flex_grow > 0 then
                                                unfrozen
                                                    |> List.map
                                                        (\child ->
                                                            { child
                                                                | target_size =
                                                                    child.target_size
                                                                        |> Size.setMainAxis direction (child.flex_basis + free_space * ((child.node |> unwrap |> .flex_grow) / sum_flex_grow))
                                                            }
                                                        )

                                            else if shrinking && sum_flex_shrink > 0 then
                                                let
                                                    sum_scaled_shrink_factor =
                                                        unfrozen
                                                            |> List.map (\child -> child.inner_flex_basis * (child.node |> unwrap |> .flex_shrink))
                                                            |> List.sum
                                                in
                                                if sum_scaled_shrink_factor > 0 then
                                                    unfrozen
                                                        |> List.map
                                                            (\child ->
                                                                let
                                                                    scaled_shrink_factor =
                                                                        child.inner_flex_basis * (child.node |> unwrap |> .flex_shrink)
                                                                in
                                                                { child
                                                                    | target_size =
                                                                        child.target_size
                                                                            |> Size.setMainAxis direction (child.flex_basis + free_space * (scaled_shrink_factor / sum_scaled_shrink_factor))
                                                                }
                                                            )

                                                else
                                                    unfrozen

                                            else
                                                unfrozen

                                        else
                                            unfrozen

                                    ( total_violation, newerUnfrozen ) =
                                        let
                                            folder child ( violation, accum ) =
                                                let
                                                    min_main =
                                                        if is_row && isNothing (child.node |> unwrap |> .measure) then
                                                            compute_internal child.node Size.undefined availableSpace False
                                                                |> .size
                                                                |> .width
                                                                |> Just
                                                                |> maybeMin child.size.width
                                                                |> maybeMax child.min_size.width
                                                                |> Maybe.withDefault 0

                                                        else
                                                            Size.mainAxis direction child.min_size
                                                                |> Maybe.withDefault 0

                                                    max_main =
                                                        Size.mainAxis direction child.max_size

                                                    clamped =
                                                        Size.mainAxis direction child.target_size
                                                            |> Just
                                                            |> maybeMin max_main
                                                            |> maybeMax (Just min_main)
                                                            |> Maybe.withDefault 0
                                                            |> max 0

                                                    childViolation =
                                                        clamped - Size.mainAxis direction child.target_size

                                                    target_size =
                                                        child.target_size
                                                            |> Size.setMainAxis direction clamped

                                                    outer_target_size =
                                                        child.outer_target_size
                                                            |> Size.setMainAxis direction (Size.mainAxis direction child.target_size + Rectangle.mainAxis direction child.margin)
                                                in
                                                ( violation + child.violation, { child | violation = childViolation, target_size = target_size, outer_target_size = outer_target_size } :: accum )
                                        in
                                        List.foldl folder ( 0, [] ) newUnfrozen
                                            |> (\( v, a ) -> ( v, List.reverse a ))

                                    newererUnfrozen =
                                        newerUnfrozen
                                            |> List.map
                                                (\child ->
                                                    if total_violation > 0.0 then
                                                        { child | frozen = child.violation > 0.0 }

                                                    else if total_violation < 0.0 then
                                                        { child | frozen = child.violation < 0.0 }

                                                    else
                                                        { child | frozen = True }
                                                )
                                in
                                ( frozen, newererUnfrozen )
                    in
                    { line | items = updatedFrozen ++ updatedUnfrozen }
            in
            List.map mapper flexLines

        newContainerSize =
            containerSize
                |> Size.setMainAxis direction
                    (Size.mainAxis direction node_size
                        |> Maybe.withDefault
                            (let
                                longest_line =
                                    flexLines2
                                        -- TODO is 0 OK here? maybe use -Infinity?
                                        |> List.foldl
                                            (\line accum ->
                                                let
                                                    length =
                                                        line.items |> List.map (.outer_target_size >> Size.mainAxis direction) |> List.sum
                                                in
                                                max length accum
                                            )
                                            0

                                size =
                                    longest_line + Rectangle.mainAxis direction paddingBorder
                             in
                             case Size.mainAxis direction availableSpace of
                                Just val ->
                                    if List.length flexLines2 > 1 && size < val then
                                        val

                                    else
                                        size

                                Nothing ->
                                    size
                            )
                    )

        newInnerContainerSize =
            innerContainerSize |> Size.setMainAxis direction (Size.mainAxis direction newContainerSize - Rectangle.mainAxis direction paddingBorder)

        -- 9.4 Corss Size Determination
        flexLines3 : List FlexLine
        flexLines3 =
            let
                mapper : FlexItem -> FlexItem
                mapper child =
                    let
                        child_cross =
                            Size.crossAxis direction child.size
                                |> maybeMax (Size.crossAxis direction child.min_size)
                                |> maybeMin (Size.crossAxis direction child.max_size)

                        hypothetical_inner_size =
                            child.hypothetical_inner_size
                                |> Size.setCrossAxis direction
                                    (compute_internal child.node
                                        { width =
                                            if is_row then
                                                child.target_size.width
                                                    |> Just

                                            else
                                                child_cross
                                        , height =
                                            if is_row then
                                                child_cross

                                            else
                                                child.target_size.height
                                                    |> Just
                                        }
                                        { width =
                                            if is_row then
                                                Size.mainAxis direction containerSize
                                                    |> Just

                                            else
                                                availableSpace.width
                                        , height =
                                            if is_row then
                                                availableSpace.height

                                            else
                                                Size.mainAxis direction containerSize
                                                    |> Just
                                        }
                                        False
                                        |> .size
                                        |> Size.crossAxis direction
                                        |> Just
                                        |> maybeMax (Size.crossAxis direction child.min_size)
                                        |> maybeMin (Size.crossAxis direction child.max_size)
                                        |> Maybe.withDefault 0
                                    )

                        hypothetical_outer_size =
                            child.hypothetical_outer_size
                                |> Size.setCrossAxis direction (Size.crossAxis direction hypothetical_inner_size + Rectangle.crossAxis direction child.margin)
                    in
                    { child | hypothetical_inner_size = hypothetical_inner_size, hypothetical_outer_size = hypothetical_outer_size }
            in
            flexLines2
                |> List.map (\line -> { line | items = List.map mapper line.items })

        calc_baseline : LayoutNode -> Float
        calc_baseline (LayoutNode layout) =
            case layout.children of
                [] ->
                    layout.size.height

                x :: _ ->
                    calc_baseline x

        flexLines4 =
            let
                mapper : FlexItem -> FlexItem
                mapper child =
                    let
                        result =
                            compute_internal child.node
                                { width =
                                    if is_row then
                                        Just child.target_size.width

                                    else
                                        Just child.hypothetical_inner_size.width
                                , height =
                                    if is_row then
                                        Just child.hypothetical_inner_size.height

                                    else
                                        Just child.target_size.height
                                }
                                { width =
                                    if is_row then
                                        Just containerSize.width

                                    else
                                        node_size.width
                                , height =
                                    if is_row then
                                        node_size.height

                                    else
                                        Just containerSize.height
                                }
                                True

                        baseline =
                            calc_baseline
                                (LayoutNode
                                    { order = position (\n -> n == child.node) node.children |> Maybe.withDefault 0
                                    , size = result.size
                                    , location = { x = 0, y = 0 }
                                    , children = result.children
                                    }
                                )
                    in
                    { child | baseline = baseline }
            in
            flexLines3
                |> List.map (\line -> { line | items = List.map mapper line.items })

        flexLines5 =
            let
                mapper : FlexLine -> FlexLine
                mapper line =
                    let
                        max_baseline =
                            line.items
                                |> List.map .baseline
                                |> List.foldl max 0

                        help : FlexItem -> Float
                        help child =
                            if
                                (Node.align { node = child.node, parent = Node node } == AlignSelf.Baseline)
                                    && (Node.crossMarginStart direction child.node /= Dimension.Auto)
                                    && (Node.crossMarginEnd direction child.node /= Dimension.Auto)
                                    && (Node.crossSize direction child.node == Dimension.Auto)
                            then
                                max_baseline - child.baseline + Size.crossAxis direction child.hypothetical_outer_size

                            else
                                Size.crossAxis direction child.hypothetical_outer_size

                        cross_size =
                            line.items
                                |> List.map help
                                |> List.foldl max 0
                    in
                    { line | cross_size = cross_size }
            in
            case flexLines4 of
                [ x ] ->
                    if isDefined (Size.crossAxis direction node_size) then
                        [ { x | cross_size = subtract (Size.crossAxis direction node_size) (Rectangle.crossAxis direction paddingBorder) |> Maybe.withDefault 0 } ]

                    else
                        List.map mapper flexLines4

                _ ->
                    List.map mapper flexLines4

        flexLines6 =
            if node.align_content == AlignContent.Stretch && isDefined (Size.crossAxis direction node_size) then
                let
                    total_cross =
                        flexLines5 |> List.map .cross_size |> List.sum

                    inner_cross =
                        subtract (Size.crossAxis direction node_size) (Rectangle.crossAxis direction paddingBorder)
                            |> Maybe.withDefault 0
                in
                if total_cross < inner_cross then
                    let
                        remaining =
                            inner_cross - total_cross

                        addition =
                            remaining / toFloat (List.length flexLines5)
                    in
                    List.map (\line -> { line | cross_size = line.cross_size + addition }) flexLines5

                else
                    flexLines5

            else
                flexLines5

        flexLines7 : List FlexLine
        flexLines7 =
            let
                mapper : Float -> FlexItem -> FlexItem
                mapper line_cross_size child =
                    let
                        target_size =
                            child.target_size
                                |> Size.setCrossAxis direction
                                    (if
                                        (Node.align { node = child.node, parent = Node node } == AlignSelf.Baseline)
                                            && (Node.crossMarginStart direction child.node /= Dimension.Auto)
                                            && (Node.crossMarginEnd direction child.node /= Dimension.Auto)
                                            && (Node.crossSize direction child.node == Dimension.Auto)
                                     then
                                        subtract (Just line_cross_size) (Rectangle.crossAxis direction child.margin)
                                            |> maybeMax (Size.crossAxis direction child.min_size)
                                            |> maybeMin (Size.crossAxis direction child.max_size)
                                            |> Maybe.withDefault 0

                                     else
                                        Size.crossAxis direction child.hypothetical_inner_size
                                    )

                        outer_target_size =
                            child.outer_target_size
                                |> Size.setCrossAxis direction (Size.crossAxis direction child.target_size + Rectangle.crossAxis direction child.margin)
                    in
                    { child | target_size = target_size, outer_target_size = outer_target_size }
            in
            List.map (\line -> { line | items = List.map (mapper line.cross_size) line.items }) flexLines6

        flexLines8 =
            let
                mapper : FlexLine -> FlexLine
                mapper line =
                    let
                        used_space =
                            line.items
                                |> List.map (\child -> Size.mainAxis direction child.outer_target_size)
                                |> List.sum

                        free_space =
                            Size.mainAxis direction innerContainerSize - used_space

                        boolToInt x =
                            if x then
                                1

                            else
                                0

                        num_auto_margins : Int
                        num_auto_margins =
                            let
                                folder child accum =
                                    accum
                                        + boolToInt (Node.mainMarginStart direction child.node == Dimension.Auto)
                                        + boolToInt (Node.mainMarginEnd direction child.node == Dimension.Auto)
                            in
                            List.foldl folder 0 line.items

                        newItems =
                            if free_space > 0 && num_auto_margins > 0 then
                                let
                                    itemMargin =
                                        free_space / toFloat num_auto_margins

                                    h1 child =
                                        let
                                            childMargin =
                                                child.margin

                                            newMargin =
                                                if Node.mainMarginStart direction child.node == Dimension.Auto then
                                                    if is_row then
                                                        { childMargin | start = itemMargin }

                                                    else
                                                        { childMargin | top = itemMargin }

                                                else
                                                    margin
                                        in
                                        { child | margin = newMargin }

                                    h2 child =
                                        let
                                            childMargin =
                                                child.margin

                                            newMargin =
                                                if Node.mainMarginEnd direction child.node == Dimension.Auto then
                                                    if is_row then
                                                        { childMargin | end = itemMargin }

                                                    else
                                                        { childMargin | bottom = itemMargin }

                                                else
                                                    margin
                                        in
                                        { child | margin = newMargin }
                                in
                                List.map (h1 >> h2) line.items

                            else
                                let
                                    num_items =
                                        List.length line.items

                                    layout_reverse =
                                        FlexDirection.isReverse direction

                                    justify_item : Int -> FlexItem -> FlexItem
                                    justify_item i child =
                                        let
                                            is_first =
                                                i == 0

                                            offset_main =
                                                case node.justify_content of
                                                    JustifyContent.FlexStart ->
                                                        if layout_reverse && is_first then
                                                            free_space

                                                        else
                                                            0

                                                    JustifyContent.Center ->
                                                        if is_first then
                                                            free_space / 2

                                                        else
                                                            0

                                                    JustifyContent.FlexEnd ->
                                                        if is_first && not layout_reverse then
                                                            free_space

                                                        else
                                                            0

                                                    JustifyContent.SpaceBetween ->
                                                        if is_first then
                                                            0

                                                        else
                                                            free_space / toFloat (num_items - 1)

                                                    JustifyContent.SpaceAround ->
                                                        if is_first then
                                                            (free_space / toFloat num_items) / 2

                                                        else
                                                            free_space / toFloat num_items

                                                    JustifyContent.SpaceEvenly ->
                                                        free_space / toFloat (num_items + 1)
                                        in
                                        { child | offset_main = offset_main }
                                in
                                if layout_reverse then
                                    line.items
                                        |> List.reverse
                                        |> List.indexedMap justify_item

                                else
                                    line.items
                                        |> List.indexedMap justify_item
                    in
                    { line | items = newItems }
            in
            List.map mapper flexLines7

        --  cross-axis alignment
        flexLines9 =
            let
                mapper : Float -> Float -> FlexItem -> FlexItem
                mapper line_cross_size max_baseline child =
                    let
                        free_space =
                            line_cross_size - Size.crossAxis direction child.outer_target_size

                        childMargin =
                            child.margin
                    in
                    if Node.crossMarginStart direction child.node == Dimension.Auto && Node.crossMarginEnd direction child.node == Dimension.Auto then
                        { child
                            | margin =
                                if is_row then
                                    { childMargin | top = free_space / 2, bottom = free_space / 2 }

                                else
                                    { childMargin | start = free_space / 2, end = free_space / 2 }
                        }

                    else if Node.crossMarginStart direction child.node == Dimension.Auto then
                        { child
                            | margin =
                                if is_row then
                                    { childMargin | top = free_space }

                                else
                                    { childMargin | start = free_space }
                        }

                    else if Node.crossMarginEnd direction child.node == Dimension.Auto then
                        { child
                            | margin =
                                if is_row then
                                    { childMargin | bottom = free_space }

                                else
                                    { childMargin | end = free_space }
                        }

                    else
                        let
                            offset_cross =
                                case Node.align { node = child.node, parent = Node node } of
                                    AlignSelf.Auto ->
                                        0.0

                                    --  Should never happen
                                    AlignSelf.FlexStart ->
                                        if is_wrap_reverse then
                                            free_space

                                        else
                                            0.0

                                    AlignSelf.FlexEnd ->
                                        if is_wrap_reverse then
                                            0.0

                                        else
                                            free_space

                                    AlignSelf.Center ->
                                        free_space / 2.0

                                    AlignSelf.Baseline ->
                                        if is_row then
                                            max_baseline - child.baseline

                                        else
                                        -- baseline alignment only makes sense if the direction is row
                                        -- we treat it as flex-start alignment in columns.
                                        if
                                            is_wrap_reverse
                                        then
                                            free_space

                                        else
                                            0.0

                                    AlignSelf.Stretch ->
                                        if is_wrap_reverse then
                                            free_space

                                        else
                                            0.0
                        in
                        { child | offset_cross = offset_cross }
            in
            flexLines8
                |> List.map
                    (\line ->
                        let
                            line_cross_size =
                                line.cross_size

                            max_baseline =
                                line.items
                                    |> List.map .baseline
                                    |> List.foldl max 0
                        in
                        { line | items = List.map (mapper line_cross_size max_baseline) line.items }
                    )

        total_cross_size =
            flexLines9
                |> List.map .cross_size
                |> List.sum

        newerContainerSize =
            newContainerSize |> Size.setCrossAxis direction (Size.crossAxis direction node_size |> Maybe.withDefault (total_cross_size + Rectangle.crossAxis direction paddingBorder))

        newerInnerContainerSize =
            newInnerContainerSize |> Size.setCrossAxis direction (Size.crossAxis direction newerContainerSize - Rectangle.crossAxis direction paddingBorder)
    in
    -- We have the container size. If our caller does not care about performing
    -- layout we are done now.
    if not perform_layout then
        let
            result =
                { size = newerContainerSize, children = [] }
        in
        -- ( Node { node | cache = LayoutCache node_size parent_size perform_layout result }, result )
        Debug.todo ""

    else
        Debug.todo ""


type alias LayoutInput =
    { direction : FlexDirection.FlexDirection
    , inner_container_size : Size Float
    , total_cross_size : Float
    , is_wrap_reverse : Bool
    }


calculateLayout : LayoutInput -> Node -> List FlexLine -> List FlexLine
calculateLayout ({ inner_container_size, total_cross_size, direction, is_wrap_reverse } as state) (Node node) flex_lines =
    let
        free_space =
            Size.crossAxis direction inner_container_size - total_cross_size

        num_lines =
            List.length flex_lines

        align_line : Int -> FlexLine -> FlexLine
        align_line i line =
            let
                is_first =
                    i == 0

                offset_cross =
                    case node.align_content of
                        AlignContent.FlexStart ->
                            if is_first && is_wrap_reverse then
                                free_space

                            else
                                0.0

                        AlignContent.FlexEnd ->
                            if is_first && not is_wrap_reverse then
                                free_space

                            else
                                0.0

                        AlignContent.Center ->
                            if is_first then
                                free_space / 2.0

                            else
                                0.0

                        AlignContent.Stretch ->
                            0

                        AlignContent.SpaceBetween ->
                            if is_first then
                                0.0

                            else
                                free_space / toFloat (num_lines - 1)

                        AlignContent.SpaceAround ->
                            if is_first then
                                (free_space / toFloat num_lines) / 2.0

                            else
                                free_space / toFloat num_lines
            in
            { line | offset_cross = offset_cross }
    in
    if is_wrap_reverse then
        flex_lines
            |> List.reverse
            |> List.indexedMap align_line

    else
        flex_lines
            |> List.indexedMap align_line


finalLayoutPass :
    { container_size : Size Float
    , direction : FlexDirection.FlexDirection
    , is_row : Bool
    , padding_border : Rectangle Float
    , is_wrap_reverse : Bool
    }
    -> Node
    -> List FlexLine
    -> List LayoutNode
finalLayoutPass { container_size, direction, is_row, padding_border, is_wrap_reverse } (Node node) flex_lines =
    let
        is_column =
            not is_row

        layout_line : FlexLine -> { total_offset_cross : Float, lines : List (List LayoutNode) } -> { total_offset_cross : Float, lines : List (List LayoutNode) }
        layout_line line { total_offset_cross, lines } =
            let
                line_offset_cross =
                    line.offset_cross

                layoutItemConfig =
                    { children = []
                    , total_offset_main = Rectangle.mainAxisStart direction padding_border
                    , total_offset_cross = total_offset_cross
                    , container_size = container_size
                    , direction = direction
                    , is_row = is_row
                    }

                newItems =
                    if FlexDirection.isReverse direction then
                        line.items
                            |> List.reverse
                            |> List.foldl (layout_item (Node node)) layoutItemConfig

                    else
                        line.items
                            |> List.foldl (layout_item (Node node)) layoutItemConfig
            in
            { total_offset_cross = total_offset_cross + line_offset_cross + line.cross_size
            , lines = lines ++ [ newItems.children ]
            }
    in
    if is_wrap_reverse then
        flex_lines
            |> List.reverse
            |> List.foldl layout_line { total_offset_cross = Rectangle.crossAxisStart direction padding_border, lines = [] }
            |> .lines
            |> List.reverse
            |> List.concat

    else
        flex_lines
            |> List.foldl layout_line { total_offset_cross = Rectangle.crossAxisStart direction padding_border, lines = [] }
            |> .lines
            |> List.concat


layout_item :
    Node
    -> FlexItem
    -> { children : List LayoutNode, total_offset_main : Float, total_offset_cross : Float, container_size : Size Float, direction : FlexDirection, is_row : Bool }
    -> { children : List LayoutNode, total_offset_main : Float, total_offset_cross : Float, container_size : Size Float, direction : FlexDirection, is_row : Bool }
layout_item (Node node) child ({ children, total_offset_main, container_size, total_offset_cross, direction, is_row } as state) =
    let
        is_column =
            not is_row

        result =
            compute_internal child.node (child.target_size |> Size.map Just) (container_size |> Size.map Just) True

        offset_main =
            total_offset_main
                + child.offset_main
                + Rectangle.mainAxisStart direction child.margin
                + (Maybe.withDefault 0 (Rectangle.mainAxisStart direction child.position) - Maybe.withDefault 0 (Rectangle.mainAxisEnd direction child.position))

        offset_cross =
            total_offset_cross
                + child.offset_cross
                + Rectangle.crossAxisStart direction child.margin
                + (Maybe.withDefault 0 (Rectangle.crossAxisStart direction child.position) - Maybe.withDefault 0 (Rectangle.crossAxisEnd direction child.position))

        newChild =
            LayoutNode
                { order =
                    position (\n -> n == child.node) node.children
                        |> Maybe.withDefault 0
                , size = result.size
                , location =
                    { x =
                        if is_row then
                            offset_main

                        else
                            offset_cross
                    , y =
                        if is_column then
                            offset_main

                        else
                            offset_cross
                    }
                , children = result.children
                }
    in
    { state
        | total_offset_main = total_offset_main + Rectangle.mainAxis direction child.margin + Size.mainAxis direction result.size
        , children = children ++ [ newChild ]
    }


unwrap (Node n) =
    n


partitionFrozenHelper remaining frozen unfrozen =
    case remaining of
        [] ->
            { frozen = List.reverse frozen, unfrozen = List.reverse unfrozen }

        x :: xs ->
            if x.frozen then
                partitionFrozenHelper xs (x :: frozen) unfrozen

            else
                partitionFrozenHelper xs frozen (x :: unfrozen)


maybeMin : Maybe comparable -> Maybe comparable -> Maybe comparable
maybeMin rhs self =
    case self of
        Just val ->
            case rhs of
                Just other ->
                    Just (min val other)

                Nothing ->
                    self

        Nothing ->
            Nothing


maybeMax : Maybe comparable -> Maybe comparable -> Maybe comparable
maybeMax self rhs =
    case self of
        Just val ->
            case rhs of
                Just other ->
                    Just (max val other)

                Nothing ->
                    self

        Nothing ->
            Nothing


isDefined : Number -> Bool
isDefined =
    not << isUndefined


isUndefined : Number -> Bool
isUndefined n =
    case n of
        Just _ ->
            False

        Nothing ->
            True


isNothing : Maybe a -> Bool
isNothing n =
    case n of
        Just _ ->
            False

        Nothing ->
            True


orElse other first =
    case first of
        Just v ->
            Just v

        _ ->
            other


subtract : Number -> Float -> Number
subtract n x =
    Maybe.map (\w -> w - x) n


{-| Returns true if the number is neither zero, infinite, subnormal, or NaN.
-}
isNormalFloat : Float -> Bool
isNormalFloat f =
    not (isNaN f) && f /= 0 && not (isInfinite f)


position : (a -> Bool) -> List a -> Maybe Int
position _ _ =
    Debug.todo ""
