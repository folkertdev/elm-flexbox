module Algorithm exposing (FlexItem, compute, isNormalFloat, orElse)

import AlignContent
import AlignSelf
import Dimension
import FlexDirection exposing (FlexDirection)
import JustifyContent
import Node exposing (ComputeResult, FlexWrap(..), LayoutCache, LayoutNode(..), Node(..), Point)
import Rectangle exposing (Rectangle)
import Size exposing (Size)


log message value =
    if False then
        Debug.log message value

    else
        value


log_ x y =
    y


{-| Smallest difference between two floating point numbers that will result in them not being the same.
-}
epsilon : Float
epsilon =
    2 ^ -52


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
                            { width = Dimension.resolveNumber size.width root.size.width
                            , height = Dimension.resolveNumber size.height root.size.height
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
                                Dimension.resolveNumber size.width root.min_size.width

                            lowerBound =
                                Dimension.resolveNumber size.width root.max_size.width
                        in
                        initial
                            |> Just
                            |> maybeMax upperBound
                            |> maybeMin lowerBound
                            |> Maybe.withDefault 0
                            |> Just
                    , height =
                        let
                            initial : Float
                            initial =
                                first_pass.size.height

                            upperBound =
                                Dimension.resolveNumber size.height root.min_size.height

                            lowerBound =
                                Dimension.resolveNumber size.height root.max_size.height
                        in
                        initial
                            |> Just
                            |> maybeMax upperBound
                            |> maybeMin lowerBound
                            |> Maybe.withDefault 0
                            |> Just
                    }
                    size
                    True

            else
                let
                    node_size =
                        { width = Dimension.resolveNumber size.width root.size.width
                        , height = Dimension.resolveNumber size.height root.size.height
                        }

                    _ =
                        ( size.width, root.size.width )
                            |> log "=============> "
                in
                compute_internal (Node root)
                    node_size
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
            { width = toFloat <| round (x + layout.size.width) - round x
            , height = toFloat <| round (y + layout.size.height) - round y
            }
    in
    LayoutNode
        { order = layout.order
        , location = location
        , size = size
        , children =
            List.map (\child -> round_layout child x y) layout.children
        }


type alias State =
    { available_space : Size Number
    , border : Rectangle Float
    , container_size : Size Float
    , direction : FlexDirection
    , inner_container_size : { height : Float, width : Float }
    , is_column : Bool
    , is_row : Bool
    , is_wrap_reverse : Bool
    , margin : Rectangle Float
    , node_inner_size : Size Number
    , padding : Rectangle Float
    , padding_border : Rectangle Float
    , node_size : Size Number
    , parent_size : Size Number
    }


initializeState : Node -> Size Number -> Size Number -> State
initializeState (Node node) node_size parent_size =
    let
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

        padding_border : Rectangle Float
        padding_border =
            { start = padding.start + border.start
            , end = padding.end + border.end
            , top = padding.top + border.top
            , bottom = padding.bottom + border.bottom
            }

        node_inner_size : Size Number
        node_inner_size =
            { width = Maybe.map (\w -> w - Rectangle.horizontal padding_border) node_size.width
            , height = Maybe.map (\h -> h - Rectangle.vertical padding_border) node_size.height
            }

        container_size =
            { width = 0, height = 0 }

        inner_container_size =
            { width = 0, height = 0 }

        {-
           9.2. Line Length Determination

            1 Generate anonymous flex items as described in §4 Flex Items.

            2 Determine the available main and cross space for the flex items.
              For each dimension, if that dimension of the flex container’s content box
              is a definite size, use that; if that dimension of the flex container is
              being sized under a min or max-content constraint, the available space in
              that dimension is that constraint; otherwise, subtract the flex container’s
              margin, border, and padding from the space available to the flex container
              in that dimension and use that value. This might result in an infinite value.
        -}
        available_space : Size Number
        available_space =
            { width =
                subtract (node_size.width |> orElse (subtract parent_size.width (Rectangle.horizontal margin))) (Rectangle.horizontal padding_border)
            , height =
                subtract (node_size.height |> orElse (subtract parent_size.height (Rectangle.vertical margin))) (Rectangle.vertical padding_border)
            }
    in
    { direction = direction
    , container_size = container_size
    , inner_container_size = inner_container_size
    , available_space = available_space
    , node_inner_size = node_inner_size
    , is_row = is_row
    , is_column = is_column
    , is_wrap_reverse = is_wrap_reverse
    , margin = margin
    , padding = padding
    , border = border
    , padding_border = padding_border
    , node_size = node_size
    , parent_size = parent_size
    }



-- Flex Items


flex_items_1 : State -> Node -> List FlexItem
flex_items_1 { node_inner_size } (Node node) =
    node.children
        |> List.filter (\(Node child) -> child.position_type /= Node.Absolute)
        |> List.filter (\(Node child) -> child.display /= Node.None)
        |> List.map
            (\(Node child) ->
                { node = Node child
                , size =
                    { width = Dimension.resolveNumber node_inner_size.width child.size.width
                    , height = Dimension.resolveNumber node_inner_size.height child.size.height
                    }
                , min_size =
                    { width = Dimension.resolveNumber node_inner_size.width child.min_size.width
                    , height = Dimension.resolveNumber node_inner_size.height child.min_size.height
                    }
                , max_size =
                    { width = Dimension.resolveNumber node_inner_size.width child.max_size.width
                    , height = Dimension.resolveNumber node_inner_size.height child.max_size.height
                    }
                , position = Rectangle.map (\p -> Dimension.resolveNumber node_inner_size.width p) child.position
                , margin = Rectangle.map (\p -> Dimension.resolveNumber node_inner_size.width p |> Maybe.withDefault 0) child.margin
                , padding = Rectangle.map (\p -> Dimension.resolveNumber node_inner_size.width p |> Maybe.withDefault 0) child.padding
                , border = Rectangle.map (\p -> Dimension.resolveNumber node_inner_size.width p |> Maybe.withDefault 0) child.border
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


flex_items_2 : State -> Node -> List FlexItem -> List FlexItem
flex_items_2 { node_inner_size, direction, node_size, is_column, available_space, is_row } (Node node) flex_items =
    flex_items
        |> List.map
            (\child ->
                let
                    (Node childNode) =
                        child.node

                    --  A. If the item has a definite used flex basis, that’s the flex base size.
                    maybe_flex_basis : Maybe Float
                    maybe_flex_basis =
                        Dimension.resolveNumber (Size.mainAxis direction node_inner_size) childNode.flex_basis

                    attempt =
                        case maybe_flex_basis of
                            Just flex_basis ->
                                Just { child | flex_basis = flex_basis }

                            Nothing ->
                                -- B. If the flex item has an intrinsic aspect ratio,
                                --    a used flex basis of content, and a definite cross size,
                                --    then the flex base size is calculated from its inner
                                --    cross size and the flex item’s intrinsic aspect ratio.
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
                        -- C. If the used flex basis is content or depends on its available space,
                        --    and the flex container is being sized under a min-content or max-content
                        --    constraint (e.g. when performing automatic table layout [CSS21]),
                        --    size the item under that constraint. The flex base size is the item’s
                        --    resulting main size.
                        -- TODO - Probably need to cover this case in future
                        ----D. Otherwise, if the used flex basis is content or depends on its
                        --    available space, the available main size is infinite, and the flex item’s
                        --    inline axis is parallel to the main axis, lay the item out using the rules
                        --    for a box in an orthogonal flow [CSS3-WRITING-MODES]. The flex base size
                        --    is the item’s max-content main size.
                        -- TODO - Probably need to cover this case in future
                        -- E. Otherwise, size the item into the available space using its used flex basis
                        --    in place of its main size, treating a value of content as max-content.
                        --    If a cross size is needed to determine the main size (e.g. when the
                        --    flex item’s main size is in its block axis) and the flex item’s cross size
                        --    is auto and not definite, in this calculation use fit-content as the
                        --    flex item’s cross size. The flex base size is the item’s resulting main size.
                        let
                            width : Number
                            width =
                                if not (isDefined child.size.width) && Node.align { node = Node childNode, parent = Node node } == AlignSelf.Stretch && is_column then
                                    available_space.width

                                else
                                    child.size.width

                            height : Number
                            height =
                                if not (isDefined child.size.height) && Node.align { node = Node childNode, parent = Node node } == AlignSelf.Stretch && is_row then
                                    available_space.height

                                else
                                    child.size.height

                            flex_basis : Float
                            flex_basis =
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
                                    available_space
                                    False
                                    |> .size
                                    |> Size.mainAxis direction
                                    |> Just
                                    |> maybeMax (Size.mainAxis direction child.min_size)
                                    |> maybeMin (Size.mainAxis direction child.max_size)
                                    -- TODO v is this correct
                                    |> Maybe.withDefault 0
                        in
                        { child | flex_basis = flex_basis }
            )


flex_items_3 : State -> List FlexItem -> List FlexItem
flex_items_3 ({ is_row, direction, available_space } as state) flex_items =
    let
        -- The hypothetical main size is the item’s flex base size clamped according to its
        -- used min and max main sizes (and flooring the content box size at zero).
        mapper : FlexItem -> FlexItem
        mapper child =
            let
                -- TODO - not really spec abiding but needs to be done somewhere. probably somewhere else though.
                -- The following logic was developed not from the spec but by trail and error looking into how
                -- webkit handled various scenarios. Can probably be solved better by passing in
                -- min-content max-content constraints from the top
                min_main : Maybe Float
                min_main =
                    if is_row then
                        compute_internal child.node Size.undefined available_space False
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
                            (child.flex_basis
                                |> Just
                                |> maybeMax min_main
                                |> maybeMin (Size.mainAxis direction child.max_size)
                            )
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
    List.map mapper flex_items



-- Flex Lines


{-|

     9.3. Main Size Determination

     5. Collect flex items into flex lines:
        - If the flex container is single-line, collect all the flex items into
          a single flex line.
        - Otherwise, starting from the first uncollected item, collect consecutive
          items one by one until the first time that the next collected item would
          not fit into the flex container’s inner main size (or until a forced break
          is encountered, see §10 Fragmenting Flex Layout). If the very first
          uncollected item wouldn’t fit, collect just it into the line.

          For this step, the size of a flex item is its outer hypothetical main size. (Note: This can be negative.)
          Repeat until all flex items have been collected into flex lines

          Note that the "collect as many" line will collect zero-sized flex items onto
          the end of the previous line even if the last non-zero item exactly "filled up" the line.

-}
flex_lines_1 : State -> Node -> List FlexItem -> List FlexLine
flex_lines_1 { direction, available_space } (Node node) flex_items =
    if node.flex_wrap == NoWrap then
        [ { items = flex_items, cross_size = 0, offset_cross = 0 } ]

    else
        let
            initialLine =
                { items = [], cross_size = 0, offset_cross = 0 }

            folder child ( line, lines, line_length ) =
                case Size.mainAxis direction available_space of
                    Just main ->
                        let
                            new_line_length =
                                line_length + Size.mainAxis direction child.hypothetical_outer_size
                        in
                        if new_line_length > main && not (List.isEmpty line.items) then
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

            result =
                flex_items
                    |> List.foldl folder ( initialLine, [], 0 )
                    |> (\( l, ls, _ ) -> ls ++ [ l ])
        in
        result


{-|

1.  Resolve the flexible lengths of all the flex items to find their used main size.
    See §9.7 Resolving Flexible Lengths.

9.7. Resolving Flexible Lengths

-}
flex_lines_2 : State -> Node -> List FlexLine -> List FlexLine
flex_lines_2 ({ direction, node_inner_size, is_row, available_space } as state) (Node node) flex_lines =
    let
        mapper : FlexLine -> FlexLine
        mapper line =
            let
                -- 1. Determine the used flex factor. Sum the outer hypothetical main sizes of all
                --    items on the line. If the sum is less than the flex container’s inner main size,
                --    use the flex grow factor for the rest of this algorithm; otherwise, use the
                --    flex shrink factor.
                used_flex_factor =
                    sumBy (Size.mainAxis direction << .hypothetical_outer_size) line.items

                growing =
                    used_flex_factor < (Size.mainAxis direction node_inner_size |> Maybe.withDefault 0)

                shrinking =
                    not growing

                -- 2. Size inflexible items. Freeze, setting its target main size to its hypothetical main size
                --    - Any item that has a flex factor of zero
                --    - If using the flex grow factor: any item that has a flex base size
                --      greater than its hypothetical main size
                --    - If using the flex shrink factor: any item that has a flex base size
                --      smaller than its hypothetical main size
                sizeInflexibleItem : FlexItem -> FlexItem
                sizeInflexibleItem child =
                    let
                        (Node childNode) =
                            child.node

                        freeze =
                            (childNode.flex_grow == 0.0 && childNode.flex_shrink == 0.0)
                                || (growing && child.flex_basis > Size.mainAxis direction child.hypothetical_inner_size)
                                || (shrinking && child.flex_basis < Size.mainAxis direction child.hypothetical_inner_size)

                        -- TODO this should really only be set inside the if-statement below but
                        -- that causes the target_main_size to never be set for some items
                        outer_target_size =
                            child.outer_target_size
                                |> Size.setMainAxis direction (Size.mainAxis direction child.target_size + Rectangle.mainAxis direction child.margin)
                    in
                    if isUndefined (Size.mainAxis direction node_inner_size) && is_row then
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
                                            available_space
                                            False
                                            |> .size
                                            |> Size.mainAxis direction
                                            |> log "--> main axis value will be"
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
                            | target_size =
                                Size.setMainAxis direction (Size.mainAxis direction child.hypothetical_inner_size) child.target_size
                            , frozen = freeze
                            , outer_target_size = outer_target_size
                        }

                newLine =
                    { line | items = List.map sizeInflexibleItem line.items }

                -- 3. Calculate initial free space. Sum the outer sizes of all items on the line,
                --    and subtract this from the flex container’s inner main size. For frozen items,
                --    use their outer target main size; for other items, use their outer flex base size.
                used_space =
                    newLine.items
                        |> sumBy
                            (\child ->
                                Rectangle.mainAxis direction child.margin
                                    + (if child.frozen then
                                        Size.mainAxis direction child.target_size

                                       else
                                        child.flex_basis
                                      )
                            )

                initial_free_space =
                    subtract (Size.mainAxis direction node_inner_size) used_space |> Maybe.withDefault 0

                arguments =
                    { initial_free_space = initial_free_space
                    , growing = growing
                    , shrinking = shrinking
                    }
            in
            -- 4. Loop
            { newLine | items = loop state arguments newLine.items }
    in
    List.map mapper flex_lines
        |> log_ "$$$$$ flex_lines_2 final mainAxis value"


loop :
    { a | direction : FlexDirection, node_inner_size : Size Number, is_row : Bool, available_space : Size Number }
    -> { initial_free_space : Float, growing : Bool, shrinking : Bool }
    -> List FlexItem
    -> List FlexItem
loop ({ direction, node_inner_size, is_row, available_space } as state) ({ initial_free_space, growing, shrinking } as arguments) items =
    -- a. Check for flexible items. If all the flex items on the line are frozen,
    --    free space has been distributed; exit this loop.
    let
        { frozen, unfrozen } =
            partitionFrozenHelper items [] []
    in
    if List.isEmpty unfrozen then
        frozen ++ unfrozen

    else
        -- b. Calculate the remaining free space as for initial free space, above.
        --    If the sum of the unfrozen flex items’ flex factors is less than one,
        --    multiply the initial free space by this sum. If the magnitude of this
        --    value is less than the magnitude of the remaining free space, use this
        --    as the remaining free space.
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
                unfrozen
                    |> List.map (.node >> unwrap >> .flex_shrink)
                    |> List.sum

            free_space =
                Maybe.withDefault 0 <|
                    if growing && sum_flex_grow < 1.0 then
                        (Just <| initial_free_space * sum_flex_grow)
                            |> maybeMin (subtract (Size.mainAxis direction node_inner_size) used_space)

                    else if shrinking && sum_flex_shrink < 1.0 then
                        (Just <| initial_free_space * sum_flex_shrink)
                            |> maybeMax (subtract (Size.mainAxis direction node_inner_size) used_space)

                    else
                        subtract (Size.mainAxis direction node_inner_size) used_space

            -- c. Distribute free space proportional to the flex factors.
            --    - If the remaining free space is zero
            --        Do Nothing
            --    - If using the flex grow factor
            --        Find the ratio of the item’s flex grow factor to the sum of the
            --        flex grow factors of all unfrozen items on the line. Set the item’s
            --        target main size to its flex base size plus a fraction of the remaining
            --        free space proportional to the ratio.
            --    - If using the flex shrink factor
            --        For every unfrozen item on the line, multiply its flex shrink factor by
            --        its inner flex base size, and note this as its scaled flex shrink factor.
            --        Find the ratio of the item’s scaled flex shrink factor to the sum of the
            --        scaled flex shrink factors of all unfrozen items on the line. Set the item’s
            --        target main size to its flex base size minus a fraction of the absolute value
            --        of the remaining free space proportional to the ratio. Note this may result
            --        in a negative inner main size; it will be corrected in the next step.
            --    - Otherwise
            --        Do Nothing
            newUnfrozen =
                if isNormalFloat free_space then
                    if growing && sum_flex_grow > 0 then
                        let
                            mapper child =
                                { child
                                    | target_size =
                                        let
                                            new =
                                                child.flex_basis + free_space * ((child.node |> unwrap |> .flex_grow) / sum_flex_grow)
                                        in
                                        child.target_size
                                            |> Size.setMainAxis direction new
                                }
                        in
                        List.map mapper unfrozen

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

                                            target_size =
                                                child.target_size
                                                    |> Size.setMainAxis direction (child.flex_basis + free_space * (scaled_shrink_factor / sum_scaled_shrink_factor))
                                        in
                                        { child | target_size = target_size }
                                    )

                        else
                            unfrozen

                    else
                        unfrozen

                else
                    unfrozen

            -- d. Fix min/max violations. Clamp each non-frozen item’s target main size by its
            --    used min and max main sizes and floor its content-box size at zero. If the
            --    item’s target main size was made smaller by this, it’s a max violation.
            --    If the item’s target main size was made larger by this, it’s a min violation.
            ( total_violation, newerUnfrozen ) =
                let
                    folder child ( violation, accum ) =
                        let
                            min_main =
                                if is_row && isNothing (child.node |> unwrap |> .measure) then
                                    compute_internal child.node Size.undefined available_space False
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
                                    |> Size.setMainAxis direction (Size.mainAxis direction target_size + Rectangle.mainAxis direction child.margin)
                        in
                        ( violation + childViolation
                        , { child
                            | violation = childViolation
                            , target_size = target_size
                            , outer_target_size = outer_target_size
                          }
                            :: accum
                        )
                in
                List.foldl folder ( 0, [] ) newUnfrozen
                    |> (\( v, a ) -> ( v, List.reverse a ))

            -- e. Freeze over-flexed items. The total violation is the sum of the adjustments
            --    from the previous step ∑(clamped size - unclamped size). If the total violation is:
            --    - Zero
            --        Freeze all items.
            --    - Positive
            --        Freeze all the items with min violations.
            --    - Negative
            --        Freeze all the items with max violations.
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
        loop state arguments (frozen ++ newererUnfrozen)


{-|

     7. Determine the hypothetical cross size of each item by performing layout with the
        used main size and the available space, treating auto as fit-content.

-}
flex_lines_3 : State -> Node -> List FlexLine -> List FlexLine
flex_lines_3 { direction, is_row, container_size, available_space } (Node node) flex_lines =
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
                            (let
                                inner =
                                    { width =
                                        if is_row then
                                            Just child.target_size.width

                                        else
                                            child_cross
                                    , height =
                                        if is_row then
                                            child_cross

                                        else
                                            Just child.target_size.height
                                    }

                                outer =
                                    { width =
                                        if is_row then
                                            Just (Size.mainAxis direction container_size)

                                        else
                                            available_space.width
                                    , height =
                                        if is_row then
                                            available_space.height

                                        else
                                            Just (Size.mainAxis direction container_size)
                                    }
                             in
                             compute_internal child.node inner outer False
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
            { child
                | hypothetical_inner_size = hypothetical_inner_size
                , hypothetical_outer_size = hypothetical_outer_size
            }
    in
    List.map (\line -> { line | items = List.map mapper line.items }) flex_lines


flex_lines_4 : State -> Node -> List FlexLine -> List FlexLine
flex_lines_4 { direction, is_row, container_size, available_space, node_size } (Node node) flex_lines =
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
                                Just container_size.width

                            else
                                node_size.width
                        , height =
                            if is_row then
                                node_size.height

                            else
                                Just container_size.height
                        }
                        True

                _ =
                    result
                        |> log_ "$$$$$$ return value in flex_lines_4 "

                baseline =
                    calculate_baseline
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
    List.map (\line -> { line | items = List.map mapper line.items }) flex_lines


{-|

     8. Calculate the cross size of each flex line.
        If the flex container is single-line and has a definite cross size, the cross size
        of the flex line is the flex container’s inner cross size. Otherwise, for each flex line:

        If the flex container is single-line, then clamp the line’s cross-size to be within
        the container’s computed min and max cross sizes. Note that if CSS 2.1’s definition
        of min/max-width/height applied more generally, this behavior would fall out automatically.

-}
flex_lines_5 : State -> Node -> List FlexLine -> List FlexLine
flex_lines_5 { direction, is_row, container_size, available_space, node_size, padding_border } (Node node) flex_lines =
    let
        mapper : FlexLine -> FlexLine
        mapper line =
            let
                --    1. Collect all the flex items whose inline-axis is parallel to the main-axis, whose
                --       align-self is baseline, and whose cross-axis margins are both non-auto. Find the
                --       largest of the distances between each item’s baseline and its hypothetical outer
                --       cross-start edge, and the largest of the distances between each item’s baseline
                --       and its hypothetical outer cross-end edge, and sum these two values.
                --    2. Among all the items not collected by the previous step, find the largest
                --       outer hypothetical cross size.
                --    3. The used cross-size of the flex line is the largest of the numbers found in the
                --       previous two steps and zero.
                max_baseline : Float
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
    case flex_lines of
        [ x ] ->
            if isDefined (Size.crossAxis direction node_size) then
                let
                    cross_size =
                        subtract (Size.crossAxis direction node_size) (Rectangle.crossAxis direction padding_border)
                            |> Maybe.withDefault 0
                in
                [ { x | cross_size = cross_size } ]

            else
                List.map mapper flex_lines

        _ ->
            List.map mapper flex_lines


flex_lines_6 : State -> Node -> List FlexLine -> List FlexLine
flex_lines_6 { direction, node_size, padding_border } (Node node) flex_lines =
    -- 9. Handle 'align-content: stretch'. If the flex container has a definite cross size,
    --    align-content is stretch, and the sum of the flex lines' cross sizes is less than
    --    the flex container’s inner cross size, increase the cross size of each flex line
    --    by equal amounts such that the sum of their cross sizes exactly equals the
    --    flex container’s inner cross size.
    if node.align_content == AlignContent.Stretch && isDefined (Size.crossAxis direction node_size) then
        let
            total_cross =
                sumBy .cross_size flex_lines

            inner_cross =
                subtract (Size.crossAxis direction node_size) (Rectangle.crossAxis direction padding_border)
                    |> Maybe.withDefault 0
        in
        if total_cross < inner_cross then
            let
                remaining =
                    inner_cross - total_cross

                addition =
                    remaining / toFloat (List.length flex_lines)
            in
            List.map
                (\line ->
                    { line
                        | cross_size =
                            let
                                result =
                                    line.cross_size + addition
                            in
                            result
                    }
                )
                flex_lines

        else
            flex_lines

    else
        flex_lines



{-
   10. Collapse visibility:collapse items. If any flex items have visibility: collapse,
       note the cross size of the line they’re in as the item’s strut size, and restart
       layout from the beginning.

       In this second layout round, when collecting items into lines, treat the collapsed
       items as having zero main size. For the rest of the algorithm following that step,
       ignore the collapsed items entirely (as if they were display:none) except that after
       calculating the cross size of the lines, if any line’s cross size is less than the
       largest strut size among all the collapsed items in the line, set its cross size to
       that strut size.

       Skip this step in the second layout round.

   TODO implement once (if ever) we support visibility:collapse

-}


{-|

     11. Determine the used cross size of each flex item. If a flex item has align-self: stretch,
         its computed cross size property is auto, and neither of its cross-axis margins are auto,
         the used outer cross size is the used cross size of its flex line, clamped according to
         the item’s used min and max cross sizes. Otherwise, the used cross size is the item’s
         hypothetical cross size.

         If the flex item has align-self: stretch, redo layout for its contents, treating this
         used size as its definite cross size so that percentage-sized children can be resolved.

         Note that this step does not affect the main size of the flex item, even if it has an
         intrinsic aspect ratio.

-}
flex_lines_7 : State -> Node -> List FlexLine -> List FlexLine
flex_lines_7 { direction, node_size } (Node node) flex_lines =
    let
        mapper : Float -> FlexItem -> FlexItem
        mapper line_cross_size child =
            let
                predicate =
                    (Node.align { node = child.node, parent = Node node } == AlignSelf.Stretch)
                        && (Node.crossMarginStart direction child.node /= Dimension.Auto)
                        && (Node.crossMarginEnd direction child.node /= Dimension.Auto)
                        && (Node.crossSize direction child.node == Dimension.Auto)

                target_size =
                    child.target_size
                        |> Size.setCrossAxis direction
                            (if predicate then
                                subtract (Just line_cross_size) (Rectangle.crossAxis direction child.margin)
                                    |> maybeMax (Size.crossAxis direction child.min_size)
                                    |> maybeMin (Size.crossAxis direction child.max_size)
                                    |> Maybe.withDefault 0

                             else
                                Size.crossAxis direction child.hypothetical_inner_size
                            )

                outer_target_size =
                    child.outer_target_size
                        |> Size.setCrossAxis direction (Size.crossAxis direction target_size + Rectangle.crossAxis direction child.margin)
            in
            { child
                | target_size = target_size
                , outer_target_size = outer_target_size
            }
    in
    List.map (\line -> { line | items = List.map (mapper line.cross_size) line.items }) flex_lines


{-|

     9.5. Main-Axis Alignment

     12. Distribute any remaining free space. For each flex line:
         1. If the remaining free space is positive and at least one main-axis margin on this
            line is auto, distribute the free space equally among these margins. Otherwise,
            set all auto margins to zero.
         2. Align the items along the main-axis per justify-content.

-}
flex_lines_8 : State -> Node -> List FlexLine -> List FlexLine
flex_lines_8 { direction, node_size, inner_container_size, is_row, margin } (Node node) flex_lines =
    let
        mapper : FlexLine -> FlexLine
        mapper line =
            let
                used_space =
                    sumBy (\child -> Size.mainAxis direction child.outer_target_size) line.items

                free_space =
                    Size.mainAxis direction inner_container_size - used_space

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
                                            childMargin
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
                                            childMargin
                                in
                                { child | margin = newMargin }
                        in
                        List.map (h1 >> h2) line.items

                    else
                        let
                            num_items : Int
                            num_items =
                                List.length line.items

                            layout_reverse : Bool
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
    List.map mapper flex_lines


{-|

     9.6. Cross-Axis Alignment

     13. Resolve cross-axis auto margins. If a flex item has auto cross-axis margins:
         - If its outer cross size (treating those auto margins as zero) is less than the
           cross size of its flex line, distribute the difference in those sizes equally
           to the auto margins.
         - Otherwise, if the block-start or inline-start margin (whichever is in the cross axis)
           is auto, set it to zero. Set the opposite margin so that the outer cross size of the
           item equals the cross size of its flex line.

-}
flex_lines_9 : State -> Node -> List FlexLine -> List FlexLine
flex_lines_9 { direction, available_space, margin, is_row, is_wrap_reverse } (Node node) flex_lines =
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
                -- 14. Align all flex items along the cross-axis per align-self, if neither of the item’s
                --     cross-axis margins are auto.
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
    flex_lines
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


compute_internal : Node -> Size Number -> Size Number -> Bool -> ComputeResult
compute_internal (Node node) node_size parent_size perform_layout =
    let
        state =
            initializeState (Node node) node_size parent_size

        { direction, container_size, inner_container_size, available_space, node_inner_size, is_row, is_column, is_wrap_reverse, margin, padding, border, padding_border } =
            state

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

        helper2 () =
            if List.isEmpty node.children then
                if isDefined node_size.width && isDefined node_size.height then
                    -- Just ( Node node, { size = Size.map (Maybe.withDefault 0) node_size, children = [] } )
                    Just { size = Size.map (Maybe.withDefault 0) node_size, children = [] }

                else
                    case node.measure of
                        Just measure ->
                            let
                                measured =
                                    measure node_size

                                -- TODO this is a hack not based on the reference. It makes sense though to limit to the parent width/height right?
                                bounded =
                                    { width =
                                        measured.width
                                            |> Just
                                            |> maybeMin parent_size.width
                                            |> Maybe.withDefault 0
                                    , height =
                                        measured.height
                                            |> Just
                                            |> maybeMin parent_size.height
                                            |> Maybe.withDefault 0
                                    }

                                result =
                                    { size = measured, children = [] }
                            in
                            -- ( Node { node | layout_cache = Just { nodeSize = node_size, parentSize = parent_size, performLayout = perform_layout, result = result } } , result)
                            Just result

                        _ ->
                            Nothing

            else
                Nothing
    in
    case helper of
        Just result ->
            result

        Nothing ->
            case helper2 () of
                Just result ->
                    result

                Nothing ->
                    let
                        flex_lines_phase_1 : List FlexLine
                        flex_lines_phase_1 =
                            let
                                flex_items : List FlexItem
                                flex_items =
                                    flex_items_1 state (Node node)
                                        |> flex_items_2 state (Node node)
                                        |> flex_items_3 state
                            in
                            flex_items
                                |> flex_lines_1 state (Node node)
                                |> flex_lines_2 state (Node node)

                        newState : State
                        newState =
                            let
                                -- Not part of the spec from what i can see but seems correct
                                newContainerSize =
                                    container_size
                                        |> Size.setMainAxis direction
                                            (Size.mainAxis direction node_size
                                                |> Maybe.withDefault
                                                    (let
                                                        longest_line =
                                                            flex_lines_phase_1
                                                                |> List.foldl
                                                                    (\line accum ->
                                                                        let
                                                                            length =
                                                                                line.items |> List.map (.outer_target_size >> Size.mainAxis direction) |> List.sum
                                                                        in
                                                                        max length accum
                                                                    )
                                                                    minusInfinity

                                                        size =
                                                            longest_line + Rectangle.mainAxis direction padding_border
                                                     in
                                                     case Size.mainAxis direction available_space of
                                                        Just val ->
                                                            if List.length flex_lines_phase_1 > 1 && size < val then
                                                                val

                                                            else
                                                                size

                                                        Nothing ->
                                                            size
                                                    )
                                            )

                                newInnerContainerSize =
                                    inner_container_size |> Size.setMainAxis direction (Size.mainAxis direction newContainerSize - Rectangle.mainAxis direction padding_border)
                            in
                            { state | container_size = newContainerSize, inner_container_size = newInnerContainerSize }

                        flex_lines_phase_2 : List FlexLine
                        flex_lines_phase_2 =
                            flex_lines_phase_1
                                |> flex_lines_3 newState (Node node)
                                |> flex_lines_4 newState (Node node)
                                |> flex_lines_5 newState (Node node)
                                |> flex_lines_6 newState (Node node)
                                |> flex_lines_7 newState (Node node)
                                |> flex_lines_8 newState (Node node)
                                |> flex_lines_9 newState (Node node)

                        --  cross-axis alignment
                        total_cross_size : Float
                        total_cross_size =
                            flex_lines_phase_2
                                |> List.map .cross_size
                                |> List.sum

                        newerState : State
                        newerState =
                            let
                                newerContainerSize =
                                    newState.container_size
                                        |> Size.setCrossAxis direction
                                            (Size.crossAxis direction node_size |> Maybe.withDefault (total_cross_size + Rectangle.crossAxis direction padding_border))

                                newerInnerContainerSize =
                                    newState.inner_container_size |> Size.setCrossAxis direction (Size.crossAxis direction newerContainerSize - Rectangle.crossAxis direction padding_border)
                            in
                            { newState | container_size = newerContainerSize, inner_container_size = newerInnerContainerSize }
                    in
                    -- We have the container size. If our caller does not care about performing
                    -- layout we are done now.
                    if not perform_layout then
                        let
                            result =
                                { size = newerState.container_size, children = [] }
                        in
                        -- ( Node { node | cache = LayoutCache node_size parent_size perform_layout result }, result )
                        result

                    else
                        let
                            flex_lines_10 =
                                calculateLayout
                                    newerState
                                    (Node node)
                                    total_cross_size
                                    flex_lines_phase_2

                            flex_children =
                                finalLayoutPass
                                    newerState
                                    (Node node)
                                    flex_lines_10

                            absolute_children =
                                layoutAbsoluteChildren
                                    newerState
                                    (Node node)
                        in
                        { size = newerState.container_size
                        , children =
                            (flex_children ++ absolute_children)
                                |> List.sortBy (\(LayoutNode n) -> n.order)
                        }


{-|

    16. Align all flex lines per align-content.

-}
calculateLayout :
    { a
        | direction : FlexDirection.FlexDirection
        , inner_container_size : Size Float
        , is_wrap_reverse : Bool
    }
    -> Node
    -> Float
    -> List FlexLine
    -> List FlexLine
calculateLayout ({ inner_container_size, direction, is_wrap_reverse } as state) (Node node) total_cross_size flex_lines =
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
    { a
        | container_size : Size Float
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
                            |> List.foldl (layout_item (Node node) line_offset_cross) layoutItemConfig

                    else
                        line.items
                            |> List.foldl (layout_item (Node node) line_offset_cross) layoutItemConfig
            in
            { total_offset_cross = total_offset_cross + line_offset_cross + line.cross_size
            , lines =
                lines
                    ++ [ if FlexDirection.isReverse direction then
                            List.reverse newItems.children

                         else
                            newItems.children
                       ]
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
    -> Float
    -> FlexItem
    -> { children : List LayoutNode, total_offset_main : Float, total_offset_cross : Float, container_size : Size Float, direction : FlexDirection, is_row : Bool }
    -> { children : List LayoutNode, total_offset_main : Float, total_offset_cross : Float, container_size : Size Float, direction : FlexDirection, is_row : Bool }
layout_item (Node node) line_offset_cross child ({ children, total_offset_main, container_size, total_offset_cross, direction, is_row } as state) =
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
                + line_offset_cross
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
        | total_offset_main = total_offset_main + child.offset_main + Rectangle.mainAxis direction child.margin + Size.mainAxis direction result.size
        , children = children ++ [ newChild ]
    }


layoutAbsoluteChildren :
    { a
        | container_size : Size Float
        , is_row : Bool
        , is_wrap_reverse : Bool
        , direction : FlexDirection
        , node_inner_size : Size Number
        , border : Rectangle Float
        , padding_border : Rectangle Float
    }
    -> Node
    -> List LayoutNode
layoutAbsoluteChildren { container_size, is_row, direction, node_inner_size, padding_border, border, is_wrap_reverse } (Node node) =
    let
        alignChild : ( Int, Node ) -> LayoutNode
        alignChild ( order, Node child ) =
            let
                container_width =
                    container_size.width

                container_height =
                    container_size.height

                start =
                    Maybe.map2 (+) (Dimension.resolve container_width child.position.start) (Dimension.resolve container_width child.margin.start)

                end =
                    Maybe.map2 (+) (Dimension.resolve container_width child.position.end) (Dimension.resolve container_width child.margin.end)

                top =
                    Maybe.map2 (+) (Dimension.resolve container_height child.position.top) (Dimension.resolve container_height child.margin.top)

                bottom =
                    Maybe.map2 (+) (Dimension.resolve container_height child.position.bottom) (Dimension.resolve container_height child.margin.bottom)

                ( start_main, end_main ) =
                    if is_row then
                        ( start, end )

                    else
                        ( top, bottom )

                ( start_cross, end_cross ) =
                    if not is_row then
                        ( start, end )

                    else
                        ( top, bottom )

                width : Maybe Float
                width =
                    child.size.width
                        |> Dimension.resolve container_width
                        |> maybeMax (Dimension.resolve container_width child.min_size.width)
                        |> maybeMin (Dimension.resolve container_width child.max_size.width)
                        |> orElse (Maybe.map2 (\s e -> container_width - s - e) start end)

                height : Maybe Float
                height =
                    child.size.height
                        |> Dimension.resolve container_height
                        |> maybeMax (Dimension.resolve container_height child.min_size.height)
                        |> maybeMin (Dimension.resolve container_height child.max_size.height)
                        |> orElse (Maybe.map2 (\s e -> container_height - s - e) start end)

                result =
                    compute_internal (Node child) { width = width, height = height } { width = Just container_width, height = Just container_height } True

                free_main_space : Float
                free_main_space =
                    Size.mainAxis direction container_size
                        - (result
                            |> .size
                            |> Size.mainAxis direction
                            |> Just
                            |> maybeMax (Dimension.resolveNumber (Size.mainAxis direction node_inner_size) (Node.minMainSize direction (Node child)))
                            |> maybeMin (Dimension.resolveNumber (Size.mainAxis direction node_inner_size) (Node.maxMainSize direction (Node child)))
                            |> Maybe.withDefault 0
                          )

                free_cross_space : Float
                free_cross_space =
                    Size.crossAxis direction container_size
                        - (result
                            |> .size
                            |> Size.crossAxis direction
                            |> Just
                            |> maybeMax (Dimension.resolveNumber (Size.crossAxis direction node_inner_size) (Node.minCrossSize direction (Node child)))
                            |> maybeMin (Dimension.resolveNumber (Size.crossAxis direction node_inner_size) (Node.maxCrossSize direction (Node child)))
                            |> Maybe.withDefault 0
                          )

                offset_main : Float
                offset_main =
                    case ( start_main, end_main ) of
                        ( Just start_main_, _ ) ->
                            start_main_ + Rectangle.mainAxisStart direction border

                        ( Nothing, Just end_main_ ) ->
                            free_main_space - end_main_ + Rectangle.mainAxisEnd direction border

                        _ ->
                            case node.justify_content of
                                JustifyContent.SpaceBetween ->
                                    Rectangle.mainAxisStart direction padding_border

                                JustifyContent.FlexStart ->
                                    Rectangle.mainAxisStart direction padding_border

                                JustifyContent.FlexEnd ->
                                    free_main_space - Rectangle.mainAxisEnd direction padding_border

                                JustifyContent.SpaceEvenly ->
                                    free_main_space / 2.0

                                JustifyContent.SpaceAround ->
                                    free_main_space / 2.0

                                JustifyContent.Center ->
                                    free_main_space / 2.0

                offset_cross : Float
                offset_cross =
                    case ( start_cross, end_cross ) of
                        ( Just start_cross_, _ ) ->
                            start_cross_ + Rectangle.crossAxisStart direction border

                        ( Nothing, Just end_cross_ ) ->
                            free_cross_space - end_cross_ + Rectangle.crossAxisEnd direction border

                        _ ->
                            case Node.align { node = Node child, parent = Node node } of
                                AlignSelf.Auto ->
                                    0.0

                                -- Should never happen
                                AlignSelf.FlexStart ->
                                    if is_wrap_reverse then
                                        free_cross_space - Rectangle.crossAxisEnd direction padding_border

                                    else
                                        Rectangle.crossAxisStart direction padding_border

                                AlignSelf.FlexEnd ->
                                    if is_wrap_reverse then
                                        Rectangle.crossAxisStart direction padding_border

                                    else
                                        free_cross_space - Rectangle.crossAxisEnd direction padding_border

                                AlignSelf.Center ->
                                    free_cross_space / 2.0

                                AlignSelf.Baseline ->
                                    free_cross_space / 2.0

                                -- Treat as center for now until we have baseline support
                                AlignSelf.Stretch ->
                                    if is_wrap_reverse then
                                        free_cross_space - Rectangle.crossAxisEnd direction padding_border

                                    else
                                        Rectangle.crossAxisStart direction padding_border
            in
            LayoutNode
                { order = order
                , size = result.size
                , location =
                    { x =
                        if is_row then
                            offset_main

                        else
                            offset_cross
                    , y =
                        if not is_row then
                            offset_main

                        else
                            offset_cross
                    }
                , children = result.children
                }
    in
    node.children
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, Node child ) -> child.position_type == Node.Absolute)
        |> List.map alignChild



-- TODO add hidden children


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
maybeMax rhs self =
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


orElse : Maybe a -> Maybe a -> Maybe a
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
    if f == 0 then
        -- there is a bug where `0 : Bool` which messes with tests
        -- so explicitly return False here
        False

    else
        not (isNaN f) && not (isInfinite f)


position : (a -> Bool) -> List a -> Maybe Int
position =
    findIndexHelp 0


findIndexHelp : Int -> (a -> Bool) -> List a -> Maybe Int
findIndexHelp index predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just index

            else
                findIndexHelp (index + 1) predicate xs


calculate_baseline : LayoutNode -> Float
calculate_baseline (LayoutNode layout) =
    case layout.children of
        [] ->
            layout.size.height

        x :: _ ->
            calculate_baseline x


minusInfinity : Float
minusInfinity =
    -1 / 0


sumBy : (a -> number) -> List a -> number
sumBy toNumber =
    List.foldl (\elem accum -> toNumber elem + accum) 0
