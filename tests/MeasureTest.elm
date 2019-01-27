module MeasureTest exposing (suite)

import Algorithm
import AlignItems
import AlignSelf
import Dimension exposing (Dimension(..))
import Expect exposing (Expectation)
import FlexDirection
import Fuzz exposing (Fuzzer, int, list, string)
import JustifyContent exposing (JustifyContent(..))
import Node exposing (LayoutNode(..), Node(..))
import Size exposing (Size)
import Test exposing (..)


defaultSize =
    Size.undefined


defaultNode =
    let
        (Node d) =
            Node.default
    in
    d


suite : Test
suite =
    describe "measurement tests"
        [ measureChildConstraint
        , measureRoot
        , measureChild
        , measureChildConstraintPaddingParent
        , measureChildWithFlexGrow
        , measureChildWithFlexShrink
        , helpers
        , remeasureChildAfterGrowing
        , remeasureChildAfterShrinking
        , remeasureChildAfterStretching
        , widthOverridesMeasure
        , heightOverridesMeasure
        , flexBasisOverridesMeasure
        , stretchOverridesMeasure
        , ignoreInvalidMeasure
        , measureAbsoluteChild
        , justify_content_row_space_around
        , absolute_layout_align_items_and_justify_content_flex_end
        , absolute_layout_in_wrap_reverse_row_container_flex_end
        , flexWrapWraps
        , flex_basis_smaller_then_content_with_flex_grow_large_size
        , align_baseline_nested_child
        , margin_auto_bottom_and_top
        ]


measureRoot =
    let
        (LayoutNode layout) =
            Algorithm.compute
                (Node
                    { defaultNode
                        | measure =
                            Just
                                (\constraint ->
                                    { width = constraint.width |> Maybe.withDefault 100
                                    , height = constraint.height |> Maybe.withDefault 100
                                    }
                                )
                    }
                )
                Size.undefined
    in
    describe "measure root"
        [ test "width is correct" <|
            \_ ->
                layout.size.width
                    |> Expect.equal 100
        , test "height is correct" <|
            \_ ->
                layout.size.height
                    |> Expect.equal 100
        ]


measureChild =
    let
        (LayoutNode layout) =
            Algorithm.compute
                (Node
                    { defaultNode
                        | children =
                            [ Node
                                { defaultNode
                                    | measure =
                                        Just
                                            (\constraint ->
                                                { width = constraint.width |> Maybe.withDefault 100
                                                , height = constraint.height |> Maybe.withDefault 100
                                                }
                                            )
                                }
                            ]
                    }
                )
                Size.undefined
    in
    describe "measure child"
        [ test "parent width is correct" <|
            \_ ->
                layout.size.width
                    |> Expect.equal 100
        , test "parent height is correct" <|
            \_ ->
                layout.size.height
                    |> Expect.equal 100
        , test "child width is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.width)
                    |> Expect.equal (Just 100)
        , test "child height is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.height)
                    |> Expect.equal (Just 100)
        ]


measureChildConstraint =
    let
        (LayoutNode layout) =
            Algorithm.compute
                (Node
                    { defaultNode
                        | size = { width = Dimension.Points 50, height = Dimension.default }
                        , children =
                            [ Node
                                { defaultNode
                                    | measure =
                                        Just
                                            (\constraint ->
                                                { width = constraint.width |> Maybe.withDefault 100
                                                , height = constraint.height |> Maybe.withDefault 100
                                                }
                                            )
                                }
                            ]
                    }
                )
                Size.undefined
    in
    describe "measure child constraint"
        [ test "parent width is correct" <|
            \_ ->
                layout.size.width
                    |> Expect.equal 50
        , test "parent height is correct" <|
            \_ ->
                layout.size.height
                    |> Expect.equal 100
        , test "child height is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.height)
                    |> Expect.equal (Just 100)
        , test "child width is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.width)
                    |> Expect.equal (Just 50)
        ]


measureChildConstraintPaddingParent =
    let
        (LayoutNode layout) =
            Algorithm.compute
                (Node
                    { defaultNode
                        | size = { width = Dimension.Points 50, height = Dimension.default }
                        , padding =
                            { start = Dimension.Points 10
                            , end = Dimension.Points 10
                            , top = Dimension.Points 10
                            , bottom = Dimension.Points 10
                            }
                        , children =
                            [ Node
                                { defaultNode
                                    | measure =
                                        Just
                                            (\constraint ->
                                                { width = constraint.width |> Maybe.withDefault 100
                                                , height = constraint.height |> Maybe.withDefault 100
                                                }
                                            )
                                }
                            ]
                    }
                )
                Size.undefined
    in
    describe "measure child constraint padding parent"
        [ test "parent width is correct" <|
            \_ ->
                layout.size.width
                    |> Expect.equal 50
        , test "parent height is correct" <|
            \_ ->
                layout.size.height
                    |> Expect.equal 120
        , test "child width is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.width)
                    |> Expect.equal (Just 30)
        , test "child height is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.height)
                    |> Expect.equal (Just 100)
        ]


measureChildWithFlexGrow =
    let
        (LayoutNode layout) =
            Algorithm.compute
                (Node
                    { defaultNode
                        | size = { width = Dimension.Points 100, height = Dimension.default }
                        , children =
                            [ Node { defaultNode | size = { width = Dimension.Points 50, height = Dimension.Points 50 } }
                            , Node
                                { defaultNode
                                    | measure =
                                        Just
                                            (\constraint ->
                                                { width = constraint.width |> Maybe.withDefault 10
                                                , height = constraint.height |> Maybe.withDefault 50
                                                }
                                            )
                                    , flex_grow = 1
                                }
                            ]
                    }
                )
                Size.undefined
    in
    describe "measure child with flex grow"
        [ test "child width is correct" <|
            \_ ->
                layout.children
                    |> List.drop 1
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.width)
                    |> Expect.equal (Just 50)
        , test "child height is correct" <|
            \_ ->
                layout.children
                    |> List.drop 1
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.height)
                    |> Expect.equal (Just 50)
        ]


measureChildWithFlexShrink =
    let
        (LayoutNode layout) =
            Algorithm.compute
                (Node
                    { defaultNode
                        | size = { width = Dimension.Points 100, height = Dimension.default }
                        , children =
                            [ Node
                                { defaultNode
                                    | size = { width = Dimension.Points 50, height = Dimension.Points 50 }
                                    , flex_shrink = 0
                                }
                            , Node
                                { defaultNode
                                    | measure =
                                        Just
                                            (\constraint ->
                                                { width = constraint.width |> Maybe.withDefault 100
                                                , height = constraint.height |> Maybe.withDefault 50
                                                }
                                            )
                                }
                            ]
                    }
                )
                Size.undefined
    in
    describe "measure child with flex shrink"
        [ test "child width is correct" <|
            \_ ->
                layout.children
                    |> List.drop 1
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.width)
                    |> Expect.equal (Just 50)
        , test "child height is correct" <|
            \_ ->
                layout.children
                    |> List.drop 1
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.height)
                    |> Expect.equal (Just 50)
        ]


remeasureChildAfterGrowing =
    let
        (LayoutNode layout) =
            Algorithm.compute
                (Node
                    { defaultNode
                        | size = { width = Dimension.Points 100, height = Dimension.default }
                        , align_items = AlignItems.FlexStart
                        , children =
                            [ Node
                                { defaultNode
                                    | size = { width = Dimension.Points 50, height = Dimension.Points 50 }
                                }
                            , Node
                                { defaultNode
                                    | measure =
                                        Just
                                            (\constraint ->
                                                let
                                                    width =
                                                        constraint.width |> Maybe.withDefault 10

                                                    height =
                                                        constraint.height |> Maybe.withDefault (width * 2)
                                                in
                                                { width = width, height = height }
                                            )
                                    , flex_grow = 1
                                }
                            ]
                    }
                )
                Size.undefined
    in
    describe "remeasure child after growing"
        [ test "child width is correct" <|
            \_ ->
                layout.children
                    |> List.drop 1
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.width)
                    |> Expect.equal (Just 50)
        , test "child height is correct" <|
            \_ ->
                layout.children
                    |> List.drop 1
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.height)
                    |> Expect.equal (Just 100)
        ]


remeasureChildAfterShrinking =
    let
        (LayoutNode layout) =
            Algorithm.compute
                (Node
                    { defaultNode
                        | size = { width = Dimension.Points 100, height = Dimension.default }
                        , align_items = AlignItems.FlexStart
                        , children =
                            [ Node
                                { defaultNode
                                    | size = { width = Dimension.Points 50, height = Dimension.Points 50 }
                                    , flex_shrink = 0
                                }
                            , Node
                                { defaultNode
                                    | measure =
                                        Just
                                            (\constraint ->
                                                let
                                                    width =
                                                        constraint.width |> Maybe.withDefault 100

                                                    height =
                                                        constraint.height |> Maybe.withDefault (width * 2)
                                                in
                                                { width = width, height = height }
                                            )
                                    , flex_grow = 1
                                }
                            ]
                    }
                )
                Size.undefined
    in
    describe "remeasure child after shrinking"
        [ test "child width is correct" <|
            \_ ->
                layout.children
                    |> List.drop 1
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.width)
                    |> Expect.equal (Just 50)
        , test "child height is correct" <|
            \_ ->
                layout.children
                    |> List.drop 1
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.height)
                    |> Expect.equal (Just 100)
        ]


remeasureChildAfterStretching =
    let
        (LayoutNode layout) =
            Algorithm.compute
                (Node
                    { defaultNode
                        | size = { width = Dimension.Points 100, height = Dimension.Points 100 }
                        , children =
                            [ Node
                                { defaultNode
                                    | measure =
                                        Just
                                            (\constraint ->
                                                let
                                                    height =
                                                        constraint.height |> Maybe.withDefault 50

                                                    width =
                                                        constraint.width |> Maybe.withDefault height
                                                in
                                                { width = width, height = height }
                                            )
                                }
                            ]
                    }
                )
                Size.undefined
    in
    describe "remeasure child after stretching"
        [ test "child width is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.width)
                    |> Expect.equal (Just 100)
        , test "child height is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.height)
                    |> Expect.equal (Just 100)
        ]


widthOverridesMeasure =
    let
        (LayoutNode layout) =
            Algorithm.compute
                (Node
                    { defaultNode
                        | children =
                            [ Node
                                { defaultNode
                                    | size = { width = Dimension.Points 50, height = Dimension.default }
                                    , measure =
                                        Just
                                            (\constraint ->
                                                let
                                                    width =
                                                        constraint.width |> Maybe.withDefault 100

                                                    height =
                                                        constraint.height
                                                            |> Maybe.withDefault 100
                                                in
                                                { width = width, height = height }
                                            )
                                }
                            ]
                    }
                )
                Size.undefined
    in
    describe "width overrides measure"
        [ test "child width is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.width)
                    |> Expect.equal (Just 50)
        , test "child height is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.height)
                    |> Expect.equal (Just 100)
        ]


heightOverridesMeasure =
    let
        (LayoutNode layout) =
            Algorithm.compute
                (Node
                    { defaultNode
                        | children =
                            [ Node
                                { defaultNode
                                    | size = { height = Dimension.Points 50, width = Dimension.default }
                                    , measure =
                                        Just
                                            (\constraint ->
                                                let
                                                    width =
                                                        constraint.width |> Maybe.withDefault 100

                                                    height =
                                                        constraint.height
                                                            |> Maybe.withDefault 100
                                                in
                                                { width = width, height = height }
                                            )
                                }
                            ]
                    }
                )
                Size.undefined
    in
    describe "height overrides measure"
        [ test "child width is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.width)
                    |> Expect.equal (Just 100)
        , test "child height is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.height)
                    |> Expect.equal (Just 50)
        ]


flexBasisOverridesMeasure =
    let
        (LayoutNode layout) =
            Algorithm.compute
                (Node
                    { defaultNode
                        | size = { width = Dimension.Points 200, height = Dimension.Points 100 }
                        , children =
                            [ Node
                                { defaultNode
                                    | flex_basis = Dimension.Points 50
                                    , flex_grow = 1
                                }
                            , Node
                                { defaultNode
                                    | flex_basis = Dimension.Points 50
                                    , flex_grow = 1
                                    , measure =
                                        Just
                                            (\constraint ->
                                                let
                                                    width =
                                                        constraint.width |> Maybe.withDefault 100

                                                    height =
                                                        constraint.height |> Maybe.withDefault 100
                                                in
                                                { width = width, height = height }
                                            )
                                }
                            ]
                    }
                )
                Size.undefined
    in
    describe "flex basis overrides measure"
        [ test "first child width is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.width)
                    |> Expect.equal (Just 100)
        , test "first child height is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.height)
                    |> Expect.equal (Just 100)
        , test "second child width is correct" <|
            \_ ->
                layout.children
                    |> List.drop 1
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.width)
                    |> Expect.equal (Just 100)
        , test "second child height is correct" <|
            \_ ->
                layout.children
                    |> List.drop 1
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.height)
                    |> Expect.equal (Just 100)
        ]


stretchOverridesMeasure =
    let
        (LayoutNode layout) =
            Algorithm.compute
                (Node
                    { defaultNode
                        | size = { height = Dimension.Points 100, width = Dimension.Points 100 }
                        , children =
                            [ Node
                                { defaultNode
                                    | measure =
                                        Just
                                            (\constraint ->
                                                let
                                                    width =
                                                        constraint.width |> Maybe.withDefault 50

                                                    height =
                                                        constraint.height
                                                            |> Maybe.withDefault 50
                                                in
                                                { width = width, height = height }
                                            )
                                }
                            ]
                    }
                )
                Size.undefined
    in
    describe "stretch overrides measure"
        [ test "child width is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.width)
                    |> Expect.equal (Just 50)
        , test "child height is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.height)
                    |> Expect.equal (Just 100)
        ]


measureAbsoluteChild =
    let
        (LayoutNode layout) =
            Algorithm.compute
                (Node
                    { defaultNode
                        | size = { height = Dimension.Points 100, width = Dimension.Points 100 }
                        , children =
                            [ Node
                                { defaultNode
                                    | position_type = Node.Absolute
                                    , measure =
                                        Just
                                            (\constraint ->
                                                let
                                                    width =
                                                        constraint.width |> Maybe.withDefault 50

                                                    height =
                                                        constraint.height |> Maybe.withDefault 50
                                                in
                                                { width = width, height = height }
                                            )
                                }
                            ]
                    }
                )
                Size.undefined
    in
    describe "measure absolute child"
        [ test "child width is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.width)
                    |> Expect.equal (Just 50)
        , test "child height is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.height)
                    |> Expect.equal (Just 50)
        ]


ignoreInvalidMeasure =
    let
        (LayoutNode layout) =
            Algorithm.compute
                (Node
                    { defaultNode
                        | size = { height = Dimension.Points 100, width = Dimension.Points 100 }
                        , children =
                            [ Node
                                { defaultNode
                                    | flex_grow = 1
                                    , measure =
                                        Just
                                            (\_ ->
                                                { width = 200, height = 200 }
                                            )
                                }
                            ]
                    }
                )
                Size.undefined
    in
    describe "ignore invalid measure"
        [ test "child width is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.width)
                    |> Expect.equal (Just 100)
        , test "child height is correct" <|
            \_ ->
                layout.children
                    |> List.head
                    |> Maybe.map (\(LayoutNode node) -> node.size.height)
                    |> Expect.equal (Just 100)
        ]


helpers =
    describe "helpers"
        [ test "orElse for Just and Just " <|
            \_ ->
                Just 1
                    |> Algorithm.orElse (Just 2)
                    |> Expect.equal (Just 1)
        , test "orElse for Nothing and Just" <|
            \_ ->
                Nothing
                    |> Algorithm.orElse (Just 2)
                    |> Expect.equal (Just 2)
        , test "orElse for Nothing and Nothing" <|
            \_ ->
                Nothing
                    |> Algorithm.orElse Nothing
                    |> Expect.equal Nothing
        , test "orElse for Just and Nothing" <|
            \_ ->
                Just 1
                    |> Algorithm.orElse Nothing
                    |> Expect.equal (Just 1)
        , test "isNormalFloat 42" <|
            \_ ->
                Algorithm.isNormalFloat 42
                    |> Expect.equal True
        , test "isNormalFloat 0" <|
            \_ ->
                Algorithm.isNormalFloat 0
                    |> Expect.equal False
        , test "isNormalFloat infinity" <|
            \_ ->
                Algorithm.isNormalFloat (1 / 0)
                    |> Expect.equal False
        ]


justify_content_row_space_around =
    let
        (LayoutNode layout) =
            Algorithm.compute node Size.undefined

        node =
            Node
                { defaultNode
                    | justify_content = JustifyContent.SpaceAround
                    , size = { width = Dimension.Points 100, height = Dimension.Points 100 }
                    , children =
                        [ Node { defaultNode | size = { width = Dimension.Points 10, height = Dimension.Auto } }
                        , Node { defaultNode | size = { width = Dimension.Points 10, height = Dimension.Auto } }
                        , Node { defaultNode | size = { width = Dimension.Points 10, height = Dimension.Auto } }
                        ]
                }
    in
    {-
       assert_eq!(layout.size.width, 100.0000);
       assert_eq!(layout.size.height, 100.0000);
       assert_eq!(layout.location.x, 0.0000);
       assert_eq!(layout.location.y, 0.0000);
       M

       assert_eq!(layout.children[0].size.width, 10.0000);
       assert_eq!(layout.children[0].size.height, 100.0000);
       assert_eq!(layout.children[0].location.x, 12.0000);
       assert_eq!(layout.children[0].location.y, 0.0000);

       assert_eq!(layout.children[1].size.width, 10.0000);
       assert_eq!(layout.children[1].size.height, 100.0000);
       assert_eq!(layout.children[1].location.x, 45.0000);
       assert_eq!(layout.children[1].location.y, 0.0000);

       assert_eq!(layout.children[2].size.width, 10.0000);
       assert_eq!(layout.children[2].size.height, 100.0000);
       assert_eq!(layout.children[2].location.x, 78.0000);
       assert_eq!(layout.children[2].location.y, 0.0000);
    -}
    describe "justify content row space around"
        [ test "1" <| \_ -> layout.size.width |> Expect.equal 100
        , test "2" <| \_ -> layout.size.height |> Expect.equal 100
        , test "3" <| \_ -> layout.location.x |> Expect.equal 0
        , test "4" <| \_ -> layout.location.y |> Expect.equal 0
        , case layout.children of
            [ LayoutNode first, LayoutNode second, LayoutNode third ] ->
                describe "children"
                    [ describe "first"
                        [ test "1" <| \_ -> first.size.width |> Expect.equal 10
                        , test "2" <| \_ -> first.size.height |> Expect.equal 100
                        , test "3" <| \_ -> first.location.x |> Expect.equal 12
                        , test "4" <| \_ -> first.location.y |> Expect.equal 0
                        ]
                    , describe "second"
                        [ test "1" <| \_ -> second.size.width |> Expect.equal 10
                        , test "2" <| \_ -> second.size.height |> Expect.equal 100
                        , test "3" <| \_ -> second.location.x |> Expect.equal 45
                        , test "4" <| \_ -> second.location.y |> Expect.equal 0
                        ]
                    , describe "third"
                        [ test "1" <| \_ -> third.size.width |> Expect.equal 10
                        , test "2" <| \_ -> third.size.height |> Expect.equal 100
                        , test "3" <| \_ -> third.location.x |> Expect.equal 78
                        , test "4" <| \_ -> third.location.y |> Expect.equal 0
                        ]
                    ]

            _ ->
                describe "fail" []
        ]


absolute_layout_align_items_and_justify_content_flex_end =
    let
        (LayoutNode layout) =
            Algorithm.compute node Size.undefined

        node =
            Node
                { defaultNode
                    | align_items = AlignItems.FlexEnd
                    , justify_content = JustifyContent.FlexEnd
                    , size = { width = Dimension.Points 110, height = Dimension.Points 100 }
                    , children =
                        [ Node { defaultNode | position_type = Node.Absolute, size = { width = Dimension.Points 60, height = Dimension.Points 40 } }
                        ]
                }
    in
    describe "absolute_layout_align_items_and_justify_content_flex_end"
        [ test "1" <| \_ -> layout.size.width |> Expect.equal 110
        , test "2" <| \_ -> layout.size.height |> Expect.equal 100
        , test "3" <| \_ -> layout.location.x |> Expect.equal 0
        , test "4" <| \_ -> layout.location.y |> Expect.equal 0
        , case layout.children of
            [ LayoutNode first ] ->
                describe "children"
                    [ describe "first"
                        [ test "1" <| \_ -> first.size.width |> Expect.equal 60
                        , test "2" <| \_ -> first.size.height |> Expect.equal 40
                        , test "3" <| \_ -> first.location.x |> Expect.equal 50
                        , test "4" <| \_ -> first.location.y |> Expect.equal 60
                        ]
                    ]

            _ ->
                describe "fail" []
        ]


absolute_layout_in_wrap_reverse_row_container_flex_end =
    let
        (LayoutNode layout) =
            Algorithm.compute node Size.undefined

        node =
            Node
                { defaultNode
                    | flex_wrap = Node.WrapReverse
                    , size = { width = Dimension.Points 100, height = Dimension.Points 100 }
                    , children =
                        [ Node
                            { defaultNode
                                | position_type = Node.Absolute
                                , align_self = AlignSelf.FlexEnd
                                , size = { width = Dimension.Points 20, height = Dimension.Points 20 }
                            }
                        ]
                }
    in
    describe "absolute_layout_in_wrap_reverse_row_container_flex_end"
        [ test "1" <| \_ -> layout.size.width |> Expect.equal 100
        , test "2" <| \_ -> layout.size.height |> Expect.equal 100
        , test "3" <| \_ -> layout.location.x |> Expect.equal 0
        , test "4" <| \_ -> layout.location.y |> Expect.equal 0
        , case layout.children of
            [ LayoutNode first ] ->
                describe "children"
                    [ describe "first"
                        [ test "1" <| \_ -> first.size.width |> Expect.equal 20
                        , test "2" <| \_ -> first.size.height |> Expect.equal 20
                        , test "3" <| \_ -> first.location.x |> Expect.equal 0
                        , test "4" <| \_ -> first.location.y |> Expect.equal 0
                        ]
                    ]

            _ ->
                describe "fail" []
        ]


flexWrapWraps =
    let
        (LayoutNode layout) =
            Algorithm.compute node Size.undefined

        node =
            Node
                { defaultNode
                    | size = { width = Dimension.Points 800, height = Dimension.Points 800 }
                    , flex_wrap = Node.Wrap
                    , children =
                        [ Node { defaultNode | size = { width = Dimension.Points 500, height = Dimension.Points 20 } }
                        , Node { defaultNode | size = { width = Dimension.Points 500, height = Dimension.Points 20 } }
                        ]
                }
    in
    describe "flex wrap actually wraps"
        [ test "1" <| \_ -> layout.size.width |> Expect.equal 800
        , test "2" <| \_ -> layout.size.height |> Expect.equal 800
        , test "3" <| \_ -> layout.location.x |> Expect.equal 0
        , test "4" <| \_ -> layout.location.y |> Expect.equal 0
        , case layout.children of
            [ LayoutNode first, LayoutNode second ] ->
                describe "children"
                    [ describe "first"
                        [ test "1" <| \_ -> first.size.width |> Expect.equal 500
                        , test "2" <| \_ -> first.size.height |> Expect.equal 20
                        , test "3" <| \_ -> first.location.x |> Expect.equal 0
                        , test "4" <| \_ -> first.location.y |> Expect.equal 0
                        ]
                    , describe "second"
                        [ test "1" <| \_ -> second.size.width |> Expect.equal 500
                        , test "2" <| \_ -> second.size.height |> Expect.equal 20
                        , test "3" <| \_ -> second.location.x |> Expect.equal 0
                        , test "4" <| \_ -> second.location.y |> Expect.equal 400
                        ]
                    ]

            _ ->
                describe "fail" []
        ]


flex_basis_smaller_then_content_with_flex_grow_large_size =
    let
        (LayoutNode layout) =
            Algorithm.compute node Size.undefined

        node =
            Node
                { defaultNode
                    | size = { width = Dimension.Points 100, height = Dimension.Auto }
                    , children =
                        [ Node
                            { defaultNode
                                | flex_direction = FlexDirection.Column
                                , flex_grow = 1
                                , flex_basis = Dimension.Points 0
                                , children =
                                    [ Node { defaultNode | size = { width = Dimension.Points 70, height = Dimension.Points 100 } }
                                    ]
                            }
                        , Node
                            { defaultNode
                                | flex_direction = FlexDirection.Column
                                , flex_grow = 1
                                , flex_basis = Dimension.Points 0
                                , children =
                                    [ Node { defaultNode | size = { width = Dimension.Points 20, height = Dimension.Points 100 } }
                                    ]
                            }
                        ]
                }
    in
    describe "flex_basis_smaller_then_content_with_flex_grow_large_size"
        [ test "width" <| \_ -> layout.size.width |> Expect.equal 100
        , test "height" <| \_ -> layout.size.height |> Expect.equal 100
        , test "location.x" <| \_ -> layout.location.x |> Expect.equal 0
        , test "location.y" <| \_ -> layout.location.y |> Expect.equal 0
        , case layout.children of
            [ LayoutNode first, LayoutNode second ] ->
                describe "children"
                    [ describe "first"
                        [ test "width" <| \_ -> first.size.width |> Expect.equal 70
                        , test "height" <| \_ -> first.size.height |> Expect.equal 100
                        , test "location.x" <| \_ -> first.location.x |> Expect.equal 0
                        , test "location.y" <| \_ -> first.location.y |> Expect.equal 0
                        , case first.children of
                            [ LayoutNode firstfirst ] ->
                                describe "first.first"
                                    [ test "width" <| \_ -> firstfirst.size.width |> Expect.equal 70
                                    , test "height" <| \_ -> firstfirst.size.height |> Expect.equal 100
                                    , test "location.x" <| \_ -> firstfirst.location.x |> Expect.equal 0
                                    , test "location.y" <| \_ -> firstfirst.location.y |> Expect.equal 0
                                    ]

                            _ ->
                                describe "fail" []
                        ]
                    , describe "second"
                        [ test "width" <| \_ -> second.size.width |> Expect.equal 30
                        , test "height" <| \_ -> second.size.height |> Expect.equal 100
                        , test "location.x" <| \_ -> second.location.x |> Expect.equal 70
                        , test "location.y" <| \_ -> second.location.y |> Expect.equal 0
                        , case second.children of
                            [ LayoutNode secondsecond ] ->
                                describe "second.second"
                                    [ test "width" <| \_ -> secondsecond.size.width |> Expect.equal 20
                                    , test "height" <| \_ -> secondsecond.size.height |> Expect.equal 100
                                    , test "location.x" <| \_ -> secondsecond.location.x |> Expect.equal 0
                                    , test "location.y" <| \_ -> secondsecond.location.y |> Expect.equal 0
                                    ]

                            _ ->
                                describe "fail" []
                        ]
                    ]

            _ ->
                describe "fail" []
        ]


margin_auto_bottom_and_top =
    let
        (LayoutNode layout) =
            Algorithm.compute node Size.undefined

        node =
            Node
                { defaultNode
                    | size = { width = Dimension.Points 200, height = Dimension.Points 200 }
                    , align_items = AlignItems.Center
                    , children =
                        [ Node
                            { defaultNode
                                | size = { width = Dimension.Points 50, height = Dimension.Points 50 }
                                , margin = { top = Dimension.Auto, bottom = Dimension.Auto, start = Dimension.Undefined, end = Dimension.Undefined }
                            }
                        , Node { defaultNode | size = { width = Dimension.Points 50, height = Dimension.Points 50 } }
                        ]
                }
    in
    describe "margin_auto_bottom_and_top"
        [ test "width" <| \_ -> layout.size.width |> Expect.equal 200
        , test "height" <| \_ -> layout.size.height |> Expect.equal 200
        , test "location.x" <| \_ -> layout.location.x |> Expect.equal 0
        , test "location.y" <| \_ -> layout.location.y |> Expect.equal 0
        , case layout.children of
            [ LayoutNode first, LayoutNode second ] ->
                describe "children"
                    [ describe "first"
                        [ test "width" <| \_ -> first.size.width |> Expect.equal 50
                        , test "height" <| \_ -> first.size.height |> Expect.equal 50
                        , test "location.x" <| \_ -> first.location.x |> Expect.equal 0
                        , test "location.y" <| \_ -> first.location.y |> Expect.equal 75
                        ]
                    , describe "second"
                        [ test "width" <| \_ -> second.size.width |> Expect.equal 50
                        , test "height" <| \_ -> second.size.height |> Expect.equal 50
                        , test "location.x" <| \_ -> second.location.x |> Expect.equal 50
                        , test "location.y" <| \_ -> second.location.y |> Expect.equal 75
                        ]
                    ]

            _ ->
                describe "fail" []
        ]


createSize w h =
    { width = Dimension.Points w, height = Dimension.Points h }


align_baseline_nested_child =
    let
        (LayoutNode layout) =
            Algorithm.compute node Size.undefined

        node =
            Node
                { defaultNode
                    | size = createSize 100 100
                    , align_items = AlignItems.Baseline
                    , children =
                        [ Node { defaultNode | size = { width = Dimension.Points 50, height = Dimension.Points 50 } }
                        , Node
                            { defaultNode
                                | size = { width = Dimension.Points 50, height = Dimension.Points 20 }
                                , flex_direction = FlexDirection.Column
                                , children =
                                    [ Node { defaultNode | size = { width = Dimension.Points 50, height = Dimension.Points 10 } } ]
                            }
                        ]
                }
    in
    describe "align_baseline_nested_child"
        [ test "width" <| \_ -> layout.size.width |> Expect.equal 100
        , test "height" <| \_ -> layout.size.height |> Expect.equal 100
        , test "location.x" <| \_ -> layout.location.x |> Expect.equal 0
        , test "location.y" <| \_ -> layout.location.y |> Expect.equal 0
        , case layout.children of
            [ LayoutNode first, LayoutNode second ] ->
                describe "children"
                    [ describe "first"
                        [ test "width" <| \_ -> first.size.width |> Expect.equal 50
                        , test "height" <| \_ -> first.size.height |> Expect.equal 50
                        , test "location.x" <| \_ -> first.location.x |> Expect.equal 0
                        , test "location.y" <| \_ -> first.location.y |> Expect.equal 0
                        ]
                    , describe "second"
                        [ test "width" <| \_ -> second.size.width |> Expect.equal 50
                        , test "height" <| \_ -> second.size.height |> Expect.equal 20
                        , test "location.x" <| \_ -> second.location.x |> Expect.equal 50
                        , test "location.y" <| \_ -> second.location.y |> Expect.equal 40
                        , case second.children of
                            [ LayoutNode secondsecond ] ->
                                describe "second.second"
                                    [ test "width" <| \_ -> secondsecond.size.width |> Expect.equal 50
                                    , test "height" <| \_ -> secondsecond.size.height |> Expect.equal 10
                                    , test "location.x" <| \_ -> secondsecond.location.x |> Expect.equal 0
                                    , test "location.y" <| \_ -> secondsecond.location.y |> Expect.equal 0
                                    ]

                            _ ->
                                describe "fail" []
                        ]
                    ]

            _ ->
                describe "fail" []
        ]
