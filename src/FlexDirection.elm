module FlexDirection exposing (FlexDirection(..), default, isColumn, isReverse, isRow)


type FlexDirection
    = Row
    | Column
    | RowReverse
    | ColumnReverse


default =
    Row


isRow flexDirection =
    case flexDirection of
        Row ->
            True

        RowReverse ->
            True

        _ ->
            False


isColumn flexDirection =
    case flexDirection of
        Column ->
            True

        ColumnReverse ->
            True

        _ ->
            False


isReverse flexDirection =
    case flexDirection of
        RowReverse ->
            True

        ColumnReverse ->
            True

        _ ->
            False
