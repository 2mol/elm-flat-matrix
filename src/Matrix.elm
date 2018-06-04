module Matrix
    exposing
        ( Matrix
        , concatHorizontal
        , concatVertical
        , empty
        , filter
        , fromList
        , get
        , getColumn
        , getRow
        , height
        , indexedMap
        , map
        , map2
        , repeat
        , set
        , toIndexedArray
        , update
        , width
        )

{-| A matrix implemention for Elm.
Internally it uses a flat array for speed reasons.


# The matrix type

@docs Matrix


# Creating a matrix

@docs repeat, fromList, empty


# Get matrix dimensions

@docs height, width


# Dealing with individual elements

@docs get, set, update


# Appending to an Matrix

@docs concatVertical, concatHorizontal


# Get rows/columns

@docs getRow, getColumn


# Applying functions

@docs filter, map, map2, indexedMap, toIndexedArray

-}

import Array.Hamt as Array exposing (Array)
import List


{-| A Matrix has a shape `m x n`, and stores its data in a flat array.
-}
type alias Matrix a =
    { size : ( Int, Int )
    , data : Array a
    }


{-| Create an empty matrix
-}
empty : Matrix a
empty =
    { size = ( 0, 0 ), data = Array.empty }


{-| Height of a given matrix
-}
height : Matrix a -> Int
height matrix =
    Tuple.first matrix.size


{-| Width of a given matrix
-}
width : Matrix a -> Int
width matrix =
    Tuple.second matrix.size


{-| Create a matrix of a given size `x y` with a default value of `v`
-}
repeat : Int -> Int -> a -> Matrix a
repeat m n v =
    { size = ( m, n )
    , data = Array.repeat (m * n) v
    }


{-| Create a matrix from a list of lists.
If the lists within the list are not consistently sized, return `Nothing`
Otherwise return a matrix where the inner lists are the columns.

Example:

`fromList [[1,2,3], [4,5,6], [7,8,9]]`

would create the matrix

1 4 7
2 5 8
3 6 9

-}
fromList : List (List a) -> Maybe (Matrix a)
fromList list =
    let
        -- the number of elements in the first list is taken as the height
        m =
            List.length <|
                case List.head list of
                    Just x ->
                        x

                    Nothing ->
                        []

        -- the number of elements in the top level list is taken as width
        n =
            List.length list

        -- ensure that all columns are the same size
        equalColumns =
            List.all (\l -> List.length l == m) list
    in
    if not equalColumns then
        Nothing
    else
        Just { size = ( m, n ), data = Array.fromList <| List.concat list }


{-| Get a value from the row-column indices `i, j`. Returns `Nothing` if the indices exceed the shape of the matrix.
-}
get : Int -> Int -> Matrix a -> Maybe a
get i j { size, data } =
    let
        ( m, n ) =
            size

        pos =
            i + (j * m)
    in
    if i >= 0 && j >= 0 && i < m && j < n then
        Array.get pos data
    else
        Nothing


{-| Get a row at a given i
-}
getRow : Int -> Matrix a -> Maybe (Array a)
getRow i { size, data } =
    let
        ( m, n ) =
            size

        indexHelper : Int -> a -> Maybe a
        indexHelper index el =
            if index % m == i then
                Just el
            else
                Nothing

        nothingHelper : Maybe a -> List a -> List a
        nothingHelper el acc =
            case el of
                Just el_ ->
                    el_ :: acc

                Nothing ->
                    acc
    in
    if i < m then
        Array.indexedMap indexHelper data
            |> Array.foldr nothingHelper []
            |> Array.fromList
            |> Just
    else
        Nothing


{-| Get a column at a given j
-}
getColumn : Int -> Matrix a -> Maybe (Array a)
getColumn j { size, data } =
    let
        ( m, n ) =
            size

        start =
            m * j

        end =
            start + m
    in
    -- if end > (width matrix * height matrix) then
    if j < n then
        Just <| Array.slice start end data
    else
        Nothing


{-| Append a matrix to another matrix horizontally and return the result. Return Nothing if the heights don't match
-}
concatHorizontal : Matrix a -> Matrix a -> Maybe (Matrix a)
concatHorizontal a b =
    let
        finalWidth =
            Tuple.first a.size + Tuple.first b.size

        insert i xs array =
            Array.append
                (Array.append (Array.slice 0 i array) xs)
                (Array.slice i (Array.length array) array)
    in
    if Tuple.second a.size /= Tuple.second b.size then
        Nothing
    else
        Just <|
            { a
                | size = ( finalWidth, Tuple.second a.size )
                , data =
                    List.foldl
                        (\( i, xs ) acc -> insert (i * finalWidth) xs acc)
                        b.data
                    <|
                        List.foldl
                            (\i ls ->
                                case getRow i a of
                                    Just v ->
                                        ls ++ [ ( i, v ) ]

                                    Nothing ->
                                        ls
                            )
                            []
                            (List.range 0 (Tuple.second a.size - 1))
            }


{-| Append a matrix to another matrix vertically and return the result. Return Nothing if the widths don't match
-}
concatVertical : Matrix a -> Matrix a -> Maybe (Matrix a)
concatVertical a b =
    if Tuple.first a.size /= Tuple.first b.size then
        Nothing
    else
        Just <| { a | size = ( Tuple.first a.size, Tuple.second a.size + Tuple.second b.size ), data = Array.append a.data b.data }


{-| Set a value at a given `i, j` in the matrix and return the new matrix.
If the `i, j` is out of bounds then return the unmodified matrix.
-}
set : Int -> Int -> a -> Matrix a -> Matrix a
set i j v matrix =
    let
        ( m, n ) =
            matrix.size

        pos =
            i + (j * m)
    in
    if i >= 0 && j >= 0 && i < m && j < n then
        { matrix | data = Array.set pos v matrix.data }
    else
        matrix


{-| Update an element at `x, y` with the given update function
If out of bounds, return the matrix unchanged
-}
update : Int -> Int -> (a -> a) -> Matrix a -> Matrix a
update x y f matrix =
    case get x y matrix of
        Nothing ->
            matrix

        Just v ->
            set x y (f v) matrix


{-| Apply a function of every element in the matrix
-}
map : (a -> b) -> Matrix a -> Matrix b
map f matrix =
    { matrix | data = Array.map f matrix.data }


{-| Apply a function to two matricies at once
-}
map2 : (a -> b -> c) -> Matrix a -> Matrix b -> Maybe (Matrix c)
map2 f a b =
    if a.size == b.size then
        Just { a | data = Array.fromList <| List.map2 f (Array.toList a.data) (Array.toList b.data) }
    else
        Nothing


{-| Apply a function, taking the `x, y` of every element in the matrix
-}
indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap f matrix =
    let
        ( m, n ) =
            matrix.size

        f_ : Int -> a -> b
        f_ i v =
            f (i % m) (i // m) v
    in
    { matrix | data = Array.indexedMap f_ matrix.data }


{-| Keep only elements that return `True` when passed to the given function f
-}
filter : (a -> Bool) -> Matrix a -> Array a
filter f matrix =
    Array.filter f matrix.data


{-| Convert a matrix to an indexed array
-}
toIndexedArray : Matrix a -> Array ( ( Int, Int ), a )
toIndexedArray matrix =
    (indexedMap (\x y v -> ( ( x, y ), v )) matrix).data
