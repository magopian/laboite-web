module Matrix exposing (..)

import Array
import Chars
import Dict


type ItemType
    = Text
    | Icon


type alias Item =
    { height : Int
    , content : String
    , width : Int
    , y : Int
    , x : Int
    , type_ : ItemType
    }


type alias Data =
    { duration : Int
    , items : List Item
    , id : Int
    , brightness : Int
    }


type Led
    = Black
    | White


type alias MatrixRow =
    Array.Array Led


type alias Matrix =
    Array.Array MatrixRow


type alias Width =
    Int


type alias Height =
    Int


emptyRow : Width -> MatrixRow
emptyRow width =
    Array.repeat width Black


empty : Width -> Height -> Matrix
empty width height =
    emptyRow width
        |> Array.repeat height


size : Matrix -> ( Int, Int )
size matrix =
    let
        firstLine =
            Array.get 0 matrix |> Maybe.withDefault (emptyRow 0)
    in
        ( Array.length firstLine, Array.length matrix )


{-| Replace the line of a matrix's content by the content of some data starting at index x
-}
replaceLineLeds : MatrixRow -> Int -> MatrixRow -> MatrixRow
replaceLineLeds dataLine x matrixLine =
    let
        dataLength =
            Array.length dataLine

        matrixLength =
            Array.length matrixLine

        matrixLineBefore =
            Array.slice 0 x matrixLine

        matrixLineAfter =
            Array.slice (x + dataLength) matrixLength matrixLine
    in
        matrixLineAfter
            |> Array.append dataLine
            |> Array.append matrixLineBefore


{-| Compose a data buffer onto a matrix
-}
dataToMatrix : Matrix -> Int -> Int -> Matrix -> Matrix
dataToMatrix data x y matrix =
    let
        dataHeight =
            Array.length data

        matrixHeight =
            Array.length matrix

        matrixBefore =
            Array.slice 0 y matrix

        matrixToChange =
            Array.slice y (y + dataHeight) matrix

        matrixAfter =
            Array.slice (y + dataHeight) matrixHeight matrix

        matrixChanged : Matrix
        matrixChanged =
            matrixToChange
                |> Array.indexedMap
                    (\j matrixLine ->
                        let
                            dataLine =
                                Array.get j data |> Maybe.withDefault (emptyRow 0)
                        in
                            replaceLineLeds dataLine x matrixLine
                    )
    in
        matrixAfter
            |> Array.append matrixChanged
            |> Array.append matrixBefore


stringToLeds : String -> Array.Array Led
stringToLeds s =
    s
        |> String.toList
        |> List.map
            (\c ->
                case c of
                    '1' ->
                        White

                    _ ->
                        Black
            )
        |> Array.fromList


charToLeds : Char -> Array.Array (Array.Array Led)
charToLeds c =
    Dict.get c Chars.charData
        |> Maybe.withDefault []
        |> List.map stringToLeds
        |> Array.fromList
