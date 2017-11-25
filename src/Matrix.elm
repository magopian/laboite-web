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


type alias Matrix =
    Array.Array (Array.Array Led)


type alias Width =
    Int


type alias Height =
    Int


empty : Width -> Height -> Matrix
empty width height =
    Array.repeat width Black
        |> Array.repeat height


{-| Replace the line of a matrix's content by the content of some data starting at index x
-}
replaceLineLeds : Array.Array Led -> Int -> Array.Array Led -> Array.Array Led
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

        dataFirstLine =
            Array.get 0 data |> Maybe.withDefault Array.empty

        dataWidth =
            Array.length dataFirstLine

        yRange =
            -- the range includes (not excludes) the end, so we need to remove 1
            List.range 0 (dataHeight - 1)

        xRange =
            -- the range includes (not excludes) the end, so we need to remove 1
            List.range 0 (dataWidth - 1)
    in
        matrix
            |> Array.indexedMap
                (\j line ->
                    if (y <= j) && (j < y + dataHeight) then
                        let
                            dataLine =
                                Array.get (j - y) data |> Maybe.withDefault Array.empty
                        in
                            replaceLineLeds dataLine x line
                    else
                        line
                )


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
