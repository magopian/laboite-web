module Matrix exposing (..)

import Array.Hamt as Array
import Chars
import Dict


type ItemContent
    = Text String
    | Icon String Width Height


type alias Item =
    { content : ItemContent
    , y : Int
    , x : Int
    }


type alias Slide =
    { duration : Int
    , items : List Item
    , id : Int
    , brightness : Int
    }


type alias SlideInfo =
    { last_activity : Int
    , id : Int
    }


type alias SlideInfoList =
    List SlideInfo


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

If the data doesn't fit, scroll it in from the right.

-}
replaceLineLeds : Int -> MatrixRow -> Int -> MatrixRow -> MatrixRow
replaceLineLeds tick dataLine x matrixLine =
    let
        dataLength =
            Array.length dataLine

        matrixLength =
            Array.length matrixLine

        -- Number of pixels available in matrixLine to the right of coordinate x
        availableLength =
            matrixLength - x

        scrolledData =
            if dataLength + x > matrixLength then
                let
                    paddedData =
                        -- Padd it from the left so it scrolls in.
                        Array.append (emptyRow availableLength) dataLine

                    paddedLength =
                        Array.length paddedData

                    offset =
                        tick % paddedLength
                in
                    Array.slice offset paddedLength paddedData
            else
                dataLine

        clippedData =
            Array.slice 0 availableLength scrolledData

        clippedDataLength =
            Array.length clippedData

        matrixLineBefore =
            Array.slice 0 x matrixLine

        matrixLineAfter =
            Array.slice (x + clippedDataLength) matrixLength matrixLine
    in
        matrixLineAfter
            -- Slice so we don't display anything too large for the display
            |> Array.append clippedData
            |> Array.append matrixLineBefore


{-| Compose a data buffer onto a matrix at the x y coordinates.
-}
dataToMatrix : Int -> Matrix -> Int -> Int -> Matrix -> Matrix
dataToMatrix tick data x y matrix =
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
                            replaceLineLeds tick dataLine x matrixLine
                    )
    in
        matrixAfter
            |> Array.append matrixChanged
            |> Array.append matrixBefore


itemToMatrix : Int -> Item -> Matrix -> Matrix
itemToMatrix tick item matrix =
    let
        content =
            fromContent item.content
    in
        dataToMatrix tick content item.x item.y matrix


itemsToMatrix : Int -> List Item -> Matrix -> Matrix
itemsToMatrix tick items matrix =
    items
        |> List.foldl (itemToMatrix tick) matrix


stringToLeds : String -> MatrixRow
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


fromChar : Char -> Matrix
fromChar c =
    Dict.get c Chars.charData
        |> Maybe.withDefault []
        |> List.map stringToLeds
        |> Array.fromList


fromContent : ItemContent -> Matrix
fromContent itemContent =
    case itemContent of
        Text text ->
            fromText text

        Icon hexContent width height ->
            fromIcon width height hexContent


splitString : Int -> String -> List String
splitString size str =
    case String.left size str of
        "" ->
            []

        listHead ->
            listHead :: splitString size (String.dropLeft size str)


fromIcon : Width -> Height -> String -> Matrix
fromIcon width height content =
    content
        -- Drop the hexadecimal header "0x" from the content
        |> String.dropLeft 2
        |> String.toList
        |> List.map hexToBin
        |> String.join ""
        |> splitString width
        |> List.map stringToLeds
        |> Array.fromList


fromText : String -> Matrix
fromText str =
    let
        chars =
            str
                |> String.toList
                |> List.map fromChar
                |> List.reverse

        head =
            List.head chars |> Maybe.withDefault (empty 0 0)

        tail =
            List.tail chars |> Maybe.withDefault []
    in
        tail
            |> List.foldl append head


hexToBin : Char -> String
hexToBin c =
    case c of
        '0' ->
            "0000"

        '1' ->
            "0001"

        '2' ->
            "0010"

        '3' ->
            "0011"

        '4' ->
            "0100"

        '5' ->
            "0101"

        '6' ->
            "0110"

        '7' ->
            "0111"

        '8' ->
            "1000"

        '9' ->
            "1001"

        'a' ->
            "1010"

        'b' ->
            "1011"

        'c' ->
            "1100"

        'd' ->
            "1101"

        'e' ->
            "1110"

        'f' ->
            "1111"

        _ ->
            "0000"


{-| Append to matrices to a new matrix. The two matrices MUST be of the same height
-}
append : Matrix -> Matrix -> Matrix
append m1 m2 =
    m1
        |> Array.indexedMap
            (\j row1 ->
                let
                    row2 =
                        m2
                            |> Array.get j
                            |> Maybe.withDefault Array.empty
                in
                    Array.append row1 row2
            )
