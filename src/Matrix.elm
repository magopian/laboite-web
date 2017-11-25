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
    List.repeat width Black
        |> Array.fromList
        |> List.repeat height
        |> Array.fromList


{-| Compose a data buffer onto a matrix
-}
dataToMatrix : Matrix -> Int -> Int -> Matrix -> Matrix
dataToMatrix data x y matrix =
    matrix


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
