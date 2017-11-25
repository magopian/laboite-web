module Tests exposing (..)

import Array
import Test exposing (..)
import Expect
import Matrix


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ matrix
        ]


matrix : Test
matrix =
    describe "Matrix"
        [ test "emptyRow creates an empty matrix row" <|
            \_ ->
                Expect.equal (Matrix.emptyRow 2) (Array.repeat 2 Matrix.Black)
        , test "emptyMatrix creates an empty matrix" <|
            \_ ->
                Expect.equal (Matrix.empty 2 3) (Array.repeat 3 (Matrix.emptyRow 2))
        , test "size returns a two-tuple of the matrix width and height" <|
            \_ ->
                Expect.equal (Matrix.size (Matrix.empty 2 3)) ( 2, 3 )
        , test "stringToLeds creates a matrix row with the given leds" <|
            \_ ->
                Expect.equal
                    (Matrix.stringToLeds "0101")
                    (Array.fromList [ Matrix.Black, Matrix.White, Matrix.Black, Matrix.White ])
        , test "replaceLineLeds replaces some leds in a line starting at some index" <|
            \_ ->
                let
                    matrix =
                        Matrix.stringToLeds "000111"

                    data =
                        Matrix.stringToLeds "1100"

                    result =
                        Matrix.stringToLeds "011001"
                in
                    Expect.equal (Matrix.replaceLineLeds data 1 matrix) result
        , test "charToLeds loads a font character" <|
            \_ ->
                let
                    charA =
                        Array.fromList
                            [ Matrix.stringToLeds "00000"
                            , Matrix.stringToLeds "00000"
                            , Matrix.stringToLeds "01110"
                            , Matrix.stringToLeds "10010"
                            , Matrix.stringToLeds "10110"
                            , Matrix.stringToLeds "01010"
                            , Matrix.stringToLeds "00000"
                            ]
                in
                    Expect.equal (Matrix.charToLeds 'a') charA
        , test "dataToMatrix composes data onto a matrix" <|
            \_ ->
                let
                    charA =
                        Array.fromList
                            [ Matrix.stringToLeds "00000"
                            , Matrix.stringToLeds "00000"
                            , Matrix.stringToLeds "01110"
                            , Matrix.stringToLeds "10010"
                            , Matrix.stringToLeds "10110"
                            , Matrix.stringToLeds "01010"
                            , Matrix.stringToLeds "00000"
                            ]

                    matrix =
                        Matrix.empty 7 10

                    result =
                        Array.fromList
                            [ Matrix.stringToLeds "0000000"
                            , Matrix.stringToLeds "0000000"
                            , Matrix.stringToLeds "0000000"
                            , Matrix.stringToLeds "0000000"
                            , Matrix.stringToLeds "0011100"
                            , Matrix.stringToLeds "0100100"
                            , Matrix.stringToLeds "0101100"
                            , Matrix.stringToLeds "0010100"
                            , Matrix.stringToLeds "0000000"
                            , Matrix.stringToLeds "0000000"
                            ]
                in
                    Expect.equal (Matrix.dataToMatrix charA 1 2 matrix) result
        ]
