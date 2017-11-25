module Main exposing (..)

import Array
import Html
import Html.Attributes
import Matrix


---- MODEL ----


type alias Model =
    { data : Matrix.Data
    , matrix : Matrix.Matrix
    }


width : Int
width =
    32


height : Int
height =
    16


init : ( Model, Cmd Msg )
init =
    let
        dataA =
            Matrix.charToLeds 'a'

        dataB =
            Matrix.charToLeds 'b'

        matrix =
            Matrix.empty 32 16

        newMatrix =
            Matrix.dataToMatrix dataA 0 0 matrix
                |> Matrix.dataToMatrix dataB 8 8
    in
        ( { data =
                { duration = 5
                , items =
                    [ { height = 9
                      , content = "0xff839999839f9f9fff"
                      , width = 8
                      , y = 1
                      , x = 1
                      , type_ = Matrix.Icon
                      }
                    , { height = 8
                      , content = "Hello !"
                      , width = 35
                      , y = 9
                      , x = 0
                      , type_ = Matrix.Text
                      }
                    ]
                , id = 38
                , brightness = 15
                }
          , matrix = newMatrix
          }
        , Cmd.none
        )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ displayMatrix width height model.matrix
        ]


toPixelAttr : Int -> String
toPixelAttr px =
    let
        attr =
            toString px
    in
        attr ++ "px"


displayMatrix : Matrix.Width -> Matrix.Height -> Matrix.Matrix -> Html.Html Msg
displayMatrix width height matrix =
    Html.div
        [ Html.Attributes.class "led"
        , Html.Attributes.style
            [ ( "width", width * 11 |> toPixelAttr )
            , ( "height", height * 11 |> toPixelAttr )
            ]
        ]
        (matrix
            |> Array.map
                (\line ->
                    Html.div [ Html.Attributes.class "row" ] (displayLine line)
                )
            |> Array.toList
        )


displayLine : Array.Array Matrix.Led -> List (Html.Html Msg)
displayLine line =
    line
        |> Array.map
            (\led ->
                Html.div [ Html.Attributes.class (toString led) ] []
            )
        |> Array.toList



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
