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
        data =
            { duration = 5
            , items =
                [ { content = Matrix.Icon "ff839999839f9f9fff" 8 9
                  , y = 7
                  , x = 12
                  }
                , { content = Matrix.Text "Hello!"
                  , y = 0
                  , x = 0
                  }
                ]
            , id = 38
            , brightness = 15
            }

        matrix =
            Matrix.empty 32 16
                |> Matrix.itemsToMatrix data.items
    in
        ( { data = data
          , matrix = matrix
          }
        , Cmd.none
        )



---- UPDATE ----


type Msg
    = Converted String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Converted s ->
            let
                _ =
                    Debug.log "converted" s
            in
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
