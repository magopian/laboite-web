module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)


---- MODEL ----


type alias Width =
    Int


width : Width
width =
    32


type alias Height =
    Int


height : Height
height =
    16


type alias Item =
    { height : Int
    , content : String
    , width : Int
    , y : Int
    , x : Int
    , type_ : String
    }


type alias Model =
    { duration : Int
    , items : List Item
    , id : Int
    , brightness : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { duration = 5
      , items =
            [ { height = 9
              , content = "0xff839999839f9f9fff"
              , width = 8
              , y = 1
              , x = 1
              , type_ = "icon"
              }
            , { height = 8
              , content = "Hello !"
              , width = 35
              , y = 9
              , x = 0
              , type_ = "text"
              }
            ]
      , id = 38
      , brightness = 15
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


view : Model -> Html Msg
view model =
    div []
        [ displayMatrix width height model.items
        ]


toPixelAttr : Int -> String
toPixelAttr px =
    let
        attr =
            toString px
    in
        attr ++ "px"


displayMatrix : Width -> Height -> List Item -> Html.Html Msg
displayMatrix width height items =
    Html.div
        [ Html.Attributes.class "led"
        , Html.Attributes.style
            [ ( "width", width * 11 |> toPixelAttr )
            , ( "height", height * 11 |> toPixelAttr )
            ]
        ]
        [ Html.ul []
            (List.map (\_ -> Html.li [] []) (List.repeat (width * height) ""))
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
