module Main exposing (..)

import Array
import Html
import Html.Attributes
import Matrix
import Time


---- MODEL ----


type alias Model =
    { slides : List Matrix.Slide
    , matrix : Matrix.Matrix
    , currentSlide : Maybe Matrix.Slide
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
            [ { duration = 2
              , items =
                    [ { content = Matrix.Icon "0xff839999839f9f9fff" 8 9
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
            , { duration = 1
              , items =
                    [ { content = Matrix.Icon "0xff839999839f9f9fff" 8 9
                      , y = 7
                      , x = 4
                      }
                    , { content = Matrix.Text "Foooo!"
                      , y = 0
                      , x = 0
                      }
                    ]
              , id = 39
              , brightness = 15
              }
            , { duration = 5
              , items =
                    [ { content = Matrix.Text "16:09"
                      , y = 0
                      , x = 4
                      }
                    , { content = Matrix.Icon "0xff839999839f9f9fff" 8 9
                      , y = 8
                      , x = 3
                      }
                    , { content = Matrix.Text "248"
                      , y = 10
                      , x = 12
                      }
                    ]
              , id = 50
              , brightness = 15
              }
            ]

        ( currentSlide, remainingSlides ) =
            nextSlide Nothing data
    in
        ( { slides = remainingSlides
          , matrix = matrixFromMaybeSlide currentSlide
          , currentSlide = currentSlide
          }
        , Cmd.none
        )


matrixFromMaybeSlide : Maybe Matrix.Slide -> Matrix.Matrix
matrixFromMaybeSlide maybeSlide =
    let
        matrix =
            Matrix.empty 32 16
    in
        case maybeSlide of
            Just slide ->
                matrix
                    |> Matrix.itemsToMatrix slide.items

            _ ->
                matrix


nextSlide : Maybe Matrix.Slide -> List Matrix.Slide -> ( Maybe Matrix.Slide, List Matrix.Slide )
nextSlide currentMaybeSlide currentRemainingSlides =
    let
        allSlides =
            case currentMaybeSlide of
                Just currentSlide ->
                    List.append currentRemainingSlides [ currentSlide ]

                _ ->
                    currentRemainingSlides
    in
        case allSlides of
            [] ->
                ( Nothing, [] )

            head :: tail ->
                ( Just head, tail )



---- UPDATE ----


type Msg
    = NewSlide Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewSlide _ ->
            let
                ( currentSlide, remainingSlides ) =
                    nextSlide model.currentSlide model.slides
            in
                ( { model
                    | slides = remainingSlides
                    , currentSlide = currentSlide
                    , matrix = matrixFromMaybeSlide currentSlide
                  }
                , Cmd.none
                )



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
        , subscriptions = subscriptions
        }



---- Subscriptions ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentSlide of
        Just slide ->
            Time.every ((toFloat slide.duration) * Time.second) NewSlide

        _ ->
            Sub.none
