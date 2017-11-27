module Main exposing (..)

import Array.Hamt as Array
import Decoder
import Html
import Html.Attributes
import Html.Events
import Http
import Matrix
import Time


---- MODEL ----


type alias Model =
    { laboiteID : Maybe String
    , slideInfoList : Maybe Matrix.SlideInfoList
    , slides : List Matrix.Slide
    , matrix : Matrix.Matrix
    , currentSlide : Maybe Matrix.Slide
    , inputValue : String
    , error : Maybe Http.Error
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
                    , { content = Matrix.Text "Hello !"
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
            ]

        decodedSlide =
            Decoder.decodeSlide

        newData =
            case decodedSlide of
                Ok slide ->
                    slide :: data

                Err msg ->
                    Debug.crash msg

        ( currentSlide, remainingSlides ) =
            nextSlide Nothing newData
    in
        ( { laboiteID = Nothing
          , slideInfoList = Nothing
          , slides = remainingSlides
          , matrix = matrixFromMaybeSlide currentSlide
          , currentSlide = currentSlide
          , inputValue = ""
          , error = Nothing
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
    = UpdateInputValue String
    | SubmitLaboiteID
    | NextSlide Time.Time
    | UpdateSlideInfoList (Result Http.Error Matrix.SlideInfoList)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInputValue value ->
            ( { model | inputValue = value, error = Nothing }, Cmd.none )

        SubmitLaboiteID ->
            ( { model | laboiteID = Just model.inputValue, error = Nothing }, getLaboiteSlideInfos model.inputValue )

        NextSlide _ ->
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

        UpdateSlideInfoList (Ok slideInfoList) ->
            ( { model | slideInfoList = Just slideInfoList, error = Nothing }, Cmd.none )

        UpdateSlideInfoList (Err err) ->
            ( { model | error = Just err }, Cmd.none )


getLaboiteSlideInfos : String -> Cmd Msg
getLaboiteSlideInfos laboiteID =
    let
        proxy =
            "https://cors-anywhere.herokuapp.com/"

        url =
            proxy ++ "http://dev.laboite.pro/boites/" ++ laboiteID ++ "/"

        request =
            Http.get url Decoder.slideInfoListDecoder
    in
        Http.send UpdateSlideInfoList request



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    case model.slideInfoList of
        Nothing ->
            viewGetLaboiteID model

        Just _ ->
            viewSlides model


viewGetLaboiteID : Model -> Html.Html Msg
viewGetLaboiteID model =
    Html.form [ Html.Events.onSubmit SubmitLaboiteID ]
        [ Html.label []
            [ Html.text "Laboite ID: "
            , Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.placeholder "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
                , Html.Attributes.size 36
                , Html.Events.onInput UpdateInputValue
                ]
                []
            ]
        , Html.input
            [ Html.Attributes.type_ "submit"
            , Html.Attributes.value "Show me the slides"
            ]
            []
        , Html.p []
            (case model.error of
                Nothing ->
                    case model.laboiteID of
                        Nothing ->
                            []

                        Just _ ->
                            [ Html.text "Loading slide informations..." ]

                Just err ->
                    [ Html.text <| toString err ]
            )
        ]


viewSlides : Model -> Html.Html Msg
viewSlides model =
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
    case model.laboiteID of
        Nothing ->
            Sub.none

        Just laboiteID ->
            case model.currentSlide of
                Just slide ->
                    Time.every ((toFloat slide.duration) * Time.second) NextSlide

                _ ->
                    Sub.none
