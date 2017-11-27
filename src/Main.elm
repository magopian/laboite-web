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


type alias LaboiteID =
    String


type alias SlideID =
    Int


width : Int
width =
    32


height : Int
height =
    16


loadingSlide : Matrix.Slide
loadingSlide =
    Matrix.Slide
        1
        [ Matrix.Item (Matrix.Text "Loading") 0 0 ]
        0
        15


init : ( Model, Cmd Msg )
init =
    ( { laboiteID = Nothing
      , slideInfoList = Nothing
      , slides = []
      , matrix = matrixFromMaybeSlide (Just loadingSlide)
      , currentSlide = Just loadingSlide
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

        withoutLoadingSlide =
            allSlides
                |> List.filter (\s -> s /= loadingSlide)
    in
        case withoutLoadingSlide of
            [] ->
                ( Just loadingSlide, [] )

            head :: tail ->
                ( Just head, tail )



---- UPDATE ----


type Msg
    = UpdateInputValue String
    | SubmitLaboiteID
    | NextSlide Time.Time
    | UpdateSlideInfoList (Result Http.Error Matrix.SlideInfoList)
    | UpdateSlide (Result Http.Error Matrix.Slide)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInputValue value ->
            ( { model | inputValue = value, error = Nothing }, Cmd.none )

        SubmitLaboiteID ->
            ( { model | laboiteID = Just model.inputValue, error = Nothing }, getSlideInfoList model.inputValue )

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
            ( { model | slideInfoList = Just slideInfoList, error = Nothing }
            , case model.laboiteID of
                Nothing ->
                    Cmd.none

                Just laboiteID ->
                    slideInfoList
                        |> List.map
                            (\slideInfo ->
                                getSlide laboiteID slideInfo.id
                            )
                        |> Cmd.batch
            )

        UpdateSlideInfoList (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        UpdateSlide (Ok slide) ->
            ( { model | slides = updateSlide slide model.slides, error = Nothing }, Cmd.none )

        UpdateSlide (Err err) ->
            let
                _ =
                    Debug.log "Failed loading slide" err
            in
                ( model, Cmd.none )


getSlideInfoList : LaboiteID -> Cmd Msg
getSlideInfoList laboiteID =
    let
        proxy =
            "https://cors-anywhere.herokuapp.com/"

        url =
            proxy ++ "http://dev.laboite.pro/boites/" ++ laboiteID ++ "/"

        request =
            Http.get url Decoder.slideInfoListDecoder
    in
        Http.send UpdateSlideInfoList request


getSlide : LaboiteID -> SlideID -> Cmd Msg
getSlide laboiteID slideID =
    let
        proxy =
            "https://cors-anywhere.herokuapp.com/"

        url =
            proxy ++ "http://dev.laboite.pro/boites/" ++ laboiteID ++ "/tiles/" ++ (toString slideID) ++ "/"

        request =
            Http.get url Decoder.slideDecoder
    in
        Http.send UpdateSlide request


updateSlide : Matrix.Slide -> List Matrix.Slide -> List Matrix.Slide
updateSlide slide slides =
    let
        filteredSlides =
            slides
                |> List.filter (\s -> s /= slide)
    in
        List.append slides [ slide ]



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
