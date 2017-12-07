module Main exposing (..)

import Array.Hamt as Array
import Decoder
import Html
import Html.Attributes
import Html.Events
import Http
import Matrix
import Navigation
import Time
import UrlParser exposing ((<?>))


---- MODEL ----


type alias Model =
    { laboiteID : Maybe String
    , slideInfoList : Maybe Matrix.SlideInfoList
    , slides : List Matrix.Slide
    , matrix : Matrix.Matrix
    , inputValue : String
    , error : Maybe Http.Error
    , tick : Int
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
        60
        [ Matrix.Item (Matrix.Text "Loading") 0 0 ]
        0


initialModel : Model
initialModel =
    { laboiteID = Nothing
    , slideInfoList = Nothing
    , slides = []
    , matrix = matrixFromSlides 0 []
    , inputValue = ""
    , error = Nothing
    , tick = 0
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( initialModel
    , parseUrl location
        |> newUrl
    )


parseUrl : Navigation.Location -> Maybe LaboiteID
parseUrl location =
    let
        { laboiteID } =
            UrlParser.parseHash (UrlParser.map QueryData queryParser) location
                |> Maybe.withDefault { laboiteID = Nothing }
    in
        laboiteID


type alias QueryData =
    { laboiteID : Maybe LaboiteID }


queryParser : UrlParser.Parser (Maybe LaboiteID -> a) a
queryParser =
    UrlParser.top
        <?> UrlParser.stringParam "laboiteID"


urlFromData : LaboiteID -> String
urlFromData laboiteID =
    "?laboiteID=" ++ laboiteID


newUrl : Maybe LaboiteID -> Cmd Msg
newUrl maybeLaboiteID =
    case maybeLaboiteID of
        Nothing ->
            Cmd.none

        Just laboiteID ->
            Navigation.newUrl (urlFromData laboiteID)


matrixFromSlides : Int -> List Matrix.Slide -> Matrix.Matrix
matrixFromSlides tick slides =
    let
        slide =
            getDisplaySlide slides

        matrix =
            Matrix.empty 32 16
    in
        matrix
            |> Matrix.itemsToMatrix tick slide.items


cycleSlides : List Matrix.Slide -> List Matrix.Slide
cycleSlides slides =
    case slides of
        [] ->
            []

        head :: tail ->
            List.append tail [ head ]



---- UPDATE ----


type Msg
    = UpdateInputValue String
    | SubmitLaboiteID
    | NextSlide Time.Time
    | UpdateSlideInfoList (Result Http.Error Matrix.SlideInfoList)
    | UpdateSlide (Result Http.Error Matrix.Slide)
    | UrlChange Navigation.Location
    | NewTick Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInputValue value ->
            ( { model | inputValue = value, error = Nothing }, Cmd.none )

        SubmitLaboiteID ->
            ( { model | laboiteID = Just model.inputValue, error = Nothing }, newUrl (Just model.inputValue) )

        NextSlide _ ->
            let
                cycledSlides =
                    cycleSlides model.slides
            in
                ( { model
                    | slides = cycledSlides
                    , matrix = matrixFromSlides 0 cycledSlides
                    , tick = 0
                  }
                , case model.laboiteID of
                    Just laboiteID ->
                        requestSlideInfoList laboiteID

                    _ ->
                        Cmd.none
                )

        UpdateSlideInfoList (Ok slideInfoList) ->
            let
                slidesIdList =
                    slideInfoList
                        |> List.map (\s -> s.id)

                updatedSlides =
                    -- Remove slides that may not be present anymore
                    model.slides
                        |> List.filter (\s -> List.member s.id slidesIdList)
            in
                ( { model | slideInfoList = Just slideInfoList, slides = updatedSlides, error = Nothing }
                , case model.laboiteID of
                    Nothing ->
                        Cmd.none

                    Just laboiteID ->
                        slideInfoList
                            |> List.map
                                (\slideInfo ->
                                    requestSlide laboiteID slideInfo.id
                                )
                            |> Cmd.batch
                )

        UpdateSlideInfoList (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        UpdateSlide (Ok slide) ->
            let
                newSlides =
                    updateSlide slide model.slides

                ( newMatrix, newTick ) =
                    case model.slides of
                        [] ->
                            -- No slide yet: immediately replace the loading slide
                            ( matrixFromSlides 0 newSlides, 0 )

                        _ ->
                            ( model.matrix, model.tick )
            in
                ( { model | slides = newSlides, matrix = newMatrix, error = Nothing, tick = newTick }, Cmd.none )

        UpdateSlide (Err err) ->
            let
                _ =
                    Debug.log "Failed loading slide" err
            in
                ( model, Cmd.none )

        UrlChange location ->
            let
                maybeLaboiteID =
                    parseUrl location
            in
                ( { model | laboiteID = maybeLaboiteID, inputValue = Maybe.withDefault "" maybeLaboiteID }
                , case maybeLaboiteID of
                    Nothing ->
                        Cmd.none

                    Just laboiteID ->
                        requestSlideInfoList laboiteID
                )

        NewTick _ ->
            let
                newTick =
                    model.tick + 1
            in
                ( { model | tick = newTick, matrix = matrixFromSlides newTick model.slides }, Cmd.none )


getDisplaySlide : List Matrix.Slide -> Matrix.Slide
getDisplaySlide slides =
    slides
        |> List.head
        |> Maybe.withDefault loadingSlide


requestSlideInfoList : LaboiteID -> Cmd Msg
requestSlideInfoList laboiteID =
    let
        url =
            "https://lenuage.redgick.com/boites/" ++ laboiteID ++ "/"

        request =
            Http.get url Decoder.slideInfoListDecoder
    in
        Http.send UpdateSlideInfoList request


requestSlide : LaboiteID -> SlideID -> Cmd Msg
requestSlide laboiteID slideID =
    let
        url =
            "https://lenuage.redgick.com/boites/" ++ laboiteID ++ "/tiles/" ++ (toString slideID) ++ "/"

        request =
            Http.get url Decoder.slideDecoder
    in
        Http.send UpdateSlide request


updateSlide : Matrix.Slide -> List Matrix.Slide -> List Matrix.Slide
updateSlide slide slides =
    let
        updatedSlides =
            slides
                |> List.map
                    (\s ->
                        if s.id == slide.id then
                            -- update the slide
                            slide
                        else
                            s
                    )
    in
        if List.member slide updatedSlides then
            -- The slide was in the list of slides and was updated
            updatedSlides
        else
            -- The slide wasn't in the list, so add it
            List.append updatedSlides [ slide ]



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
                , Html.Attributes.value model.inputValue
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
    Html.div [ Html.Attributes.class "matrix-container" ]
        [ Html.div [ Html.Attributes.class "matrix" ]
            [ displayMatrix width height model.matrix
            ]
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
        [ Html.Attributes.class "led" ]
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
    Navigation.program UrlChange
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
            let
                slide =
                    getDisplaySlide model.slides
            in
                Sub.batch
                    [ Time.every ((toFloat slide.duration) * Time.second) NextSlide
                    , Time.every (50 * Time.millisecond) NewTick
                    ]
