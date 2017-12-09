module KintoDecoders exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Kinto
import Types


-- Configure a Kinto client: the server url and authentication


client : Kinto.Client
client =
    Kinto.client
        "https://kinto.agopian.info/v1/"
        (Kinto.Basic "test" "test")



-- Resources


slideResource : String -> Kinto.Resource Types.Slide
slideResource laboiteID =
    Kinto.recordResource "laboite" laboiteID slideDecoder



-- Decoders


textItemDecoder : Decode.Decoder Types.Item
textItemDecoder =
    Pipeline.decode textToMatrixItem
        |> Pipeline.required "type" Decode.string
        |> Pipeline.required "x" Decode.int
        |> Pipeline.required "y" Decode.int
        |> Pipeline.required "text" Decode.string
        |> Pipeline.resolve


textToMatrixItem : String -> Int -> Int -> String -> Decode.Decoder Types.Item
textToMatrixItem type_ x y content =
    Decode.succeed
        { content = Types.Text content
        , x = x
        , y = y
        }


iconItemDecoder : Decode.Decoder Types.Item
iconItemDecoder =
    Pipeline.decode iconToMatrixItem
        |> Pipeline.required "type" Decode.string
        |> Pipeline.required "x" Decode.int
        |> Pipeline.required "y" Decode.int
        |> Pipeline.required "content" Decode.string
        |> Pipeline.required "width" Decode.int
        |> Pipeline.resolve


iconToMatrixItem : String -> Int -> Int -> String -> Int -> Decode.Decoder Types.Item
iconToMatrixItem type_ x y content width =
    Decode.succeed
        { content = Types.Icon content width
        , x = x
        , y = y
        }


itemDecoder : Decode.Decoder Types.Item
itemDecoder =
    Decode.oneOf
        [ textItemDecoder
        , iconItemDecoder
        ]


slideDecoder : Decode.Decoder Types.Slide
slideDecoder =
    Pipeline.decode Types.Slide
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "last_modified" Decode.int
        |> Pipeline.required "display_time" Decode.int
        |> Pipeline.required "items" (Decode.list itemDecoder)


slideListDecoder : Decode.Decoder (List Types.Slide)
slideListDecoder =
    Decode.at [ "data" ] (Decode.list slideDecoder)


getSlide : String -> String -> Kinto.Request Types.Slide
getSlide laboiteID slideID =
    client
        |> Kinto.get (slideResource laboiteID) slideID


getSlides : String -> Kinto.Request (Kinto.Pager Types.Slide)
getSlides laboiteID =
    client
        |> Kinto.getList (slideResource laboiteID)


pagerFromSlides : List Types.Slide -> Kinto.Pager Types.Slide
pagerFromSlides slides =
    { client = client
    , objects = slides
    , decoder = slideListDecoder
    , total = List.length slides
    , nextPage = Nothing
    }
