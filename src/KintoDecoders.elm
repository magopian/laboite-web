module KintoDecoders exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Kinto
import Matrix


-- Configure a Kinto client: the server url and authentication


client : Kinto.Client
client =
    Kinto.client
        "https://kinto.agopian.info/v1/"
        (Kinto.Basic "test" "test")



-- Resources


slideResource : String -> Kinto.Resource Slide
slideResource laboiteID =
    Kinto.recordResource "laboite" laboiteID slideDecoder



-- Decoders


type alias Slide =
    { id : String
    , display_time : Int
    , last_modified : Int
    , items : List Matrix.Item
    }


textItemDecoder : Decode.Decoder Matrix.Item
textItemDecoder =
    Pipeline.decode toMatrixItem
        |> Pipeline.required "type" Decode.string
        |> Pipeline.required "x" Decode.int
        |> Pipeline.required "y" Decode.int
        |> Pipeline.required "text" Decode.string
        |> Pipeline.resolve


toMatrixItem : String -> Int -> Int -> String -> Decode.Decoder Matrix.Item
toMatrixItem type_ x y content =
    Decode.succeed
        { content = Matrix.Text content
        , x = x
        , y = y
        }


slideDecoder : Decode.Decoder Slide
slideDecoder =
    Pipeline.decode Slide
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "display_time" Decode.int
        |> Pipeline.required "last_modified" Decode.int
        |> Pipeline.required "items" (Decode.list textItemDecoder)


slideListDecoder : Decode.Decoder (List Slide)
slideListDecoder =
    Decode.at [ "data" ] (Decode.list slideDecoder)


getSlide : String -> String -> Kinto.Request Slide
getSlide laboiteID slideID =
    client
        |> Kinto.get (slideResource laboiteID) slideID


getSlides : String -> Kinto.Request (Kinto.Pager Slide)
getSlides laboiteID =
    client
        |> Kinto.getList (slideResource laboiteID)
