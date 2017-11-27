module Decoder exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Matrix


itemDecoder : Decode.Decoder Matrix.Item
itemDecoder =
    Pipeline.decode toMatrixItem
        |> Pipeline.required "content" Decode.string
        |> Pipeline.required "y" Decode.int
        |> Pipeline.required "x" Decode.int
        |> Pipeline.required "width" Decode.int
        |> Pipeline.required "height" Decode.int
        |> Pipeline.required "type" Decode.string
        |> Pipeline.resolve


toMatrixItem : String -> Int -> Int -> Int -> Int -> String -> Decode.Decoder Matrix.Item
toMatrixItem content y x width height type_ =
    case type_ of
        "text" ->
            Decode.succeed
                { content = Matrix.Text content
                , y = y
                , x = x
                }

        "icon" ->
            Decode.succeed
                { content = Matrix.Icon content width height
                , y = y
                , x = x
                }

        _ ->
            Decode.fail "Couldn't interpret item"


slideDecoder : Decode.Decoder Matrix.Slide
slideDecoder =
    Pipeline.decode Matrix.Slide
        |> Pipeline.required "duration" Decode.int
        |> Pipeline.required "items" (Decode.list itemDecoder)
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "brightness" Decode.int


slideInfoDecoder : Decode.Decoder Matrix.SlideInfo
slideInfoDecoder =
    Pipeline.decode Matrix.SlideInfo
        |> Pipeline.required "last_activity" Decode.int
        |> Pipeline.required "id" Decode.int


slideInfoListDecoder : Decode.Decoder Matrix.SlideInfoList
slideInfoListDecoder =
    Decode.at [ "tiles" ] (Decode.list slideInfoDecoder)


{-| Only here for reference/mocking
-}
decodeSlide : Result String Matrix.Slide
decodeSlide =
    Decode.decodeString
        slideDecoder
        """
        {
          "duration": 5,
          "items": [
            {
              "height": 8,
              "content": "16:09",
              "width": 25,
              "y": 0,
              "x": 4,
              "type": "text"
            },
            {
              "height": 9,
              "content": "0xff839999839f9f9fff",
              "width": 8,
              "y": 8,
              "x": 3,
              "type": "icon"
            },
            {
              "height": 8,
              "content": "248",
              "width": 15,
              "y": 10,
              "x": 12,
              "type": "text"
            }
          ],
          "id": 50,
          "brightness": 15
        }
    """


{-| Only here for reference/mocking
-}
decodeSlideInfos : Result String Matrix.SlideInfoList
decodeSlideInfos =
    Decode.decodeString
        slideInfoListDecoder
        """
    {
      "tiles": [
        {
          "last_activity": 1511714267,
          "id": 48
        },
        {
          "last_activity": 1511714267,
          "id": 49
        },
        {
          "last_activity": 1511714268,
          "id": 50
        }
      ],
      "id": 7
    }
    """
