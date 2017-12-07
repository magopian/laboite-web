module TestMain exposing (..)

import Test exposing (..)
import Decoder
import Expect
import Http
import Main
import Matrix


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


mainTest : Test
mainTest =
    let
        initialModel : Main.Model
        initialModel =
            Main.initialModel

        item1 : Matrix.Item
        item1 =
            { content = (Matrix.Text "Foo")
            , x = 0
            , y = 0
            }

        slide1 : Matrix.Slide
        slide1 =
            { duration = 5
            , items = [ item1 ]
            , id = 1
            }

        item2 : Matrix.Item
        item2 =
            { content = (Matrix.Text "Bar")
            , x = 0
            , y = 0
            }

        slide2 =
            { duration = 5
            , items = [ item2 ]
            , id = 2
            }
    in
        describe "Main"
            [ test "UpdateSlideInfoList removes slides that aren't present anymore" <|
                \_ ->
                    let
                        model =
                            { initialModel | slides = [ slide1, slide2 ] }

                        slideInfoList : Matrix.SlideInfoList
                        slideInfoList =
                            [ { last_activity = 123, id = 1 } ]

                        ( updatedModel, _ ) =
                            (Main.update (Main.UpdateSlideInfoList (Ok slideInfoList)) model)
                    in
                        Expect.equal updatedModel.slides [ slide1 ]
            ]
