module TestMain exposing (..)

import Test exposing (..)
import Expect
import Kinto
import KintoDecoders
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

        slide1 : KintoDecoders.Slide
        slide1 =
            { id = "slide 1"
            , last_modified = 123
            , display_time = 5
            , items = [ item1 ]
            }

        item2 : Matrix.Item
        item2 =
            { content = (Matrix.Text "Bar")
            , x = 0
            , y = 0
            }

        slide2 : KintoDecoders.Slide
        slide2 =
            { id = "slide 2"
            , last_modified = 123
            , display_time = 5
            , items = [ item2 ]
            }
    in
        describe "Main"
            [ test "UpdateSlideList removes slides that aren't present anymore" <|
                \_ ->
                    let
                        model =
                            { initialModel | slides = [ slide1, slide2 ] }

                        pager : Kinto.Pager KintoDecoders.Slide
                        pager =
                            { client = KintoDecoders.client
                            , objects = [ slide1 ]
                            , decoder = KintoDecoders.slideListDecoder
                            , total = 1
                            , nextPage = Nothing
                            }

                        ( updatedModel, _ ) =
                            (Main.update (Main.UpdateSlideList (Ok pager)) model)
                    in
                        Expect.equal updatedModel.slides [ slide1 ]
            , test "getSlidesToUpdate only returns slide IDs that were updated" <|
                \_ ->
                    let
                        updatedSlide2 : KintoDecoders.Slide
                        updatedSlide2 =
                            { slide2 | last_modified = 124 }
                    in
                        Expect.equal
                            (Main.getSlidesToUpdate [ slide1, updatedSlide2 ] [ slide1, slide2 ])
                            [ "slide 2" ]
            ]
