module TestMain exposing (..)

import Test exposing (..)
import Expect
import KintoDecoders
import Main
import Types


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


mainTest : Test
mainTest =
    let
        initialModel : Main.Model
        initialModel =
            Main.initialModel

        item1 : Types.Item
        item1 =
            { content = (Types.Text "Foo")
            , x = 0
            , y = 0
            }

        slide1 : Types.Slide
        slide1 =
            { id = "slide 1"
            , last_modified = 123
            , display_time = 5
            , items = [ item1 ]
            }

        item2 : Types.Item
        item2 =
            { content = (Types.Text "Bar")
            , x = 0
            , y = 0
            }

        slide2 : Types.Slide
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

                        pager =
                            KintoDecoders.pagerFromSlides [ slide1 ]

                        ( updatedModel, _ ) =
                            (Main.update (Main.UpdateSlideList (Ok pager)) model)
                    in
                        Expect.equal updatedModel.slides [ slide1 ]
            , test "getSlidesToUpdate only returns slide IDs that were updated" <|
                \_ ->
                    let
                        updatedSlide2 : Types.Slide
                        updatedSlide2 =
                            { slide2 | last_modified = 124 }
                    in
                        Expect.equal
                            (Main.getSlidesToUpdate [ slide1, updatedSlide2 ] [ slide1, slide2 ])
                            [ "slide 2" ]
            ]
