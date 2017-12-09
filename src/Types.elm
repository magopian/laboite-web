module Types exposing (..)


type alias Slide =
    { id : String
    , last_modified : Int
    , display_time : Int
    , items : List Item
    }


type alias Item =
    { content : ItemContent
    , y : Int
    , x : Int
    }


type ItemContent
    = Text String
    | Icon String Width


type alias Width =
    Int


type alias Height =
    Int
