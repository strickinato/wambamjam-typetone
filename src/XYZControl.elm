module XYZControl exposing (..)

import Json.Encode as Encode


type alias XYZControl =
    { x : Float, y : Float, z : Float }


encode : XYZControl -> Encode.Value
encode { x, y, z } =
    Encode.object
        [ ( "x", Encode.float x )
        , ( "y", Encode.float y )
        , ( "z", Encode.float z )
        ]
