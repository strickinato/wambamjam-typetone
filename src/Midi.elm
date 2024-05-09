module Midi exposing (..)

import Json.Decode as Decode exposing (Decoder)


type Info
    = Success (List Port)
    | Failed String


type alias Port =
    { id : String
    , name : String
    }


decodeInfo : Decoder Info
decodeInfo =
    Decode.field "success" Decode.bool
        |> Decode.andThen
            (\success ->
                if success then
                    Decode.field "outputs" (Decode.list decodePort)
                        |> Decode.map Success

                else
                    Decode.field "message" Decode.string
                        |> Decode.map Failed
            )


decodePort : Decoder Port
decodePort =
    Decode.map2 Port
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
