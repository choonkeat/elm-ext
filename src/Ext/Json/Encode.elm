module Ext.Json.Encode exposing (..)

import Json.Encode


{-| Decode a nested JSON object, requiring certain fields.

    import Json.Encode exposing (..)

    encodedValue : Json.Encode.Value
    encodedValue =
        (Json.Encode.string "Elm Street")

    at [ "Nightmare", "At" ] encodedValue
        |> Json.Encode.encode 0
    --> "{\"Nightmare\":{\"At\":\"Elm Street\"}}"

This is really just a shorthand for:

    Json.Encode.object [ ( "Nightmare", Json.Encode.object [ ( "At", encodedValue ) ] ) ]
        |> Json.Encode.encode 0

-}
at : List String -> Json.Encode.Value -> Json.Encode.Value
at keys value =
    case keys of
        [] ->
            value

        [ x ] ->
            Json.Encode.object [ ( x, value ) ]

        x :: xs ->
            Json.Encode.object [ ( x, at xs value ) ]


{-| Convenience function to encode a tuple

    import Json.Encode exposing (..)

    tuple Json.Encode.int Json.Encode.string (1, "hello")
        |> Json.Encode.encode 0
    --> "[1,\"hello\"]"

-}
tuple : (a -> Json.Encode.Value) -> (b -> Json.Encode.Value) -> ( a, b ) -> Json.Encode.Value
tuple encoderA encoderB ( a, b ) =
    Json.Encode.list identity [ encoderA a, encoderB b ]
