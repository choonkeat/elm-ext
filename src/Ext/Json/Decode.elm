module Ext.Json.Decode exposing (asString, fromResult, listOk, timePosix)

import Imf.DateTime
import Iso8601
import Json.Decode
import Time


prependOk : Json.Decode.Decoder a -> Json.Decode.Value -> List a -> List a
prependOk decoder value list =
    case Json.Decode.decodeValue decoder value of
        Ok a ->
            a :: list

        Err a ->
            list


{-| like `Json.Decode.list` but instead of failing when any item fail to decode,
we simply drop the item from the list. This decoder would always succeed unless
the json value is not a List
-}
listOk : Json.Decode.Decoder a -> Json.Decode.Decoder (List a)
listOk decoder =
    Json.Decode.list Json.Decode.value
        |> Json.Decode.map (List.foldr (prependOk decoder) [])


timePosix : Json.Decode.Decoder Time.Posix
timePosix =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                let
                    sanitizedString =
                        if String.endsWith " UTC" string then
                            -- convert `2014-05-17 05:03:17 UTC`
                            -- into    `2014-05-17T05:03:17Z`
                            String.split " " string
                                |> List.take 2
                                |> String.join "T"
                                |> (\s -> s ++ "Z")

                        else
                            string
                in
                case Iso8601.toTime sanitizedString of
                    Ok iso8601value ->
                        Json.Decode.succeed iso8601value

                    _ ->
                        case Imf.DateTime.toPosix string of
                            Ok rfc2822value ->
                                Json.Decode.succeed rfc2822value

                            _ ->
                                Json.Decode.fail ("Invalid ISO 8601 or RFC 2822 format: " ++ string)
            )


fromResult : Result Json.Decode.Error a -> Json.Decode.Decoder a
fromResult result =
    case result of
        Ok a ->
            Json.Decode.succeed a

        Err err ->
            Json.Decode.fail (Json.Decode.errorToString err)


{-| Decodes a json string, int, or float value as String
-}
asString : Json.Decode.Decoder String
asString =
    Json.Decode.oneOf
        [ Json.Decode.string
        , Json.Decode.int |> Json.Decode.map String.fromInt
        , Json.Decode.float |> Json.Decode.map String.fromFloat
        ]
