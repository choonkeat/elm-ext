module Ext.Http exposing (..)

import Http
import Json.Decode


{-| applies a json decoder to response of `Http.task`
Http.task
{ resolver = Http.stringResolver (httpJsonBodyResolver thingDecoder)
, ...
}
-}
jsonDecodeHttpResponse : Json.Decode.Decoder a -> Http.Response String -> Result Http.Error a
jsonDecodeHttpResponse decoder resp =
    case resp of
        Http.GoodStatus_ m s ->
            Json.Decode.decodeString decoder s
                |> Result.mapError (Json.Decode.errorToString >> Http.BadBody)

        Http.BadUrl_ s ->
            Err (Http.BadUrl s)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ m s ->
            if s == "" then
                Err (Http.BadStatus m.statusCode)

            else
                Err (Http.BadBody (String.fromInt m.statusCode ++ ": " ++ s))



--


{-| to obtain a String from `Http.task`
Http.task
{ resolver = Http.stringResolver httpStringBodyResolver
, ...
}
-}
httpStringBodyResolver : Http.Response String -> Result Http.Error String
httpStringBodyResolver resp =
    case resp of
        Http.GoodStatus_ m s ->
            Ok s

        Http.BadUrl_ s ->
            Err (Http.BadUrl s)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ m s ->
            if s == "" then
                Err (Http.BadStatus m.statusCode)

            else
                Err (Http.BadBody (String.fromInt m.statusCode ++ ": " ++ s))


{-| httpRawResolver makes no judgement of what is good or bad http status.
if the http request was executed, we'll get the response metadata and data
-}
httpRawResolver : Http.Response String -> Result Http.Error { metadata : Http.Metadata, data : String }
httpRawResolver resp =
    case resp of
        Http.GoodStatus_ m s ->
            Ok { metadata = m, data = s }

        Http.BadStatus_ m s ->
            Ok { metadata = m, data = s }

        Http.BadUrl_ s ->
            Err (Http.BadUrl s)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError


httpErrorString : Http.Error -> String
httpErrorString err =
    case err of
        Http.BadUrl s ->
            "Bad url: " ++ s

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus i ->
            "Unexpected status " ++ String.fromInt i

        Http.BadBody s ->
            "Unexpected response: " ++ s
