module Ext.JsonWebToken exposing (..)

{-| Implements <https://tools.ietf.org/html/rfc7515#appendix-C>

But does not implement

    s = s.Replace('+', '-'); // 62nd char of encoding
    s = s.Replace('/', '_'); // 63rd char of encoding

since it screws up the data?

-}

import Json.Decode
import Json.Encode
import JsonWebToken


encode : JsonWebToken.Alg -> (a -> Json.Encode.Value) -> String -> a -> JsonWebToken.Token
encode algo encoder jwtSecret token =
    let
        base64WithoutPadding s =
            s
                |> String.split "."
                |> List.map (\part -> String.join "=" (List.take 1 (String.split "=" part)))
                |> String.join "."
    in
    JsonWebToken.encode algo encoder jwtSecret token
        |> base64WithoutPadding


decode : Json.Decode.Decoder a -> String -> JsonWebToken.Token -> Result (JsonWebToken.DecodeError a) a
decode decoder jwtSecret jwtString =
    let
        base64RestorePadding s =
            case modBy 4 (String.length s) of
                2 ->
                    s ++ "=="

                3 ->
                    s ++ "="

                _ ->
                    s

        base64Restore6362 s =
            s
                |> String.split "."
                |> List.map base64RestorePadding
                |> String.join "."
    in
    base64Restore6362 jwtString
        |> JsonWebToken.decode decoder jwtSecret
