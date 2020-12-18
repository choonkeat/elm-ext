module Ext.Json.Decode exposing
    ( asString
    , filterOk
    , fromResult
    , keyword
    , listOk
    , stringAs
    , timePosix
    )

import Ext.Maybe
import Ext.Time
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


filterOk : Json.Decode.Decoder a -> List Json.Decode.Value -> List a
filterOk decoder values =
    List.foldr
        (\value acc ->
            case Json.Decode.decodeValue decoder value of
                Err _ ->
                    acc

                Ok a ->
                    a :: acc
        )
        []
        values


timePosix : Json.Decode.Decoder Time.Posix
timePosix =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                let
                    maybeTimePosix =
                        List.foldl
                            (\convert maybe -> Ext.Maybe.otherwise (\() -> convert string) maybe)
                            Nothing
                            [ Ext.Time.maybeTimeFromIso8601
                            , Ext.Time.maybeTimeFromRuby
                            , Ext.Time.maybeTimeFromInternetMessageFormat
                            ]
                in
                case maybeTimePosix of
                    Just a ->
                        Json.Decode.succeed a

                    Nothing ->
                        Json.Decode.fail ("Invalid date time: " ++ string)
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


{-| decode a string and require the string to be an exact value, otherwise fail
-}
keyword : String -> Json.Decode.Decoder ()
keyword expected =
    Json.Decode.string
        |> Json.Decode.andThen
            (\actual ->
                if expected == actual then
                    Json.Decode.succeed ()

                else
                    Json.Decode.fail actual
            )


stringAs : Json.Decode.Decoder a -> Json.Decode.Decoder a
stringAs decoder =
    Json.Decode.string
        |> Json.Decode.andThen (Json.Decode.decodeString decoder >> fromResult)
