module Ext.Url.Parser.Query exposing (dictFromString, dictFromUrl, uriDecode)

import Dict exposing (Dict)
import Json.Decode
import Parser exposing ((|.), (|=), Parser, chompUntil, chompUntilEndOr, getChompedString, oneOf, succeed, symbol)
import Url
import Url.Parser.Query


{-| includes treating "+" as spaces (i.e. "%20")
-}
uriDecode : String -> String
uriDecode string =
    string
        |> String.replace "+" "%20"
        |> Url.percentDecode
        |> Maybe.withDefault string


{-| Given a raw url string, return a Dict of the query

    import Dict exposing (Dict)

    expected : Dict String String
    expected =
        Dict.fromList
            [ ( "abc", "123" )
            , ( "def", "456" )
            , ( "Gh", "7 8" )
            , ( "i jk", "89 10" )
            ]

    dictFromString "http://host:1234/pa/th?abc=123&def=456&Gh=7+8&i%20jk=89%2010"
    --> expected

    dictFromString "http://host:1234/pa/th?"
    --> Dict.empty

    dictFromString "http://host:1234/pa/th"
    --> Dict.empty

-}
dictFromString : String -> Dict String String
dictFromString urlString =
    Url.fromString urlString
        |> Maybe.map dictFromUrl
        |> Maybe.withDefault Dict.empty


{-| Given a parsed url, return a Dict of the query
-}
dictFromUrl : Url.Url -> Dict String String
dictFromUrl { query } =
    case query of
        Nothing ->
            Dict.empty

        Just queryString ->
            Parser.run queryParser queryString
                |> Result.withDefault Dict.empty



--


type alias State =
    List ( String, String )


queryParser : Parser (Dict String String)
queryParser =
    Parser.loop [] queryParserHelp


queryParserHelp : State -> Parser (Parser.Step State (Dict String String))
queryParserHelp kvList =
    oneOf
        [ succeed (\k v -> Parser.Loop (( k, v ) :: kvList))
            |= paramKey
            |. symbol "="
            |= paramValue
            |. oneOf [ symbol "&", succeed () ]
        , succeed ()
            |> Parser.map (\_ -> Parser.Done (Dict.fromList kvList))
        ]


paramKey : Parser String
paramKey =
    succeed ()
        |. chompUntil "="
        |> getChompedString
        |> Parser.map uriDecode


paramValue : Parser String
paramValue =
    succeed ()
        |. chompUntilEndOr "&"
        |> getChompedString
        |> Parser.map uriDecode
