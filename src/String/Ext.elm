module String.Ext exposing (..)

import Base16
import Base64


{-| Makes each word between dashes Titlecase. Used to convert lowercased http headers back to normal

    dashedTitles "user-agent"
    --> "User-Agent"

    dashedTitles "x-custom-URL"
    --> "X-Custom-URL"

-}
dashedTitles : String -> String
dashedTitles string =
    String.split "-" string
        |> List.map firstUpper
        |> String.join "-"


{-| Make the first letter uppercase

    firstUpper "hello"
    --> "Hello"

    firstUpper "Nice"
    --> "Nice"

-}
firstUpper : String -> String
firstUpper string =
    case String.uncons string of
        Just ( c, str ) ->
            String.cons (Char.toUpper c) str

        Nothing ->
            string


{-| Converts a hex string to base64

    hexadecimalToBase64 "c23da3b4d5cc304a7a88fc4003d6862a689cc8a7740888534bb562a4bf46d9fb"
    --> "wj2jtNXMMEp6iPxAA9aGKmicyKd0CIhTS7VipL9G2fs="

    hexadecimalToBase64 "5c2874c6fc13182e70f7e72a0c2ba77e01c1513e26aac5bb01a53b87b8b6302f"
    --> "XCh0xvwTGC5w9+cqDCunfgHBUT4mqsW7AaU7h7i2MC8="

-}
hexadecimalToBase64 : String -> String
hexadecimalToBase64 string =
    String.toUpper string
        |> Base16.decode
        |> Result.andThen Base64.encode
        |> Result.withDefault string
