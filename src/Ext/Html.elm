module Ext.Html exposing (..)

import Html exposing (Html, text)


maybe : (a -> Html msg) -> Maybe a -> Html msg
maybe view maybeThing =
    Maybe.map view maybeThing
        |> Maybe.withDefault (text "")
