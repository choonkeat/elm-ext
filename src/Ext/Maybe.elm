module Ext.Maybe exposing (..)

import Task exposing (Task)


toTask : x -> Maybe a -> Task x a
toTask x =
    Maybe.map Task.succeed >> Maybe.withDefault (Task.fail x)


otherwise : (() -> Maybe a) -> Maybe a -> Maybe a
otherwise lazyDefaultMaybe currentMaybe =
    case currentMaybe of
        Just _ ->
            currentMaybe

        Nothing ->
            lazyDefaultMaybe ()
