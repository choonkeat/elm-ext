module Ext.Task exposing (..)

import Task exposing (Task)


fromResult : Result x a -> Task x a
fromResult result =
    case result of
        Err x ->
            Task.fail x

        Ok a ->
            Task.succeed a


fromMaybe : x -> Maybe a -> Task x a
fromMaybe x maybeA =
    case maybeA of
        Nothing ->
            Task.fail x

        Just a ->
            Task.succeed a
