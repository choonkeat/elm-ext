module Ext.Task exposing (..)

import Task exposing (Task)
import Time


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


nowAndThen : (Time.Posix -> a -> Task x b) -> Task x a -> Task x b
nowAndThen f task =
    Task.map2 Tuple.pair task Time.now
        |> Task.andThen (\( a, t ) -> f t a)
