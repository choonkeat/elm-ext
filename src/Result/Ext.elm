module Result.Ext exposing (..)

import Task exposing (Task)


toTask : Result x a -> Task x a
toTask result =
    case result of
        Ok a ->
            Task.succeed a

        Err x ->
            Task.fail x
