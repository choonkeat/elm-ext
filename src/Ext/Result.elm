module Ext.Result exposing (..)

import Task exposing (Task)


toTask : Result x a -> Task x a
toTask result =
    case result of
        Ok a ->
            Task.succeed a

        Err x ->
            Task.fail x


okValues : List (Result x a) -> List a
okValues =
    List.foldr (\rx acc -> Result.withDefault acc (Result.map (\x -> x :: acc) rx)) []
