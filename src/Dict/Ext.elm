module Dict.Ext exposing (..)

import Dict exposing (Dict)


{-|

     import Dict

     dropMaybe (Dict.fromList [("a", Nothing), ("b", Just 1), ("c", Just 42)])
     --> Dict.fromList [("b",1), ("c",42)]

-}
dropMaybe : Dict comparable (Maybe a) -> Dict comparable a
dropMaybe dictMaybe =
    let
        withoutNothing ( k, mv ) dict =
            case mv of
                Nothing ->
                    dict

                Just v ->
                    Dict.insert k v dict
    in
    dictMaybe
        |> Dict.toList
        |> List.foldl withoutNothing Dict.empty


merge : Dict comparable b -> Dict comparable b -> Dict comparable b
merge dict1 dict2 =
    List.append (Dict.toList dict1) (Dict.toList dict2)
        |> Dict.fromList


{-|

    import Dict

    unnest "key" (Dict.fromList [("key[child1]", "1"), ("hello", "world"), ("key[child2]", "two")])
    --> Dict.fromList [("child1", "1"), ("child2", "two")]

-}
unnest : String -> Dict String String -> Dict String String
unnest key dict =
    Dict.foldl
        (\k v acc ->
            if String.startsWith (key ++ "[") k && String.endsWith "]" k then
                let
                    childKey =
                        String.dropRight 1 (String.dropLeft (String.length key + 1) k)
                in
                Dict.insert childKey v acc

            else
                acc
        )
        Dict.empty
        dict


{-|

    import Dict

    without ["b", "a"] (Dict.fromList [("a", Nothing), ("b", Just 1), ("c", Just 42)])
    --> Dict.fromList [("c",Just 42)]

-}
without : List comparable -> Dict comparable a -> Dict comparable a
without blacklist dict =
    Dict.foldl
        (\k v acc ->
            if List.member k blacklist then
                acc

            else
                Dict.insert k v acc
        )
        Dict.empty
        dict
