module Ext.List exposing (..)


last : List a -> Maybe a
last =
    -- https://www.reddit.com/r/elm/comments/4j2fg6/finding_the_last_list_element/d33g6ae
    List.foldl (Just >> always) Nothing
