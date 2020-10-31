module Ext.CSV exposing (format)


format : String -> String
format s =
    "\"" ++ escapeDoubleQuote s ++ "\""


escapeDoubleQuote : String -> String
escapeDoubleQuote =
    String.split "\""
        >> String.join "\"\""
