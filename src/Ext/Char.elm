module Ext.Char exposing (..)


isNewLine : Char.Char -> Bool
isNewLine char =
    case char of
        '\n' ->
            True

        '\u{000D}' ->
            True

        _ ->
            False


isWhitespace : Char.Char -> Bool
isWhitespace char =
    case char of
        '\n' ->
            True

        '\u{000D}' ->
            True

        ' ' ->
            True

        '\t' ->
            True

        '\u{0008}' ->
            True

        _ ->
            False
