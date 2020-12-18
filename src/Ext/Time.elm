module Ext.Time exposing (..)

import DateFormat
import Imf.DateTime
import Iso8601
import Time


{-|

    import Time

    timezoneOffset : Int
    timezoneOffset =
        480

    tz : Time.Zone
    tz =
        Time.customZone timezoneOffset []

    now : Time.Posix
    now =
        -- (Time.parse "2019-05-10 12:34:56 +0800").to_i * 1000
        Time.millisToPosix 1557462896000

    twothirty : Time.Posix
    twothirty =
        -- (Time.parse "2019-05-10 14:30:00 +0800").to_i * 1000
        Time.millisToPosix 1557469800000

    formatTimeFull tz (at { hh = 14, mm = 30, timezoneOffset = timezoneOffset } now)
    --> "2:30pm Friday, 10th May 2019"

-}
at : { hh : Int, mm : Int, timezoneOffset : Int } -> Time.Posix -> Time.Posix
at { hh, mm, timezoneOffset } now =
    let
        day =
            24 * 3600 * 1000

        tzOffsetMillis =
            timezoneOffset * 60 * 1000

        midnightTodayMillis =
            -- zero out the hh:mm:ss.sss of `now`
            ((Time.posixToMillis now + tzOffsetMillis) // day) * day

        hhmmMillis =
            ((hh * 3600) + (mm * 60)) * 1000
    in
    Time.millisToPosix (midnightTodayMillis - tzOffsetMillis + hhmmMillis)


formatTimeShort : Time.Zone -> Time.Posix -> String
formatTimeShort =
    DateFormat.format
        [ DateFormat.hourNumber
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        , DateFormat.amPmLowercase
        ]


formatTimeFull : Time.Zone -> Time.Posix -> String
formatTimeFull =
    DateFormat.format
        [ DateFormat.hourNumber
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        , DateFormat.amPmLowercase
        , DateFormat.text " "
        , DateFormat.dayOfWeekNameFull
        , DateFormat.text ", "
        , DateFormat.dayOfMonthSuffix
        , DateFormat.text " "
        , DateFormat.monthNameFull
        , DateFormat.text " "
        , DateFormat.yearNumber
        ]


formatDate : Time.Zone -> Time.Posix -> String
formatDate =
    DateFormat.format
        [ DateFormat.dayOfWeekNameFull
        , DateFormat.text ", "
        , DateFormat.dayOfMonthSuffix
        , DateFormat.text " "
        , DateFormat.monthNameFull
        , DateFormat.text " "
        , DateFormat.yearNumber
        ]


yyyymmdd : Time.Zone -> Time.Posix -> String
yyyymmdd =
    DateFormat.format
        [ DateFormat.yearNumber
        , DateFormat.monthFixed
        , DateFormat.dayOfMonthFixed
        ]


{-| <https://ruby-doc.org/core-2.6.3/Time.html#method-i-to_s>

    `t.utc.to_s                         #=> "2012-11-10 17:16:12 UTC"`
    `t.strftime "%Y-%m-%d %H:%M:%S UTC" #=> "2012-11-10 17:16:12 UTC"`

-}
maybeTimeFromRuby : String -> Maybe Time.Posix
maybeTimeFromRuby string =
    let
        sanitizedString =
            if String.endsWith " UTC" string then
                -- convert `2014-05-17 05:03:17 UTC`
                -- into    `2014-05-17T05:03:17Z`
                String.split " " string
                    |> List.take 2
                    |> String.join "T"
                    |> (\s -> s ++ "Z")

            else
                string
    in
    Result.toMaybe (Iso8601.toTime sanitizedString)


maybeTimeFromIso8601 : String -> Maybe Time.Posix
maybeTimeFromIso8601 =
    Iso8601.toTime >> Result.toMaybe


maybeTimeFromInternetMessageFormat : String -> Maybe Time.Posix
maybeTimeFromInternetMessageFormat =
    Imf.DateTime.toPosix >> Result.toMaybe
