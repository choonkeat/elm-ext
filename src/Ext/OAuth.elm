module Ext.OAuth exposing (..)

import Ext.Http
import Http
import Json.Decode
import Json.Encode
import Platform exposing (Task)
import Task
import Url.Builder


type alias Config =
    { authorizeUrl : String
    , accessTokenUrl : String
    , redirectUrl : String
    , clientId : String
    , clientSecret : String
    , httpTimeout : Maybe Float
    }


authorizeLink : Config -> List String -> String -> String
authorizeLink { authorizeUrl, clientId, redirectUrl } scopes state =
    authorizeUrl
        ++ Url.Builder.toQuery
            [ Url.Builder.string "client_id" clientId
            , Url.Builder.string "scope" (String.join "," scopes)
            , Url.Builder.string "redirect_uri" redirectUrl
            , Url.Builder.string "state" state
            ]


type alias AccessTokenResponse =
    { accessToken : String
    }


decodeAccessTokenResponse : Json.Decode.Decoder AccessTokenResponse
decodeAccessTokenResponse =
    Json.Decode.map AccessTokenResponse
        (Json.Decode.field "access_token" Json.Decode.string)


accessToken : Config -> String -> Task Http.Error AccessTokenResponse
accessToken { accessTokenUrl, clientId, clientSecret, httpTimeout } verificationCode =
    let
        bodyPayload =
            Json.Encode.object
                [ ( "client_id", Json.Encode.string clientId )
                , ( "client_secret", Json.Encode.string clientSecret )
                , ( "code", Json.Encode.string verificationCode )
                ]
    in
    Http.task
        { method = "POST"
        , headers = []
        , url = accessTokenUrl
        , body = Http.jsonBody bodyPayload
        , resolver = Http.stringResolver (Ext.Http.jsonDecodeHttpResponse decodeAccessTokenResponse)
        , timeout = httpTimeout
        }
