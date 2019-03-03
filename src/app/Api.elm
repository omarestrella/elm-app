module Api exposing (ApiEnvironment(..), Method(..), RequestOptions, accessTokensUrl, methodString, request, url)

import Http
import Json.Decode as Decode exposing (field, int, string)
import Url.Builder as Builder


type ApiEnvironment
    = Sandbox
    | Development
    | Production


type alias RequestOptions msg =
    { url : String
    , accessToken : Maybe String
    , method : Method
    , body : Maybe Http.Body
    , handler : Http.Expect msg
    }


type Method
    = GET
    | POST
    | PUT
    | PATCH
    | DELETE



-- Utility


url : String -> String
url url_ =
    Builder.absolute [ url_ ] []


accessTokensUrl : List String -> String -> String
accessTokensUrl tokens base =
    Builder.absolute [ base ] (List.map (Builder.string "accessToken") tokens)


methodString : Method -> String
methodString method =
    case method of
        GET ->
            "GET"

        POST ->
            "POST"

        PATCH ->
            "PATCH"

        PUT ->
            "PUT"

        DELETE ->
            "DELETE"


request : RequestOptions msg -> Cmd msg
request options =
    let
        requestBody =
            Maybe.withDefault Http.emptyBody options.body

        headers =
            case options.accessToken of
                Just token ->
                    [ Http.header "Authorization" ("bearer " ++ token) ]

                Nothing ->
                    []
    in
    Http.request
        { method = methodString options.method
        , headers = headers
        , url = "/api/v1" ++ options.url
        , body = requestBody
        , expect = options.handler
        , timeout = Nothing
        , tracker = Nothing
        }
