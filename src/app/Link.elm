module Link exposing (Error, Link, LinkAccount, LinkErrorResponse, LinkInstitution, LinkResponse(..), LinkSuccessResponse, errorDecoder, linkAccountDecoder, linkDecoder, linkErrorDecoder, linkErrorResponseDecoder, linkInstitutionDecoder, linkResponseDecoder, linkSuccessDecoder, linkSuccessResponseDecoder)

import Json.Decode as Decode exposing (map)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


type alias Error =
    { code : String
    }


type LinkResponse
    = LinkSuccess Link
    | LinkError (List Error)
    | NoLink


type alias LinkSuccessResponse =
    { data : Link
    }


type alias LinkErrorResponse =
    { errors : List Error
    }


type alias Link =
    { publicToken : String
    , linkSessionId : String
    , accounts : List LinkAccount
    , institution : LinkInstitution
    }


type alias LinkAccount =
    { id : String
    , name : String
    , mask : String
    , type_ : String
    , subtype : String
    }


type alias LinkInstitution =
    { name : String
    , institutionId : String
    }



-- Decoders


errorDecoder : Decode.Decoder Error
errorDecoder =
    Decode.succeed Error
        |> required "code" Decode.string


linkAccountDecoder : Decode.Decoder LinkAccount
linkAccountDecoder =
    Decode.succeed LinkAccount
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "mask" Decode.string
        |> required "type" Decode.string
        |> required "subtype" Decode.string


linkInstitutionDecoder : Decode.Decoder LinkInstitution
linkInstitutionDecoder =
    Decode.succeed LinkInstitution
        |> required "name" Decode.string
        |> required "institution_id" Decode.string


linkDecoder : Decode.Decoder Link
linkDecoder =
    Decode.succeed Link
        |> required "publicToken" Decode.string
        |> required "linkSessionId" Decode.string
        |> required "accounts" (Decode.list linkAccountDecoder)
        |> required "institution" linkInstitutionDecoder


linkSuccessResponseDecoder : Decode.Decoder LinkSuccessResponse
linkSuccessResponseDecoder =
    Decode.succeed LinkSuccessResponse
        |> required "data" linkDecoder


linkErrorResponseDecoder : Decode.Decoder LinkErrorResponse
linkErrorResponseDecoder =
    Decode.succeed LinkErrorResponse
        |> required "errors" (Decode.list errorDecoder)


linkSuccessDecoder : Decode.Decoder LinkResponse
linkSuccessDecoder =
    Decode.map
        (\response -> LinkSuccess response.data)
        linkSuccessResponseDecoder


linkErrorDecoder : Decode.Decoder LinkResponse
linkErrorDecoder =
    Decode.map
        (\response -> LinkError response.errors)
        linkErrorResponseDecoder


linkResponseDecoder : Decode.Decoder LinkResponse
linkResponseDecoder =
    Decode.oneOf [ linkSuccessDecoder, linkErrorDecoder ]
