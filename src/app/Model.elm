module Model exposing (Account, ApiEnvironment(..), AppModel, Balance, Error, GuestModel, LoginForm, User, accountDecoder, balanceDecoder, errorDecoder)

import Http
import Json.Decode as Decode exposing (map)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Link exposing (Link, LinkResponse)


type ApiEnvironment
    = Sandbox
    | Development
    | Production


type alias Error =
    { code : String
    , message : Maybe String
    }


type alias GuestModel =
    { loginForm : LoginForm
    }


type alias AppModel =
    { user : User
    , environment : ApiEnvironment
    , linkResponse : LinkResponse
    , accounts : List Account
    }


type alias LoginForm =
    { email : String
    , password : String
    }


type alias User =
    { id : String
    , firstName : String
    , lastName : String
    , email : String
    }


type alias Balance =
    { available : Maybe Float
    , current : Float
    , isoCurrencyCode : String
    , limit : Maybe Float
    }


type alias Account =
    { accountId : String
    , balances : Maybe Balance
    , name : String
    , mask : String
    , officialName : String
    , type_ : String
    , subtype : String
    }


errorDecoder : Decode.Decoder Error
errorDecoder =
    Decode.succeed Error
        |> required "code" Decode.string
        |> optional "message" (Decode.maybe Decode.string) Nothing


balanceDecoder : Decode.Decoder Balance
balanceDecoder =
    Decode.succeed Balance
        |> optional "available" (Decode.maybe Decode.float) Nothing
        |> required "current" Decode.float
        |> required "iso_currency_code" Decode.string
        |> optional "limit" (Decode.maybe Decode.float) Nothing


accountDecoder : Decode.Decoder Account
accountDecoder =
    Decode.succeed Account
        |> required "account_id" Decode.string
        |> optional "balances" (Decode.maybe balanceDecoder) Nothing
        |> required "name" Decode.string
        |> required "mask" Decode.string
        |> required "official_name" Decode.string
        |> required "type" Decode.string
        |> required "subtype" Decode.string
