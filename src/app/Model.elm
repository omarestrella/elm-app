module Model exposing (Account, ApiEnvironment(..), AppModel, Balance, Error, GuestModel, LoginForm, User, accountDecoder, balanceDecoder, errorDecoder)

import Http
import Json.Decode as Decode exposing (map)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Link exposing (Link, LinkResponse)


type alias Error =
    { code : String
    , message : Maybe String
    }


type alias GuestModel =
    { loginForm : LoginForm
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
