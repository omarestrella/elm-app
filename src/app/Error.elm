module Error exposing (ErrorCode(..), PlaidError, plaidErrorDecoder)

import Json.Decode as Decode exposing (string)
import Json.Decode.Pipeline exposing (hardcoded, requiredAt)


type ErrorCode
    = ItemLoginRequired
    | UnknownError


type alias PlaidError =
    { code : ErrorCode
    , message : String
    }


errorCodeFromString : String -> ErrorCode
errorCodeFromString str =
    case str of
        "ITEM_LOGIN_REQUIRED" ->
            ItemLoginRequired

        _ ->
            UnknownError


plaidErrorDecoder : Decode.Decoder PlaidError
plaidErrorDecoder =
    Decode.at [ "error", "code" ] Decode.string
        |> Decode.andThen
            (\str ->
                let
                    code =
                        errorCodeFromString str
                in
                Decode.succeed PlaidError
                    |> hardcoded code
                    |> requiredAt [ "error", "message" ] Decode.string
            )
