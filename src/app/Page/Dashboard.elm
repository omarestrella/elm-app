port module Page.Dashboard exposing (Model, Msg, defaultModel, linkResponse, routeHandler, subscriptions, update, view)

import Api exposing (ApiEnvironment(..), Method(..))
import Component.Button as Button
import Html.Styled exposing (Html, div, li, span, text, ul)
import Html.Styled.Attributes exposing (css, id)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Link exposing (LinkResponse(..), linkResponseDecoder)
import Session exposing (Session(..))
import Style exposing (accountsList, homeContainer)


type Msg
    = NoOp
    | StartLink
    | LinkResponseMsg (Result Decode.Error LinkResponse)
    | HandleItemLink (Result Http.Error Session.Item)
    | LoadData
    | GotAccounts (Result Http.Error (List Session.Account))
    | GotTransactions (Result Http.Error (List Transaction))


defaultUser =
    { id = "0"
    , firstName = "First"
    , lastName = "Last"
    , email = "example@email.com"
    }


defaultModel session =
    { environment = Sandbox
    , linkResponse = LinkError []
    , session = session
    , transactions = []
    }


type alias Model =
    { environment : ApiEnvironment
    , linkResponse : LinkResponse
    , session : Session
    , transactions : List Transaction
    }


type alias Transaction =
    { amount : Float
    , categories : List String
    , date : String
    , name : String
    , pending : Bool
    }


transactionDecoder : Decode.Decoder Transaction
transactionDecoder =
    Decode.succeed Transaction
        |> required "amount" Decode.float
        |> required "category" (Decode.list Decode.string)
        |> required "date" Decode.string
        |> required "name" Decode.string
        |> required "pending" Decode.bool



--Update


routeHandler : Msg
routeHandler =
    LoadData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartLink ->
            ( model, plaidLink "StartLink" )

        LinkResponseMsg response ->
            let
                _ =
                    Debug.log "Response" response

                token =
                    Session.accessToken model.session
            in
            case response of
                Ok linkData ->
                    case linkData of
                        LinkSuccess link ->
                            ( model
                            , Session.linkItemToUser HandleItemLink token link.publicToken
                            )

                        LinkError errors ->
                            -- TODO: graceful error handling
                            ( model, Cmd.none )

                Err _ ->
                    -- TODO: graceful error handling
                    ( model, Cmd.none )

        HandleItemLink response ->
            case response of
                Ok item ->
                    ( { model
                        | session = Session.addItemToUser model.session item
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        LoadData ->
            let
                token =
                    Session.accessToken model.session

                accountTokens =
                    Session.accountTokens model.session
            in
            ( model
            , Cmd.batch
                [ Session.loadAccounts GotAccounts token accountTokens
                , loadTransactions token accountTokens
                ]
            )

        GotAccounts response ->
            case response of
                Ok accounts ->
                    let
                        newModel =
                            { model | session = Session.addAccountsToSession model.session accounts }
                    in
                    ( newModel, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotTransactions response ->
            case response of
                Ok transactions ->
                    ( { model | transactions = transactions }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


loadTransactions : String -> List String -> Cmd Msg
loadTransactions token accessTokens =
    let
        url =
            Api.accessTokensUrl accessTokens "transactions"
    in
    Api.request
        { url = url
        , accessToken = Just token
        , method = GET
        , body = Nothing
        , handler = Http.expectJson GotTransactions (Decode.list transactionDecoder)
        }



-- View


accountsPane : List Session.Account -> Html Msg
accountsPane accounts =
    div []
        [ Button.primary "Add account" StartLink
        , div [ css accountsList ] <|
            List.map
                (\account ->
                    div []
                        [ text account.name
                        , text <| "(" ++ account.institutionName ++ ")"
                        ]
                )
                accounts
        ]


transactionsPane : List Transaction -> Html Msg
transactionsPane transactions =
    div [] <|
        List.map
            (\transaction ->
                div []
                    [ span [] [ text transaction.name ]
                    , span [] [ text (String.fromFloat transaction.amount) ]
                    ]
            )
            transactions


view : Model -> Html Msg
view model =
    case model.session of
        Guest _ ->
            text "Guest Home"

        LoggedIn _ data ->
            div [ css homeContainer ]
                [ accountsPane data.accounts
                , transactionsPane model.transactions
                ]


port plaidLink : String -> Cmd msg


port linkResponse : (Decode.Value -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    linkResponse
        (\value ->
            LinkResponseMsg (Decode.decodeValue linkResponseDecoder value)
        )
