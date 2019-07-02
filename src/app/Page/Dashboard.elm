port module Page.Dashboard exposing (Model, Msg, bootstrap, defaultModel, linkResponse, routeHandler, subscriptions, update, view)

import Api exposing (ApiEnvironment(..), Method(..))
import Component.Button as Button
import Component.Chart as Chart
import Component.Input as Input
import DateFormat.Relative exposing (relativeTime)
import Dates exposing (DateRange(..), dateRangeToQuery, dateRangeToString, dateSelector, formattedTransactionGroupDate, stringToDateRange)
import Html.Styled exposing (Html, a, button, div, form, fromUnstyled, h4, h5, h6, input, label, li, option, select, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Styled.Attributes exposing (class, css, href, id, placeholder, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Http
import Http.Detailed
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required, requiredAt, resolve)
import Json.Encode as Encode
import Link exposing (LinkResponse(..), linkResponseDecoder)
import List.Extra exposing (groupWhile)
import Money exposing (currencyCodeToSymbol, stringToCurrencyCode)
import Round exposing (round)
import Session exposing (Session(..))
import Style.Dashboard as Style
import Task
import Time exposing (Posix, Zone)


type Msg
    = NoOp
    | StartLink
    | LinkResponseMsg (Result Decode.Error LinkResponse)
    | HandleItemLink (Result Http.Error Session.Item)
    | LoadData
    | LoadTransactionsForTime String (List String) Posix
    | UpdateBudgetGroup String
    | ClearBudgetGroup
    | SubmitNewBudgetGroup String
    | PerformCategorySearch String
    | GotAccounts (Result Http.Error (List Session.Account))
    | GotCategories (Result Http.Error (List Category))
    | GotTransactions (Result (Http.Detailed.Error String) ( List Transaction, Http.Metadata ))
    | ChangeDateRange String
    | GotCurrentTime Posix
    | GotCurrentZone Zone


type CategoryState
    = Loading String
    | Default (List Category)
    | Search String (List Category)


type NewBudgetGroup
    = None
    | New String


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
    , categories = Default []
    , budgetGroups = []
    , newBudgetGroup = None
    , errors = []
    , transactionDateRange = ThisMonth
    , currentTime = Time.millisToPosix 0
    , currentZone = Time.utc
    }


type alias Model =
    { environment : ApiEnvironment
    , linkResponse : LinkResponse
    , session : Session
    , transactions : List Transaction
    , categories : CategoryState
    , budgetGroups : List BudgetGroup
    , newBudgetGroup : NewBudgetGroup
    , errors : List Error
    , transactionDateRange : DateRange
    , currentTime : Posix
    , currentZone : Zone
    }


type alias TransactionDetail =
    { amount : Float
    , categories : List String
    , date : String
    , name : String
    , pending : Bool
    , currencyCode : String
    }


type Transaction
    = Success (List TransactionDetail)
    | TransactionError Error
    | UnknownTransactionStatus


type alias Category =
    { id : String
    , group : String
    , hierarchy : List String
    }


type alias BudgetGroup =
    { id : String
    , name : String
    , items : List BudgetItem
    }


type alias BudgetItem =
    { id : String
    , name : String
    , plannedSpending : Float
    , category : Category
    }


type ErrorCode
    = ItemLoginRequired
    | UnknownError


type alias Error =
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


transactionDecoder : Decode.Decoder Transaction
transactionDecoder =
    Decode.oneOf
        [ transactionErrorDecoder
            |> Decode.map TransactionError
        , Decode.field "transactions" (Decode.list transactionDetailDecoder)
            |> Decode.map Success
        , Decode.succeed UnknownTransactionStatus
        ]


transactionErrorDecoder : Decode.Decoder Error
transactionErrorDecoder =
    Decode.at [ "error", "code" ] Decode.string
        |> Decode.andThen
            (\str ->
                let
                    code =
                        errorCodeFromString str
                in
                Decode.succeed Error
                    |> hardcoded code
                    |> requiredAt [ "error", "message" ] Decode.string
            )


transactionDetailDecoder : Decode.Decoder TransactionDetail
transactionDetailDecoder =
    Decode.succeed TransactionDetail
        |> required "amount" Decode.float
        |> required "category" (Decode.list Decode.string)
        |> required "date" Decode.string
        |> required "name" Decode.string
        |> required "pending" Decode.bool
        |> required "iso_currency_code" Decode.string


categoryDecoder : Decode.Decoder Category
categoryDecoder =
    Decode.succeed Category
        |> required "category_id" Decode.string
        |> required "group" Decode.string
        |> required "hierarchy" (Decode.list Decode.string)


budgetGroupDecoder : List Category -> Decode.Decoder BudgetGroup
budgetGroupDecoder categories =
    Decode.succeed BudgetGroup
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "items" (Decode.list (budgetItemDecoder categories))


budgetItemDecoder : List Category -> Decode.Decoder BudgetItem
budgetItemDecoder categories =
    let
        toDecoder : String -> String -> Float -> String -> Decode.Decoder BudgetItem
        toDecoder id name spending categoryId =
            let
                possibleCategory =
                    List.filter (\category -> category.id == categoryId) categories
                        |> List.head
            in
            case possibleCategory of
                Just category ->
                    Decode.succeed (BudgetItem id name spending category)

                Nothing ->
                    Decode.fail ("Category with id " ++ categoryId ++ " not found")
    in
    Decode.succeed toDecoder
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "plannedSpending" Decode.float
        |> required "categoryId" Decode.string
        |> resolve


errorDecoder : Decode.Decoder Error
errorDecoder =
    Decode.oneOf
        [ Decode.field "code" Decode.string
        , Decode.at [ "error", "code" ] Decode.string
        ]
        |> Decode.andThen
            (\str ->
                let
                    code =
                        errorCodeFromString str
                in
                Decode.succeed Error
                    |> hardcoded code
                    |> required "message" Decode.string
            )



--Update


bootstrap : Session -> Cmd Msg
bootstrap session =
    let
        token =
            Session.accessToken session

        accountTokens =
            Session.accountTokens session

        haveAccountTokens =
            List.length accountTokens > 0

        accountsCmd =
            if haveAccountTokens then
                Session.loadAccounts GotAccounts token accountTokens

            else
                Cmd.none

        transactionsCmd =
            if haveAccountTokens then
                Task.perform (LoadTransactionsForTime token accountTokens) Time.now

            else
                Cmd.none
    in
    Cmd.batch
        [ accountsCmd
        , transactionsCmd
        , loadCategories token
        , Task.perform GotCurrentZone Time.here
        ]


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
                token =
                    Session.accessToken model.session
            in
            case response of
                Ok linkData ->
                    case linkData of
                        LinkSuccess link ->
                            ( model
                            , Session.linkItemToUser HandleItemLink token link
                            )

                        LinkError errors ->
                            -- MARK: graceful error handling
                            ( model, Cmd.none )

                Err _ ->
                    -- MARK: graceful error handling
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
            ( model
            , bootstrap model.session
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
                Ok ( transactions, _ ) ->
                    ( { model | transactions = transactions }, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log "error" err
                    in
                    ( model, Cmd.none )

        GotCategories response ->
            case response of
                Ok categories ->
                    case model.categories of
                        Loading string ->
                            if String.length string == 0 then
                                ( { model | categories = Default categories }, Cmd.none )

                            else
                                ( { model | categories = Search string categories }, Cmd.none )

                        Search string _ ->
                            ( { model | categories = Search string categories }, Cmd.none )

                        Default _ ->
                            ( { model | categories = Default categories }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        PerformCategorySearch search ->
            if String.length search > 0 then
                ( { model | categories = Loading search }, searchCategories search (Session.accessToken model.session) )

            else
                ( { model | categories = Loading "" }, loadCategories (Session.accessToken model.session) )

        UpdateBudgetGroup name ->
            ( { model | newBudgetGroup = New name }, Cmd.none )

        ClearBudgetGroup ->
            ( { model | newBudgetGroup = None }, Cmd.none )

        SubmitNewBudgetGroup name ->
            ( { model | budgetGroups = model.budgetGroups ++ [ { id = "", name = name, items = [] } ] }, Cmd.none )

        ChangeDateRange value ->
            let
                range =
                    stringToDateRange value

                token =
                    Session.accessToken model.session

                accountTokens =
                    Session.accountTokens model.session

                date =
                    dateRangeToQuery range model.currentTime
            in
            ( { model | transactionDateRange = range }
            , loadTransactions token accountTokens date.start date.end
            )

        LoadTransactionsForTime token accountTokens time ->
            let
                date =
                    dateRangeToQuery model.transactionDateRange time
            in
            ( { model | currentTime = time }
            , loadTransactions token accountTokens date.start date.end
            )

        GotCurrentTime time ->
            ( { model | currentTime = time }, Cmd.none )

        GotCurrentZone zone ->
            ( { model | currentZone = zone }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


loadTransactions : String -> List String -> Int -> Int -> Cmd Msg
loadTransactions token accessTokens start end =
    let
        url =
            Api.dateRangeUrl accessTokens "transactions" start end
    in
    Api.request
        { url = url
        , accessToken = Just token
        , method = GET
        , body = Nothing
        , handler =
            Http.Detailed.expectJson GotTransactions (Decode.list transactionDecoder)
        }


loadCategories : String -> Cmd Msg
loadCategories token =
    let
        url =
            Api.url "categories"
    in
    Api.request
        { url = url
        , accessToken = Just token
        , method = GET
        , body = Nothing
        , handler = Http.expectJson GotCategories (Decode.list categoryDecoder)
        }


searchCategories : String -> String -> Cmd Msg
searchCategories search token =
    let
        url =
            Api.url "categories/search"

        body =
            Encode.object [ ( "query", Encode.string search ) ]
    in
    Api.request
        { url = url
        , accessToken = Just token
        , method = POST
        , body = Just (Http.jsonBody body)
        , handler = Http.expectJson GotCategories (Decode.list categoryDecoder)
        }



-- Helpers


categoryText : Category -> String
categoryText category =
    List.reverse category.hierarchy
        |> List.head
        |> Maybe.withDefault ""


transactionDetails : List Transaction -> List TransactionDetail
transactionDetails transactions =
    List.filterMap
        (\t ->
            case t of
                Success details ->
                    Just details

                TransactionError _ ->
                    Nothing

                UnknownTransactionStatus ->
                    Nothing
        )
        transactions
        |> List.concat


groupedTransactions : List Transaction -> List ( TransactionDetail, List TransactionDetail )
groupedTransactions transactions =
    transactionDetails transactions
        |> List.Extra.groupWhile
            (\a b ->
                a.date == b.date
            )


transactionAmount : TransactionDetail -> String
transactionAmount transaction =
    let
        symbol =
            stringToCurrencyCode transaction.currencyCode
                |> currencyCodeToSymbol
    in
    String.join "" [ symbol, round 2 (abs transaction.amount) ]



-- View


accountsPane : List Session.Account -> Html Msg
accountsPane accounts =
    div []
        [ Button.primary [ onClick StartLink ] [ text "Add account" ]
        , div [] <|
            List.map
                (\account ->
                    div []
                        [ text account.name
                        , text <| "(" ++ account.institutionName ++ ")"
                        ]
                )
                accounts
        ]


transactionDetailsView : TransactionDetail -> Html Msg
transactionDetailsView transaction =
    div [ css Style.transactionDetail ]
        [ div [ css Style.transactionDetailNameCategory ]
            [ span [ css Style.transactionDetailName ]
                [ text transaction.name ]
            , span [ css Style.transactionDetailCategory ]
                [ text (List.head transaction.categories |> Maybe.withDefault "") ]
            ]
        , span
            [ css Style.transactionDetailAmount
            , css (Style.transactionDetailAmountColor transaction)
            ]
            [ text (transactionAmount transaction) ]
        ]


transactionsDateRangeView : Html Msg
transactionsDateRangeView =
    dateSelector
        [ ThisMonth, LastMonth, Last30Days, Last90Days ]
        ChangeDateRange


transactionsGroupView : List TransactionDetail -> Html Msg
transactionsGroupView transactions =
    let
        date =
            List.head transactions
                |> Maybe.andThen
                    (\t ->
                        Just (formattedTransactionGroupDate t.date)
                    )
                |> Maybe.withDefault ""
    in
    div [ css Style.transactionGroup ]
        [ h5 [ css Style.transactionGroupTitle ] [ text date ]
        , div [ css Style.transactionGroupDetail ] <|
            List.map
                transactionDetailsView
                transactions
        ]


transactionsPane : Model -> Html Msg
transactionsPane model =
    div [ class "transactions-pane" ]
        [ div []
            [ transactionsDateRangeView ]
        , div [] <|
            List.map
                (\( transactionGroup, transactions ) ->
                    transactionsGroupView (transactionGroup :: transactions)
                )
                (groupedTransactions model.transactions)
        ]


addNewGroupView : Model -> Html Msg
addNewGroupView model =
    case model.newBudgetGroup of
        New name ->
            div [ css Style.addNewBudgetGroup ]
                [ form [ onSubmit (SubmitNewBudgetGroup name) ]
                    [ div []
                        [ label [] [ text "Name" ]
                        , Input.default
                            [ value name, onInput UpdateBudgetGroup ]
                            []
                        ]
                    , div []
                        [ Button.primary [ type_ "submit" ]
                            [ text "Add" ]
                        , Button.link [ type_ "reset", onClick ClearBudgetGroup ]
                            [ text "Cancel" ]
                        ]
                    ]
                ]

        None ->
            div [] []


categoryListSearchView : CategoryState -> Html Msg
categoryListSearchView categoryState =
    let
        searchValue =
            case categoryState of
                Search searchString _ ->
                    searchString

                Loading searchString ->
                    searchString

                _ ->
                    ""
    in
    div []
        [ Input.default
            [ value searchValue, onInput PerformCategorySearch, placeholder "Search or add new..." ]
            []
        ]


categoryListView : CategoryState -> Html Msg
categoryListView categoryState =
    case categoryState of
        Default categories ->
            div [] <|
                List.map
                    (\category ->
                        div []
                            [ a [ href "#" ]
                                [ text (categoryText category) ]
                            ]
                    )
                    categories

        Search _ categories ->
            div [] <|
                List.map
                    (\category ->
                        div []
                            [ a [ href "#" ]
                                [ text (categoryText category) ]
                            ]
                    )
                    categories

        Loading _ ->
            div []
                [ text "Loading..." ]


categoryPickerView : Model -> Html Msg
categoryPickerView model =
    div []
        [ categoryListSearchView model.categories
        , categoryListView model.categories
        ]


budgetGroupListView : Model -> Html Msg
budgetGroupListView model =
    div [ css Style.budgetGroupList ]
        [ Button.primary [ onClick (UpdateBudgetGroup "") ] [ text "Add Group" ]
        , case model.newBudgetGroup of
            New _ ->
                addNewGroupView model

            None ->
                text ""

        -- , categoryPickerView model
        ]


snapshotView : Model -> Html Msg
snapshotView model =
    let
        money =
            [ 1500, 1200 ]
    in
    div [ css Style.snapshotContainer ]
        [ div [ css Style.snapshotEntryContainer, css Style.snapshotBalance ]
            [ h6 [ css Style.snapshotEntryHeading ]
                [ text "Balance" ]
            , span [ css Style.snapshotEntryValue ]
                [ text "$15,000" ]
            ]
        , div [ css Style.snapshotEntryContainer, css Style.snapshotIncomeSpending ]
            [ div [ css Style.snapshotEntry ]
                [ h6 [ css Style.snapshotEntryHeading ]
                    [ text "Income" ]
                , span [ css Style.snapshotEntryValue ]
                    [ text "$1,500" ]
                ]
            , div [ css Style.snapshotEntry ]
                [ h6 [ css Style.snapshotEntryHeading ]
                    [ text "Spending" ]
                , span [ css Style.snapshotEntryValue ]
                    [ text "$1,200" ]
                ]
            ]
        , div [ css Style.snapshotEntryContainer, css Style.snapshotChart ]
            [ Chart.view money 150 150
                |> fromUnstyled
            ]
        ]


view : Model -> Html Msg
view model =
    case model.session of
        Guest _ ->
            text "Guest Home"

        LoggedIn _ data ->
            div [ css Style.budgetContainer ]
                [ div []
                    [ Button.primary [ onClick StartLink ] [ text "Add account" ] ]
                , snapshotView model
                , transactionsPane model

                -- budgetGroupListView model
                -- , accountsPane data.accounts
                ]


port plaidLink : String -> Cmd msg


port linkResponse : (Decode.Value -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    linkResponse
        (\value ->
            LinkResponseMsg (Decode.decodeValue linkResponseDecoder value)
        )
