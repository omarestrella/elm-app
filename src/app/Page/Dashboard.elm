port module Page.Dashboard exposing (Model, Msg, bootstrap, defaultModel, linkResponse, routeHandler, subscriptions, update, view)

import Api exposing (ApiEnvironment(..), Method(..))
import Component.Button as Button
import Component.Input as Input
import Html.Styled exposing (Html, a, button, div, input, li, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Styled.Attributes exposing (class, css, href, id, placeholder, value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required, resolve)
import Json.Encode as Encode
import Link exposing (LinkResponse(..), linkResponseDecoder)
import Session exposing (Session(..))
import Style.Dashboard as Style


type Msg
    = NoOp
    | StartLink
    | LinkResponseMsg (Result Decode.Error LinkResponse)
    | HandleItemLink (Result Http.Error Session.Item)
    | LoadData
    | AddBudgetGroup
    | PerformCategorySearch String
    | GotAccounts (Result Http.Error (List Session.Account))
    | GotTransactions (Result Http.Error (List Transaction))
    | GotCategories (Result Http.Error (List Category))


type CategoryState
    = Loading String
    | Default (List Category)
    | Search String (List Category)


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
    , addNewGroup = False
    }


type alias Model =
    { environment : ApiEnvironment
    , linkResponse : LinkResponse
    , session : Session
    , transactions : List Transaction
    , categories : CategoryState
    , addNewGroup : Bool
    }


type alias Transaction =
    { amount : Float
    , categories : List String
    , date : String
    , name : String
    , pending : Bool
    }


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


type alias Error =
    { code : String
    , message : String
    }


transactionDecoder : Decode.Decoder Transaction
transactionDecoder =
    Decode.succeed Transaction
        |> required "amount" Decode.float
        |> required "category" (Decode.list Decode.string)
        |> required "date" Decode.string
        |> required "name" Decode.string
        |> required "pending" Decode.bool


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
    Decode.succeed Error
        |> required "code" Decode.string
        |> required "message" Decode.string



--Update


bootstrap : Session -> Cmd Msg
bootstrap session =
    let
        token =
            Session.accessToken session

        accountTokens =
            Session.accountTokens session
    in
    Cmd.batch
        [ Session.loadAccounts GotAccounts token accountTokens
        , loadTransactions token accountTokens
        , loadCategories token
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
                Ok transactions ->
                    ( { model | transactions = transactions }, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log "Error" err
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

        AddBudgetGroup ->
            ( { model | addNewGroup = True }, Cmd.none )

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
        , handler =
            Http.expectJson GotTransactions (Decode.list transactionDecoder)
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


categoryText : Category -> String
categoryText category =
    List.reverse category.hierarchy
        |> List.head
        |> Maybe.withDefault ""



-- View


accountsPane : List Session.Account -> Html Msg
accountsPane accounts =
    div []
        [ Button.primary "Add account" StartLink
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


addNewGroupView : Model -> Html Msg
addNewGroupView model =
    div [ css Style.addNewBudgetGroup ]
        [ if model.addNewGroup then
            table [ class "table" ]
                [ thead []
                    [ th [] [ text "Name" ]
                    , th [] [ text "Amount" ]
                    ]
                , tbody []
                    [ tr []
                        [ td []
                            [ input []
                                []
                            ]
                        , td []
                            [ input []
                                []
                            ]
                        ]
                    ]
                ]

          else
            text ""
        ]


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
        [ Button.primary "Add Group" AddBudgetGroup
        , addNewGroupView model
        , categoryPickerView model
        ]


view : Model -> Html Msg
view model =
    case model.session of
        Guest _ ->
            text "Guest Home"

        LoggedIn _ data ->
            div [ css Style.budgetContainer ]
                [ budgetGroupListView model

                -- accountsPane data.accounts
                -- , transactionsPane model.transactions
                ]


port plaidLink : String -> Cmd msg


port linkResponse : (Decode.Value -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    linkResponse
        (\value ->
            LinkResponseMsg (Decode.decodeValue linkResponseDecoder value)
        )
