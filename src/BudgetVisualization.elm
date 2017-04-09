port module BudgetVisualization exposing (..)

import Account
import Array exposing (Array, map, indexedMap, push, set, get)
import BudgetMath
import DataStructureHelp exposing (removeFromArray)
import Html exposing (Html, text, div, input, ul, li, button)
import Html.Attributes exposing (placeholder, type_, id, width, height, class)
import Html.Events exposing (onInput, onClick)
import Maybe exposing (withDefault)
import Navigation exposing (Location)
import RouteUrl exposing (App, UrlChange, RouteUrlProgram)
import RouteUrl.Builder exposing (Builder, builder, path, appendToPath, toUrlChange, insertQuery)


-- TODO: just give copy state and input state buttons


main : RouteUrlProgram Never Model Msg
main =
    RouteUrl.program
        { delta2url = delta2url
        , location2messages = location2messages
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias UpdateAcountMsg =
    { accountNum : Int
    , update : Account.Msg
    }


type Msg
    = UpdateAccount UpdateAcountMsg
    | DeleteAccount Int
    | NewAccount


type alias Model =
    Array Account.Model


init : ( Model, Cmd Msg )
init =
    ( Array.fromList [ (Account.init) ], Cmd.none )


port exportExtrapolation : BudgetMath.Model -> Cmd msg


updateAccounts : Msg -> Model -> Model
updateAccounts action accounts =
    case action of
        UpdateAccount { accountNum, update } ->
            let
                account =
                    get accountNum accounts |> withDefault Account.init
            in
                set accountNum (Account.update update account) accounts

        DeleteAccount accountNum ->
            removeFromArray accountNum accounts

        NewAccount ->
            push Account.init accounts


update : Msg -> Model -> ( Model, Cmd msg )
update action accounts =
    let
        updatedModel =
            updateAccounts action accounts
    in
        ( updatedModel, exportExtrapolation (BudgetMath.asData updatedModel) )


accountListItem : Int -> Account.Model -> Html Msg
accountListItem index account =
    li []
        [ div []
            [ button
                [ onClick (DeleteAccount index) ]
                [ text "-" ]
            , Html.map
                (\update ->
                    UpdateAccount
                        { accountNum = index
                        , update = update
                        }
                )
              <|
                Account.view account
            ]
        ]


accountsList : Array Account.Model -> Html Msg
accountsList accounts =
    ul [ class "accountList" ]
        (Array.toList <|
            indexedMap (\index account -> accountListItem index account)
                accounts
        )


view : Model -> Html Msg
view accounts =
    div
        []
        [ accountsList accounts
        , button [ onClick NewAccount ] [ text "+" ]
        ]



-- Routing


delta2url : Model -> Model -> Maybe UrlChange
delta2url previous current =
    Maybe.map toUrlChange <|
            |> Just
        )



-- TODO: use insertQuery to add each account
-- TODO: use updateQuery on each account query entry to add append a list of income event infos


queryableString : Account.Model -> String
queryableString account =
    "account:"
        ++ account.name
        ++ ",initialValue:"
        ++ (toString account.initialValue)
        ++ ",incomeEvents:"
        ++ (Array.foldl
                (\incomeEvent eventStr ->
                    "name:"
                        ++ incomeEvent.name
                        ++ ",flatChange:"
                        ++ (toString incomeEvent.flatChange)
                        ++ ",percentChange:"
                        ++ (toString incomeEvent.percentChange)
                        ++ ";"
                )
                ""
                account.incomeEvents
           )



-- TODO: use query function to grab the query dict and then map over accounts


location2messages : Location -> List Msg
location2messages location =
    []



--     case path builder of
--         first :: rest ->
--             case toInt first of
--                 Ok value ->
--                     [ Set value ]
--
--                 Err _ ->
--                     -- If it wasn't an integer, then no action ... we could
--                     -- show an error instead, of course.
--                     []
--
--         _ ->
--             -- If nothing provided for this part of the URL, return empty list
--             []
