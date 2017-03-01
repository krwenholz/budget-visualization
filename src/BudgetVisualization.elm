port module BudgetVisualization exposing (..)

import Array exposing (Array, map, indexedMap, push, set, get)
import Html exposing (Html, text, div, input, ul, li, button)
import Html.Attributes exposing (placeholder, type_, id, width, height, class)
import Html.Events exposing (onInput, onClick)
import Maybe exposing (withDefault)
import DataStructureHelp exposing (removeFromArray)
import BudgetMath
import Account


-- TODO: try to repro bug with account value 1 and percent change 1 -> 10 breaking graph


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
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
