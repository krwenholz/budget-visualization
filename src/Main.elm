module Main exposing (..)

import Array exposing (Array, map, indexedMap, push, set, get)
import Html exposing (Html, text, div, input, ul, li, button)
import Html.Attributes exposing (placeholder, type_, id, width, height)
import Html.Events exposing (onInput, onClick)
import Maybe exposing (withDefault)
import DataStructureHelp exposing (removeFromArray)
import BudgetGraphic
import Account

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

type alias UpdateAcountMsg =
  { accountNum : Int
  , update : Account.Msg }

type Msg =
  UpdateAccount UpdateAcountMsg
  | DeleteAccount Int
  | NewAccount

type alias State = Array Account.State

init : ( State, Cmd Msg )
init = ( Array.fromList [ (Account.init) ], Cmd.none )

update : Msg -> State -> ( State, Cmd Msg )
update action accounts =
  case action of
    UpdateAccount { accountNum, update } ->
      let
          account = get accountNum accounts |> withDefault Account.init
      in
          ( set accountNum (Account.update update account) accounts, Cmd.none )
    DeleteAccount accountNum ->
      ( removeFromArray accountNum accounts, Cmd.none )
    NewAccount ->
      ( push Account.init accounts, Cmd.none )

accountListItem : Int -> Account.State -> Html Msg
accountListItem index account =
  li [] [ div [] [ Html.map (\update -> UpdateAccount { accountNum = index
                                                      , update = update })
                                                      <| Account.view account]
        , button [ onClick  (DeleteAccount index) ] [ text "Delete account" ]]

accountsList : Array Account.State -> Html Msg
accountsList accounts =
  ul [] (Array.toList <| indexedMap (\index account -> accountListItem index account)
                                    accounts)

view : State -> Html Msg
view accounts =
  div
    []
    [ accountsList accounts
    , button [ onClick NewAccount ] [ text "New account" ]
    , BudgetGraphic.view accounts
    ]
