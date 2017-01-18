module BudgetVisualization exposing (..)

import Html exposing (Html, text, div, input, ul, li, button)
import Html.Attributes exposing (placeholder, type_, id, width, height)
import Html.Events exposing (onInput, onClick)
import Array exposing (Array, map, indexedMap, push, set, get)
import DataStructureHelp exposing (removeFromArray)
import BudgetGraphic
import Account

type alias UpdateAcountMsg =
  { accountNum : Int
  , update : Account.Action }


type Action =
  UpdateAccount UpdateAcountMsg
  | DeleteAccount Int
  | NewAccount

type State = Array Account.State

init : State
init = [ (Account.init) ]

update : Action -> State -> State
update action accounts =
  case action of
    UpdateAccount { accountNum, update } ->
      let
          account = get accountNum accounts
      in
          set accountNum (Account.update update account) accounts
    DeleteAccount accountNum ->
      removeFromArray accountNum accounts
    NewAccount ->
      push Account.init accounts

view : State -> Html Action
view accounts =
  div
    []
    [ ul [] (indexedMap (\index account ->
                          li
                            []
                            [ map (\update -> UpdateAccount { accountNum = index
                                                            , update = update })
                                  <| (Account.view account)
                            , button [ onClick  (DeleteAccount index) ] [ text "Delete account" ]
                            ])
                          accounts)
    , button [ onClick NewAccount ] [ text "New account" ]
    , BudgetGraphic.view 1
    ]

