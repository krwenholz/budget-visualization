module BudgetVisualization exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import List exposing (..)
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
      fromMaybe accounts $ modifyAt accountNum (\account -> Account.update update account) accounts
    DeleteAccount accountNum ->
      fromMaybe accounts $ deleteAt accountNum accounts
    NewAccount ->
      accounts push Account.init

view : State -> Html Action
view accounts =
  div
    []
    [ ul [] (mapWithIndex (\index account ->
                          li
                            []
                            [ map (\update -> UpdateAccount { accountNum = index
                                                            , update = update })
                                  $ (Account.view account)
                            , button [ onClick $ const $ DeleteAccount index ] [ text "Delete account" ]
                            ])
                          accounts)
    , button [ onClick (const NewAccount) ] [ text "New account" ]
    , BudgetGraphic.view 1
    ]

