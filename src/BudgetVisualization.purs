module App.BudgetVisualization where

import Prelude (const, show, map, ($))
import Pux.Html (Html, div, span, button, text, input, li, ul)
import Pux.Html.Events (onClick, onInput)
import Pux.Html.Attributes (placeholder, type_)
import Data.Array (deleteAt, modifyAt, mapWithIndex, snoc)
import Data.Maybe (fromMaybe)
import Data.Int (fromString) as Int
import App.Account as Account

data Action
  = UpdateAccount { accountNum :: Int
                  , update :: Account.Action }
  | DeleteAccount Int
  | NewAccount

type State = Array Account.State

init :: State
init = [ (Account.init) ]

update :: Action -> State -> State
update (UpdateAccount { accountNum: accountNum, update: update }) accounts =
  fromMaybe account.incomeEvents $ modifyAt eventNum
                                            (\account -> Account.update update account)
                                            accounts
update (DeleteAccount accountNum) accounts =
  fromMaybe account.incomeEvents $ deleteAt accountNum accounts
update NewAccount accounts =
  accounts `snoc` Account.init

view :: State -> Html Action
view accounts =
  div
    []
    [ ul [] (mapWithIndex (\index account ->
                          li
                            []
                            [ map (\update -> UpdateAccount { accountNum: index
                                                            , update: update })
                                  $ (Account.view account)
                            , button [ onClick $ const $ DeleteAccount index ] [ text "-" ]
                            ])
                          accounts)
    , button [ onClick (const NewAccount) ] [ text "New account" ]
    ]

