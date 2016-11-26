module App.Counter where

import Prelude ((+), (-), const, show)
import Pux.Html (Html, div, span, button, text)
import Pux.Html.Events (onClick)
import Data.Array (deleteAt, insertAt)
import Data.Maybe (fromMaybe)

data Action
  = UpdateAccount { name :: String
                  , initialValue :: Float }
  |UpdateIncomeEvent { eventNum :: Int
                      , name :: String
                      , change :: Int }
  | DeleteIncomeEvent Int
  | NewIncomeEvent

type IncomeEvent = {
  name :: String,
  change :: Int }

initIncomeEvent :: IncomeEvent
initIncomeEvent = { name: ""
                 , change: 0 }

type Account = {
  name :: String,
  initialValue :: Float,
  incomeEvents :: Array IncomeEvent }

init :: Account
init = { name: ""
       , initialValue: 0
       , incomeEvents: [(initIncomeEvent)] }

update :: Action -> Account -> Account
update NewIncomeEvent account =
  account { incomeEvents = (initIncomeEvent : account.incomeEvents) }
update (UpdateAccount { name: name, initialValue: initialValue }) account =
  account { name = name, initialValue = initialValue }
update (UpdateIncomeEvent { eventNum: eventNum, name: name, initialValue: initialValue }) account =
  let
    newIncomeEvent = { name: name, initialValue: initialValue }
  in
    account { incomeEvents = (fromMaybe account.incomeEvents
                                        (modifyAt eventNum (\_ -> newEvent) account.incomeEvents)) }
update (DeleteIncomeEvent eventNum) account =
  account { incomeEvents = (fromMaybe account.incomeEvents
                                      (deleteAt eventNum account.incomeEvents)) }

view :: Account -> Html Action
-- TODO: make a single Account view that can dispatch to its own income events
view state =
  div
    []
    [ button [ onClick (const Increment) ] [ text "Increment" ]
    , span [] [ text (show state) ]
    , button [ onClick (const Decrement) ] [ text "Decrement" ]
    ]

-- buildIncomeEventInputs : Array IncomeEvent -> Int -> Html Msg
-- buildIncomeEventInputs incomeEvents accountNum =
--   ul [] (List.append (Array.toList
--       (Array.indexedMap (\eventNum ie ->
--         li []
--           [ input [ type_ "text",
--                     placeholder ie.name,
--                     onInput (\newName -> UpdateIncomeEventMsg (UpdateIncomeEventDetails accountNum
--                                                                                         eventNum
--                                                                                         newName
--                                                                                         ie.change)) ]
--                   []
--           , input [ type_ "number",
--                     placeholder (toString ie.change),
--                     onInput (\change -> UpdateIncomeEventMsg
--                                           (UpdateIncomeEventDetails accountNum
--                                                                     eventNum
--                                                                     ie.name
--                                                                     (Result.withDefault 0 (String.toInt change)))) ]
--                   []
--           , button [ onClick (DeleteIncomeEventMsg (DeleteIncomeEventDetails accountNum eventNum)) ] [ text "-" ]
--           ]
--       ) incomeEvents))
--     [ button [ onClick (NewIncomeEventMsg accountNum) ] [ text "New income event" ] ])
-- 
-- buildAccountInputs : Array Account -> List (Html Msg)
-- buildAccountInputs accounts =
--   List.append (Array.toList
--     <| Array.indexedMap (\accountNum account ->
--       div []
--         [ input [ type_ "text", placeholder account.name,
--                  onInput (\newName -> UpdateAccountMsg <| UpdateAccountDetails accountNum
--                                                                                newName
--                                                                                account.initialValue) ]
--                []
--         , input [ type_ "number", placeholder (toString account.initialValue),
--                   onInput (\value -> UpdateAccountMsg
--                                        <| UpdateAccountDetails accountNum
--                                                                account.name
--                                                                (Result.withDefault 0 <| String.toFloat value)) ]
--                 []
--         , button [ onClick (DeleteAccountMsg accountNum) ] [ text "-" ]
--         , buildIncomeEventInputs account.incomeEvents accountNum
--         ]
--     ) accounts)
--     [ button [ onClick NewAccountMsg ] [ text "New account" ] ]
