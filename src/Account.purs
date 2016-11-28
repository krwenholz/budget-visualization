module App.Account where

import Prelude (const, show, map, ($))
import Pux.Html (Html, div, span, button, text, input, li, ul)
import Pux.Html.Events (onClick, onInput)
import Pux.Html.Attributes (placeholder, type_)
import Data.Array (deleteAt, modifyAt, mapWithIndex, snoc)
import Data.Maybe (fromMaybe)
import Data.Int (fromString) as Int

-- TODO: build my own parser for Numbers or upgrade the simple-parser library
-- TODO: turn Ints into Numbers
data Action
  = UpdateAccount { name :: String
                  , initialValue :: Int }
  |UpdateIncomeEvent { eventNum :: Int
                     , name :: String
                     , change :: Int }
  | DeleteIncomeEvent Int
  | NewIncomeEvent

type IncomeEvent = { name :: String
                   , change :: Int }

updateIncomeEvent :: Int -> IncomeEvent -> Action
updateIncomeEvent eventNum { name: name, change: change } =
  UpdateIncomeEvent { eventNum: eventNum
                    , name: name
                    , change: change }

emptyIncomeEvent :: IncomeEvent
emptyIncomeEvent = { name: ""
                  , change: 0 }

type State = { name :: String
             , initialValue :: Int
             , incomeEvents :: Array IncomeEvent }

init :: State
init = { name: ""
       , initialValue: 0
       , incomeEvents: [(emptyIncomeEvent)] }

update :: Action -> State -> State
update NewIncomeEvent account =
  account { incomeEvents = account.incomeEvents `snoc` emptyIncomeEvent }
update (UpdateAccount { name: name, initialValue: initialValue }) account =
  account { name = name, initialValue = initialValue }
update (UpdateIncomeEvent { eventNum: eventNum, name: name, change: change}) account =
  account { incomeEvents = (fromMaybe account.incomeEvents
                                      (modifyAt eventNum (\_ -> newIncomeEvent) account.incomeEvents)) }
  where
    newIncomeEvent = { name: name, change: change}
update (DeleteIncomeEvent eventNum) account =
  account { incomeEvents = (fromMaybe account.incomeEvents
                                      (deleteAt eventNum account.incomeEvents)) }

incomeEventInput :: IncomeEvent -> Html IncomeEvent
incomeEventInput incomeEvent =
  div
    []
    [ input [ type_ "text"
            , placeholder incomeEvent.name
            , onInput (\newName -> { name: newName.target.value, change: incomeEvent.change }) ]
            []
    , input [ type_ "number"
            , placeholder $ show incomeEvent.change
            , onInput (\change -> { name: incomeEvent.name
                                  , change: (fromMaybe 0 $ Int.fromString change.target.value) }) ]
            []
    ]

incomeEventInputs :: Array IncomeEvent -> Html Action
incomeEventInputs incomeEvents =
  div
    []
    [ ul [] (mapWithIndex (\index incomeEvent ->
                          li
                            []
                            [ map (updateIncomeEvent index) $ (incomeEventInput incomeEvent)
                            , button [ onClick $ const $ DeleteIncomeEvent index ] [ text "-" ]
                            ])
                          incomeEvents)
    , button [ onClick (const NewIncomeEvent) ] [ text "New income event" ]
    ]

view :: State -> Html Action
view account =
  div
    []
    [ input [ type_ "text"
            , placeholder account.name
            , onInput (\newName -> UpdateAccount { name: newName.target.value
                                                 , initialValue: account.initialValue })
            ]
            []
    , input [ type_ "number"
            , placeholder $ show account.initialValue
            , onInput (\initialValue -> UpdateAccount { name: account.name
                                                      , initialValue: fromMaybe 0 $ Int.fromString initialValue.target.value })
            ]
            []
    , incomeEventInputs account.incomeEvents
    ]
