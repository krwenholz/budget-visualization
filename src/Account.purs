module App.Account where

import Prelude (const, show, map, ($), id, (#))
import Pux.Html (Html, div, span, button, text, input, li, ul)
import Pux.Html.Events (onClick, onInput)
import Pux.Html.Attributes (placeholder, type_)
import Data.Array (deleteAt, modifyAt, mapWithIndex, snoc)
import Data.Maybe (fromMaybe)
import Global (isNaN, readFloat)

data Action
  = UpdateAccount { name :: String -- TODO: Update could be an either or a few types to be cleaner
                  , initialValue :: Number }
  | UpdateIncomeEvent { eventNum :: Int
                      , event :: IncomeEvent }
  | DeleteIncomeEvent Int
  | NewIncomeEvent

type IncomeEvent = { name :: String
                   , flatChange :: Number
                   , percentChange :: Number }

updateIncomeEvent :: Int -> IncomeEvent -> Action
updateIncomeEvent eventNum incomeEvent =
  UpdateIncomeEvent { eventNum: eventNum
                    , event: incomeEvent }

emptyIncomeEvent :: IncomeEvent
emptyIncomeEvent = { name: ""
                   , flatChange: 0.0
                   , percentChange: 0.0 }

type State = { name :: String
             , initialValue :: Number
             , incomeEvents :: Array IncomeEvent }

init :: State
init = { name: ""
       , initialValue: 0.0
       , incomeEvents: [(emptyIncomeEvent)] }

update :: Action -> State -> State
update NewIncomeEvent account =
  account { incomeEvents = account.incomeEvents `snoc` emptyIncomeEvent }
update (UpdateAccount { name: name, initialValue: initialValue }) account =
  account { name = name, initialValue = initialValue }
update (UpdateIncomeEvent { eventNum: eventNum , event: newIncomeEvent }) account =
  account { incomeEvents = (fromMaybe account.incomeEvents
          (modifyAt eventNum (const newIncomeEvent) account.incomeEvents)) }
update (DeleteIncomeEvent eventNum) account =
  account { incomeEvents = (fromMaybe account.incomeEvents
                                      (deleteAt eventNum account.incomeEvents)) }

readNumber :: String -> Number
readNumber numberText =
  if maybeNumber # isNaN then 0.0 else maybeNumber
  where
    maybeNumber = readFloat numberText

incomeEventInput :: IncomeEvent -> Html IncomeEvent
incomeEventInput incomeEvent =
  div
    []
    [ input [ type_ "text"
            , placeholder incomeEvent.name
            , onInput (\newName -> incomeEvent { name = newName.target.value }) ]
            []
    , input [ type_ "number"
            , placeholder $ show incomeEvent.flatChange
            , onInput (\change -> incomeEvent { flatChange = readNumber change.target.value }) ]
            []
    , input [ type_ "number"
            , placeholder $ show incomeEvent.percentChange
            , onInput (\change -> incomeEvent { percentChange = readNumber change.target.value }) ]
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
                                                      , initialValue: readNumber initialValue.target.value })
            ]
            []
    , incomeEventInputs account.incomeEvents
    ]
