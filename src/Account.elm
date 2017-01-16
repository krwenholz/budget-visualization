module Account exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import List exposing (..)

type alias UpdateAccountMsg = { name : String
                              , initialValue : Float }

type alias UpdateIncomeEventMsg = { eventNum : Int
                                  , event : IncomeEvent }

type Action
  = UpdateAccount UpdateAccountMsg
  | UpdateIncomeEvent UpdateIncomeEventMsg
  | DeleteIncomeEvent Int
  | NewIncomeEvent

type alias IncomeEvent = { name : String
                         , flatChange : Float
                         , percentChange : Float }

updateIncomeEvent : Int -> IncomeEvent -> Action
updateIncomeEvent eventNum incomeEvent =
  UpdateIncomeEvent { eventNum = eventNum
                    , event = incomeEvent }

emptyIncomeEvent : IncomeEvent
emptyIncomeEvent = { name = ""
                   , flatChange = 0.0
                   , percentChange = 0.0 }

type alias State = { name : String
                   , initialValue : Float
                   , incomeEvents : Array IncomeEvent }

init : State
init = { name = ""
       , initialValue = 0.0
       , incomeEvents = [(emptyIncomeEvent)] }

update : Action -> State -> State
update action account =
  case action of
    NewIncomeEvent ->
      { account | incomeEvents = account.incomeEvents push emptyIncomeEvent }
    UpdateAccount { name, initialValue } ->
      { account | name = name, initialValue = initialValue }
    UpdateIncomeEvent { eventNum, event } ->
      { account | incomeEvents = (fromMaybe account.incomeEvents (modifyAt eventNum (const newIncomeEvent) account.incomeEvents)) }
    DeleteIncomeEvent eventNum ->
      { account | incomeEvents = (fromMaybe account.incomeEvents (deleteAt eventNum account.incomeEvents)) }

readFloat : String -> Number
readFloat numberText =
  let
    maybeFloat = readFloat numberText
  in
    if maybeFloat # isNaN then 0.0 else maybeNumber

incomeEventInput : IncomeEvent -> Html IncomeEvent
incomeEventInput incomeEvent =
  div
    []
    [ input [ type_ "text"
            , placeholder incomeEvent.name
            , onInput (\newName -> incomeEvent { name = newName.target.value }) ]
            []
    , input [ type_ "number"
            , placeholder $ show incomeEvent.flatChange
            , onInput (\change -> incomeEvent { flatChange = readFloat change.target.value }) ]
            []
    , input [ type_ "number"
            , placeholder $ show incomeEvent.percentChange
            , onInput (\change -> incomeEvent { percentChange = readFloat change.target.value }) ]
            []
    ]

incomeEventInputs : Array IncomeEvent -> Html Action
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

view : State -> Html Action
view account =
  div
    []
    [ input [ type_ "text"
            , placeholder account.name
            , onInput (\newName -> UpdateAccount { name = newName.target.value
                                                 , initialValue = account.initialValue })
            ]
            []
    , input [ type_ "number"
            , placeholder $ show account.initialValue
            , onInput (\initialValue -> UpdateAccount { name = account.name
                                                      , initialValue = readFloat initialValue.target.value })
            ]
            []
    , incomeEventInputs account.incomeEvents
    ]
