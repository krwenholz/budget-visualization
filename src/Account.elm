module Account exposing (..)

import Html exposing (Html, text, div, input, ul, li, button)
import Html.Attributes exposing (placeholder, type_, id, width, height)
import Html.Events exposing (onInput, onClick)
import Array exposing (Array, map, indexedMap, push, set)
import Result exposing (fromMaybe)
import DataStructureHelp exposing (removeFromArray)

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
       , incomeEvents = Array.fromList [(emptyIncomeEvent)] }

update : Action -> State -> State
update action account =
  case action of
    NewIncomeEvent ->
      { account | incomeEvents = push emptyIncomeEvent account.incomeEvents }
    UpdateAccount { name, initialValue } ->
      { account | name = name, initialValue = initialValue }
    UpdateIncomeEvent { eventNum, event } ->
      { account | incomeEvents = set eventNum event account.incomeEvents }
    DeleteIncomeEvent eventNum ->
      { account | incomeEvents = removeFromArray eventNum account.incomeEvents }

readFloat : String -> Float
readFloat numberText =
  let
    maybeFloat = readFloat numberText
  in
    if maybeFloat |> isNaN then 0.0 else maybeFloat

incomeEventInput : IncomeEvent -> Int -> Html Action
incomeEventInput incomeEvent index =
  div
    []
    [ input [ type_ "text"
            , placeholder incomeEvent.name
            , onInput (\newName -> UpdateIncomeEvent <|
                                   UpdateIncomeEventMsg index { incomeEvent | name = newName }) ]
            []
    , input [ type_ "number"
            , placeholder <| toString incomeEvent.flatChange
            , onInput (\change -> UpdateIncomeEvent <|
                                  UpdateIncomeEventMsg index { incomeEvent | flatChange = readFloat change }) ]
            []
    , input [ type_ "number"
            , placeholder <| toString incomeEvent.percentChange
            , onInput (\change -> UpdateIncomeEvent <|
                                  UpdateIncomeEventMsg index { incomeEvent | percentChange = readFloat change }) ]
            []
    ]

incomeEventInputs : Array IncomeEvent -> Html Action
incomeEventInputs incomeEvents =
  div
    []
    [ ul [] (Array.toList <| indexedMap
                             (\index incomeEvent ->
                               li [] [ incomeEventInput incomeEvent index
                                     , button [ onClick <| DeleteIncomeEvent index ]
                                              [ text "-" ]
                                     ])
                incomeEvents)
    , button [ onClick (NewIncomeEvent) ] [ text "New income event" ]
    ]

view : State -> Html Action
view account =
  div
    []
    [ input [ type_ "text"
            , placeholder account.name
            , onInput (\newName -> UpdateAccount { name = newName
                                                 , initialValue = account.initialValue })
            ]
            []
    , input [ type_ "number"
            , placeholder <| toString account.initialValue
            , onInput (\initialValue -> UpdateAccount { name = account.name
                                                      , initialValue = readFloat initialValue })
            ]
            []
    , incomeEventInputs account.incomeEvents
    ]
