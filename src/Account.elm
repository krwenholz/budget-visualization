module Account exposing (Msg, Model, IncomeEvent, init, update, view)

import Html exposing (Html, text, div, input, ul, li, button, label)
import Html.Attributes exposing (placeholder, type_, id, width, height, class)
import Html.Events exposing (onInput, onClick)
import Array exposing (Array, map, indexedMap, push, set)
import Result exposing (fromMaybe)
import DataStructureHelp exposing (removeFromArray)


type alias UpdateAccountMsg =
    { name : String
    , initialValue : Float
    }


type alias UpdateIncomeEventMsg =
    { eventNum : Int
    , event : IncomeEvent
    }


type Msg
    = UpdateAccount UpdateAccountMsg
    | UpdateIncomeEvent UpdateIncomeEventMsg
    | DeleteIncomeEvent Int
    | NewIncomeEvent


type alias IncomeEvent =
    { name : String
    , flatChange : Float
    , percentChange : Float
    }


updateIncomeEvent : Int -> IncomeEvent -> Msg
updateIncomeEvent eventNum incomeEvent =
    UpdateIncomeEvent
        { eventNum = eventNum
        , event = incomeEvent
        }


emptyIncomeEvent : IncomeEvent
emptyIncomeEvent =
    { name = "Income Event"
    , flatChange = 0.0
    , percentChange = 0.0
    }


type alias Model =
    { name : String
    , initialValue : Float
    , incomeEvents : Array IncomeEvent
    }


init : Model
init =
    { name = "Account Name"
    , initialValue = 10.0
    , incomeEvents = Array.fromList [ (emptyIncomeEvent) ]
    }


update : Msg -> Model -> Model
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
        maybeFloat =
            String.toFloat numberText
    in
        case maybeFloat of
            Ok float ->
                float

            Err msg ->
                0.0


incomeEventInput : IncomeEvent -> Int -> Html Msg
incomeEventInput incomeEvent index =
    div
        [ class "incomeEvent" ]
        [ input
            [ type_ "text"
            , placeholder incomeEvent.name
            , onInput
                (\newName ->
                    UpdateIncomeEvent <|
                        UpdateIncomeEventMsg index { incomeEvent | name = newName }
                )
            , class "incomeEventName"
            ]
            []
        , button [ onClick <| DeleteIncomeEvent index ]
            [ text "-" ]
        , label []
            [ text "Flat monthly change: "
            , input
                [ type_ "number"
                , placeholder <| toString incomeEvent.flatChange
                , onInput
                    (\change ->
                        UpdateIncomeEvent <|
                            UpdateIncomeEventMsg index { incomeEvent | flatChange = readFloat change }
                    )
                , class "incomeEventFlatChange"
                ]
                []
            ]
        , label []
            [ text "Percent yearly change: "
            , input
                [ type_ "number"
                , placeholder <| toString incomeEvent.percentChange
                , onInput
                    (\change ->
                        UpdateIncomeEvent <|
                            UpdateIncomeEventMsg index { incomeEvent | percentChange = readFloat change }
                    )
                , class "incomeEventPercentChange"
                ]
                []
            ]
        ]


incomeEventInputs : Array IncomeEvent -> Html Msg
incomeEventInputs incomeEvents =
    div
        []
        [ ul []
            (Array.toList <|
                indexedMap
                    (\index incomeEvent ->
                        li []
                            [ incomeEventInput incomeEvent index ]
                    )
                    incomeEvents
            )
        , button [ onClick (NewIncomeEvent) ] [ text "+" ]
        ]


view : Model -> Html Msg
view account =
    div [ class "account" ]
        [ input
            [ type_ "text"
            , placeholder account.name
            , onInput
                (\newName ->
                    UpdateAccount
                        { name = newName
                        , initialValue = account.initialValue
                        }
                )
            , class "accountName"
            ]
            []
        , label []
            [ text "$"
            , input
                [ type_ "number"
                , placeholder <| toString account.initialValue
                , onInput
                    (\initialValue ->
                        UpdateAccount
                            { name = account.name
                            , initialValue = readFloat initialValue
                            }
                    )
                , class "accountValue"
                ]
                []
            ]
        , incomeEventInputs account.incomeEvents
        ]
