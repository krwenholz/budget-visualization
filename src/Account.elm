module Account exposing (Msg, Model, IncomeEvent, decode, encode, init, update, view)

import Array exposing (Array, map, indexedMap, push, set)
import Html exposing (Html, text, div, input, ul, li, button, label)
import Html.Attributes exposing (placeholder, type_, class)
import Html.Events exposing (onInput, onClick)
import Json.Decode as Decode
import Json.Decode.Pipeline as DPipeline
import Json.Encode as Encode
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


encodeIncomeEvent : IncomeEvent -> Encode.Value
encodeIncomeEvent { name, flatChange, percentChange } =
    let
        jname =
            Encode.string name

        jflat =
            Encode.float flatChange

        jpercent =
            Encode.float percentChange
    in
        Encode.object [ ( "name", jname ), ( "flatChange", jflat ), ( "percentChange", jpercent ) ]


encode : Model -> Encode.Value
encode { name, initialValue, incomeEvents } =
    let
        encodedIncomeEvents =
            Encode.array <| Array.map encodeIncomeEvent incomeEvents
    in
        Encode.object [ ( "name", Encode.string name ), ( "initialValue", Encode.float initialValue ), ( "incomeEvents", encodedIncomeEvents ) ]


decodeIncomeEvent : Decode.Decoder IncomeEvent
decodeIncomeEvent =
    DPipeline.decode IncomeEvent
        |> DPipeline.required "name" Decode.string
        |> DPipeline.required "flatChange" Decode.float
        |> DPipeline.required "percentChange" Decode.float


decode : Decode.Decoder Model
decode =
    DPipeline.decode Model
        |> DPipeline.required "name" Decode.string
        |> DPipeline.required "initialValue" Decode.float
        |> DPipeline.required "incomeEvents" (Decode.array <| decodeIncomeEvent)


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
        [ class "income-event" ]
        [ button [ onClick <| DeleteIncomeEvent index, class "delete-income-event prefix" ]
            [ text "-" ]
        , div [ class "input-field" ]
            [ input
                [ type_ "text"
                , placeholder incomeEvent.name
                , onInput
                    (\newName ->
                        UpdateIncomeEvent <|
                            UpdateIncomeEventMsg index { incomeEvent | name = newName }
                    )
                , class "income-event-name"
                ]
                []
            ]
        , div [ class "input-field" ]
            [ label []
                [ text "Flat monthly change" ]
            , input
                [ type_ "number"
                , placeholder <| toString incomeEvent.flatChange
                , onInput
                    (\change ->
                        UpdateIncomeEvent <|
                            UpdateIncomeEventMsg index { incomeEvent | flatChange = readFloat change }
                    )
                , class "income-event-flat-change"
                ]
                []
            ]
        , div [ class "input-field" ]
            [ label []
                [ text "Percent yearly change" ]
            , input
                [ type_ "number"
                , placeholder <| toString incomeEvent.percentChange
                , onInput
                    (\change ->
                        UpdateIncomeEvent <|
                            UpdateIncomeEventMsg index { incomeEvent | percentChange = readFloat change }
                    )
                , class "income-event-percent-change"
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
        , button [ onClick (NewIncomeEvent), class "new-income-event" ] [ text "+" ]
        ]


view : Model -> Html Msg
view account =
    div []
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
            , class "account-name"
            ]
            []
        , div [ class "input-field inline" ]
            [ input
                [ type_ "number"
                , placeholder <| toString account.initialValue
                , onInput
                    (\initialValue ->
                        UpdateAccount
                            { name = account.name
                            , initialValue = readFloat initialValue
                            }
                    )
                , class "account-value"
                ]
                []
            , label []
                [ text "$" ]
            ]
        , incomeEventInputs account.incomeEvents
        ]
