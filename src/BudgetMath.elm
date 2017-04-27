module BudgetMath exposing (..)

import Html exposing (Html, text, div, input, ul, li, button, canvas)
import Array exposing (map, indexedMap, push, set, foldl, Array)
import List
import Account


type alias AccountTrend =
    { label :
        String
        -- account name
    , data : List Float
    }


type alias Model =
    { labels :
        List Int
        -- months
    , datasets : List AccountTrend
    }


applyEvent : Float -> Account.IncomeEvent -> Float -> Float
applyEvent initialValue { name, flatChange, percentChange } currentValue =
    currentValue + (initialValue * (0.01 * (percentChange / 12))) + flatChange


valueAfterEvents : Array Account.IncomeEvent -> Float -> Float
valueAfterEvents incomeEvents initialValue =
    foldl (applyEvent initialValue) initialValue incomeEvents


expandAccount : Int -> Int -> Float -> Array Account.IncomeEvent -> List Float
expandAccount maxMonth currentMonth initialValue incomeEvents =
    if maxMonth == currentMonth then
        []
    else
        let
            value =
                valueAfterEvents incomeEvents initialValue
        in
            value :: expandAccount maxMonth (currentMonth + 1) value incomeEvents


asData : Array Account.Model -> Model
asData accounts =
    { labels = List.range 0 120
    , datasets =
        Array.toList <|
            map
                (\{ name, initialValue, incomeEvents } ->
                    AccountTrend name (expandAccount 120 0 initialValue incomeEvents)
                )
                accounts
    }


showData : Model -> Html msg
showData { labels, datasets } =
    ul []
        (List.map
            (\{ label, data } ->
                li []
                    [ text label
                    , ul [] (List.map (\y -> li [] [ text <| "value: " ++ (toString y) ]) data)
                    ]
            )
            datasets
        )


view : Array Account.Model -> Html msg
view accounts =
    div [] [ showData <| asData accounts ]
