module BudgetGraphic exposing (..)

import Html exposing (Html, text, div, input, ul, li, button, canvas)
import Html.Attributes exposing (placeholder, type_, id, width, height)
import Html.Events exposing (onInput, onClick)
import Array exposing (map, indexedMap, push, set, foldl, Array)
import List
import Account

type alias Point = { month : Int, value : Float }

type alias AccountTrend = { name : String
                          , trend : List Point }

type alias State = List AccountTrend

applyEvent : Float -> Account.IncomeEvent -> Float -> Float
applyEvent initialValue { name, flatChange, percentChange } currentValue =
  if flatChange == 0 then
    currentValue + (initialValue * percentChange)
  else
    currentValue + flatChange

valueAfterEvents : Array Account.IncomeEvent -> Float -> Float
valueAfterEvents incomeEvents initialValue =
  foldl (applyEvent initialValue) initialValue incomeEvents

expandAccount : Int -> Int -> Float -> Array Account.IncomeEvent -> List Point
expandAccount maxMonth currentMonth initialValue incomeEvents =
  if maxMonth == currentMonth then []
  else
    let
      value = valueAfterEvents incomeEvents initialValue
    in
      { month = currentMonth
      , value = value } :: expandAccount maxMonth (currentMonth + 1) value incomeEvents

asData : Array Account.State -> State
asData accounts =
  Array.toList <| map (\{ name, initialValue, incomeEvents } ->
                        AccountTrend name (expandAccount 48 0 initialValue incomeEvents))
                        accounts

showData : State -> Html msg
showData trends =
  ul [] (List.map (\{ name, trend }-> li [] [ text name
        , ul [] (List.map (\{ month, value }-> li [] [ text <| "month: " ++ (toString month) ++ " value: " ++ (toString value) ]) trend) ]) trends)

-- TODO: Run a simulation based on X number of years
-- TODO: graph it with D3
-- TODO: fix signature
-- view : Array Account.State -> Html
view accounts =
  div
    []
    [ showData <| asData accounts
    , canvas [ id "visualization", width 400, height 400 ] [] ]
