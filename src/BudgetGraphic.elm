module BudgetGraphic exposing (..)

import Html exposing (Html, text, div, input, ul, li, button, canvas)
import Html.Attributes exposing (placeholder, type_, id, width, height)
import Html.Events exposing (onInput, onClick)
import Array exposing (map, indexedMap, push, set, Array)

type State = Int -- TODO: define the line input (probably an array or list of them)
-- A line probably has a name and data (date, value (dollars))

-- TODO: Run a simulation based on X number of years
-- TODO: graph it with D3
view lines =
  div
    []
    [ canvas [ id "visualization", width 400, height 400 ] [] ]
