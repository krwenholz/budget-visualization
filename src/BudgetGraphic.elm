module BudgetGraphic exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import List exposing (..)

type State = Int -- TODO: define the line input (probably an array or list of them)
-- A line probably has a name and data (date, value (dollars))

-- TODO: Run a simulation based on X number of years
-- TODO: graph it with D3
view lines =
  div
    []
    [ canvas [ id "visualization", width 400, height 400 ] [] ]
