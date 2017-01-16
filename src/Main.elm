module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import List exposing (..)
import BudgetVisualization

main =
  Html.program
    { init = BudgetVisualization.init
    , view = BudgetVisualization.view
    , update = BudgetVisualization.update
    }
