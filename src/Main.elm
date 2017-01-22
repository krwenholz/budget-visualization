module Main exposing (..)

import Html exposing (Html)
import BudgetVisualization

main =
  Html.program
    { init = BudgetVisualization.init
    , view = BudgetVisualization.view
    , update = BudgetVisualization.update
    , subscriptions = \_ -> Sub.none
    }
