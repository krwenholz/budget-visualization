module App.BudgetGraphic where

import Prelude (const, show, map, ($), (<>))
import Pux.Html (Html, div, canvas, script, text)
import Pux.Html.Events (onClick, onInput)
import Pux.Html.Attributes (placeholder, type_, id_, width, height)
import Data.Array (deleteAt, modifyAt, mapWithIndex, snoc)
import Data.Maybe
import Data.Int (fromString) as Int
import App.Account as Account
import App.ChartJs as ChartJs

type State = Int -- TODO: define the line input (probably an array or list of them)
-- A line probably has a name and data (date, value (dollars))

-- TODO: Run a simulation based on X number of years
-- TODO: graph it with D3
view :: forall t1 t2. t1 -> Html t2
view lines = do
  ChartJs.drawChart "XXXXXXXXXXXXXXXXXXX" "---------------------"
  div
    []
    [ canvas [ id_ "visualization", width "400", height "400" ] [] ]
