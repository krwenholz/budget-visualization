module App.BudgetVisualization where

import Prelude (const, show, map, ($))
import Pux.Html (Html, div, span, button, text, input, li, ul)
import Pux.Html.Events (onClick, onInput)
import Pux.Html.Attributes (placeholder, type_)
import Data.Array (deleteAt, modifyAt, mapWithIndex, snoc)
import Data.Maybe (fromMaybe)
import Data.Int (fromString) as Int
import App.Account as Account

-- TODO: turn ints into Numbers
-- TODO: Run a simulation based on X number of years
-- TODO: graph it with D3
