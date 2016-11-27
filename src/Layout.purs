module App.Layout where

import App.BudgetVisualization as BudgetVisualization
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import Prelude (($), map)
import Pux.Html (Html, div, h1, text)

data Action
  = Child (BudgetVisualization.Action)
  | PageView Route

type State =
  { route :: Route
  , count :: BudgetVisualization.State }

init :: State
init =
  { route: NotFound
  , count: BudgetVisualization.init }

update :: Action -> State -> State
update (PageView route) state = state { route = route }
update (Child action) state = state { count = BudgetVisualization.update action state.count }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Budget Visualization" ]
    , case state.route of
        Home -> map Child $ BudgetVisualization.view state.count
        NotFound -> NotFound.view state
    ]
