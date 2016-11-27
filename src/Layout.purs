module App.Layout where

import App.Account as Account
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import Prelude (($), map)
import Pux.Html (Html, div, h1, p, text)

data Action
  = Child (Account.Action)
  | PageView Route

type State =
  { route :: Route
  , count :: Account.State }

init :: State
init =
  { route: NotFound
  , count: Account.init }

update :: Action -> State -> State
update (PageView route) state = state { route = route }
update (Child action) state = state { count = Account.update action state.count }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Pux Starter App" ]
    , p [] [ text "Change src/Layout.purs and watch me hot-reload." ]
    , case state.route of
        Home -> map Child $ Account.view state.count
        NotFound -> NotFound.view state
    ]
