module App.BudgetGraphic where

import Prelude (const, show, map, ($), (<>))
import Pux.Html (Html, div, canvas, script, text)
import Pux.Html.Events (onClick, onInput)
import Pux.Html.Attributes (placeholder, type_, id_, width, height)
import Data.Array (deleteAt, modifyAt, mapWithIndex, snoc)
import Data.Maybe
import Data.Int (fromString) as Int
import App.Account as Account

type State = Int -- TODO: define the line input (probably an array or list of them)
-- A line probably has a name and data (date, value (dollars))

-- TODO: Run a simulation based on X number of years
-- TODO: graph it with D3
view :: forall t1 t2. t1 -> Html t2
view lines =
  div
    []
    [ canvas [ id_ "visualization", width "400", height "400" ] []
    , script [] [ text $ """
             var ctx = document.getElementById("visualization");
						 var myChart = new Chart(ctx, {
						     type: 'bar',
						     data: {
						         labels: ["Red", "Blue", "Yellow", "Green", "Purple", "Orange"],
						         datasets: [{
						             label: '# of Votes',
						             data: [12, 19, 3, 5, 2, 3],
						             backgroundColor: [
						                 'rgba(255, 99, 132, 0.2)',
						                 'rgba(54, 162, 235, 0.2)',
						                 'rgba(255, 206, 86, 0.2)',
						                 'rgba(75, 192, 192, 0.2)',
						                 'rgba(153, 102, 255, 0.2)',
						                 'rgba(255, 159, 64, 0.2)'
						             ],
						             borderColor: [
						                 'rgba(255,99,132,1)',
						                 'rgba(54, 162, 235, 1)',
						                 'rgba(255, 206, 86, 1)',
						                 'rgba(75, 192, 192, 1)',
						                 'rgba(153, 102, 255, 1)',
						                 'rgba(255, 159, 64, 1)'
						             ],
						             borderWidth: 1
						         }]
						     },
						     options: {
						         scales: {
						             yAxes: [{
						                 ticks: {
						                     beginAtZero:true
						                 }
						             }]
						         }
						     }
						 });
             """ ] -- TODO: define a simple chart here that affects the canvas
    ]
