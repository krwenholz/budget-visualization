module Main where

import App.Counter (update, accountInput, init)
import Control.Monad.Eff (Eff)
import Prelude (bind, Unit)
import Pux (start, fromSimple, renderToDOM, CoreEffects)

main :: forall e. Eff (CoreEffects e) Unit
main = do
  app <- start
    { initialState: init
    , update: fromSimple update
    , view: accountInput
    , inputs: [] }

  renderToDOM "#app" app.html
