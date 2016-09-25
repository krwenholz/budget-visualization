{-
def check_account_balance(amount, month):
	if amount < 0:
		raise Exception('We went negative at month_num [{}]'.format(month))

def elapse_time(years, annual_rate_of_return, amount_to_invest, amount_to_vacation):
	table = [('month/year', 'extra', 'investments', 'vacations')]
	investments = 0
	vacations = 0
	extra = 0
	for month_num in range(years * 12):
		month = month_num % 12
		year = month_num // 12
		for fn in income_and_expenses:
			extra += fn(month, year)
		check_account_balance(extra, month_num)

		extra -= amount_to_invest
		investments += amount_to_invest
		investments += (investments * (annual_rate_of_return / 12))
		check_account_balance(extra, month_num)

		extra -= amount_to_vacation
		vacations += amount_to_vacation
		check_account_balance(extra, month_num)

		table.append((str(month+1)+'/'+str(year), extra, investments, vacations))
-}

-- TODO: domain maybe moneymatters.xyz, dollas.xyz, moneystuff.xyz/
-- TODO: one type of income event:
  -- constant: every month they do something to any number of accounts
  -- this limits the focus to things we actually have control over on an every day basis
-- TODO: need some sort of UI and data structure for income rule, UI and terms are nice
  -- [here](http://ux.stackexchange.com/questions/46318/logical-operator-icons)
-- TODO: income events live in currentAccount, take (month, year, masterAccount, currentAccount)
  -- display them: maybe break them down to a dict or tuple expansion OR let people write several simple
    -- logical operators to define them
  -- maybe an income-event belongs in the account logic
-- TODO: accounts need to interact with one another
  -- accounts contain their own income events that get to act on that account and the master account
  -- allow users to order the events/accounts
-- TODO: pretty line graph of account balances (updates on new input, submit to start and focus based later)
  -- would be nice to include optional horizontal bars for each event type, marking when important
    -- events happened

import Html exposing (Html, Attribute, div, fieldset, input, label, text)
import Html.App as App
import Html.Attributes exposing (name, style, type')
import Html.Events exposing (onClick)
import Markdown
import Account



main =
  App.beginnerProgram { model = chapter1, update = update, view = view }


-- MODEL


type alias IncomeEvent = {
  name : String,
  applicableAccount : String,
  percentChange : Float,
  flatChange : Int }


type alias Account = {
	name : String,
  value : Float }

type alias Model = { 
  incomeEvents : List IncomeEvent,
  accounts : List Account }


-- UPDATE


type Msg
  = SwitchTo Account.FontSize


update : Msg -> Model -> Model
update msg model =
  case msg of
    SwitchTo newFontSize ->
      { model | fontSize = newFontSize }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ fieldset []
        [ radio "Small" (SwitchTo Account.Small)
        , radio "Medium" (SwitchTo Account.Medium)
        , radio "Large" (SwitchTo Account.Large)
        ]
    , Markdown.toHtml [ sizeToStyle model.fontSize ] model.content
    ]


radio : String -> msg -> Html msg
radio value msg =
  label
    [ style [("padding", "20px")]
    ]
    [ input [ type' "radio", name "font-size", onClick msg ] []
    , text value
    ]


sizeToStyle : Account.FontSize -> Attribute msg
sizeToStyle fontSize =
  let
    size =
      case fontSize of
        Account.Small ->
          "0.8em"

        Account.Medium ->
          "1em"

        Account.Large ->
          "1.2em"
  in
    style [("font-size", size)]
