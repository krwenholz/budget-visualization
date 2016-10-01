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


main =
  App.beginnerProgram { model = chapter1, update = update, view = view }


-- MODEL
type alias IncomeEvent = {
  name : String,
  change : Int }

type alias Account = {
	name : String,
  initialValue : Float,
  incomeEvents : List IncomeEvent }

type alias Model = {
  incomeEvents : List IncomeEvent,
  accounts : List Account }

type UpdateAccountMsg = {
	name : String,
  initialValue : Float,
  incomeEventName: Maybe IncomeEvent }

type Msg = UpdateAccountMsg

model : Model
model = Model [IncomeEvent "" "" 0 0] [Account "" 0]


-- UPDATE
hasName : a -> String -> Bool
hasName name { otherName } = name == otherName

containsItemWithName : List a -> String -> Bool
containsItemWithName list name =
  List.any (\{ otherName } -> name == otherName)

update : Msg -> Model -> Model
update msg model =
  case msg of
    IncomeEventMsg incomeEvent ->
        { model | incomeEvents =
            incomeEvent :: (List.filter (not hasName incomeEvent.name) model.incomeEvents) }
    AccountMsg account ->
        { model | accounts =
            account :: (List.filter (not hasName account.name) model.accounts) }


-- VIEW
inputIncomeEvents : List IncomeEvent -> List Account -> Html
inputIncomeEvents events accounts=
  List.map (\ie -> [ input [ type' "text", placeholder "Name", onInput TODO ] [],
                     select [] (List.map (\account -> account.name) accounts),
                     input [ type' "number", placeholder "Monthly change (flat)", onInput TODO ] [],
                     input [ type' "number", placeholder "Monthly change (percentage)", onInput TODO ] [])
view : Model -> Html Msg
view model =
  div [] (List.map (\ie -> input )
    [ input [ type' "text", placeholder "Name", onInput Name ] []
    , input [ type' "password", placeholder "Password", onInput Password ] []
    , input [ type' "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , viewValidation model
    ]
