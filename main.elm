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

import Html exposing (Html, Attribute, div, fieldset, input, label, text, ol, ul, li)
import Html.App as App
import Html.Attributes exposing (name, style, type', placeholder)
import Html.Events exposing (onClick, onInput)
import String exposing (toFloat, toInt)


main =
  App.beginnerProgram { model = model, update = update, view = view }


-- MODEL
type alias IncomeEvent = {
  name : String,
  change : Int }

type alias Account = {
  name : String,
  initialValue : Float,
  incomeEvents : List IncomeEvent }

type alias Model = List Account

model : Model
model = [Account "" 0 [IncomeEvent "" 0]]

type alias UpdateAccountDetails = {
  oldName : String,
  newName : String,
  initialValue : Float }

type alias UpdateIncomeEventDetails = {
  accountName : String,
  oldName : String,
  newName : String,
  change : Int }

type alias DeleteIncomeEventDetails = {
  accountName : String,
  eventName : String }

type Msg =
  NewAccountMsg
  | NewIncomeEventMsg String
  | UpdateAccountMsg UpdateAccountDetails
  | UpdateIncomeEventMsg UpdateIncomeEventDetails
  | DeleteAccountMsg String
  | DeleteIncomeEventMsg DeleteIncomeEventDetails


-- UPDATE
-- TODO: the update events are now much simpler and should be broken down accordingly
addEvent : List Account -> String -> List Account
addEvent accounts accountName =
  List.map (\account -> if account.name == accountName
                        then { account | incomeEvents = List.append account.incomeEvents [IncomeEvent "" 0]}
                        else account) accounts

updateAccount : List Account -> UpdateAccountDetails -> List Account
updateAccount accounts msg =
  List.map (\account -> if account.name == msg.oldName
                        then { account | name = msg.newName, initialValue = msg.initialValue}
                        else account) accounts

updateIncomeEventInAccount : Account -> UpdateIncomeEventDetails -> Account
updateIncomeEventInAccount account msg =
  { account | incomeEvents = List.map (\incomeEvent -> if incomeEvent.name == msg.oldName
                                                       then IncomeEvent msg.newName msg.change
                                                       else incomeEvent) account.incomeEvents }

updateIncomeEvent : List Account -> UpdateIncomeEventDetails -> List Account
updateIncomeEvent accounts updateMsg =
  List.map (\account -> if account.name == updateMsg.accountName
                        then updateIncomeEventInAccount account updateMsg
                        else account) accounts

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateAccountMsg updateMsg ->
      updateAccount model updateMsg
    UpdateIncomeEventMsg updateMsg ->
      updateIncomeEvent model updateMsg
    NewAccountMsg ->
      List.append model [Account "" 0 [IncomeEvent "" 0]]
    NewIncomeEventMsg accountName ->
      addEvent model accountName
    DeleteAccountMsg accountName ->
      List.filter (\account -> account.name /= accountName) model
    DeleteIncomeEventMsg deleteMsg ->
      model -- TODO: delete the income event


-- VIEW
incomeEventInputs : List IncomeEvent -> String -> Html Msg
incomeEventInputs incomeEvents accountName =
  ul [] (List.map (\ie ->
    li []
      [ input [ type' "text",
                placeholder ie.name,
                onInput (\newName -> UpdateIncomeEventMsg (UpdateIncomeEventDetails accountName
                                                                                    ie.name
                                                                                    newName
                                                                                    ie.change)) ]
              []
      , input [ type' "number",
                placeholder (toString ie.change),
                onInput (\change -> UpdateIncomeEventMsg
                                      (UpdateIncomeEventDetails accountName
                                                                ie.name
                                                                ie.name
                                                                (Result.withDefault 0 (String.toInt change)))) ]
              []
      ]
  ) incomeEvents)

accountInputs : List Account -> List (Html Msg)
accountInputs accounts =
  List.map (\account ->
    div []
      [ input [ type' "text", placeholder account.name,
               onInput (\newName -> UpdateAccountMsg (UpdateAccountDetails account.name
                                                         newName
                                                         account.initialValue)) ]
             []
      , input [ type' "number", placeholder (toString account.initialValue),
                onInput (\value -> UpdateAccountMsg
                                     (UpdateAccountDetails account.name
                                                           account.name
                                                           (Result.withDefault 0 (String.toFloat value)))) ]
              []
      , (incomeEventInputs account.incomeEvents account.name)
      ]
  ) accounts

-- TODO: function to compute graph based on current model! Hooray!
-- TODO: delete buttons
-- TODO: function to set + button displays based on the accounts and income events that require names
        -- hover text could tell folks they need to input names

showModel : Model -> Html a
showModel model =
  ol [] (List.map (\account -> li [] [(text ("name: " ++ account.name  ++ " val: " ++ (toString account.initialValue))),
                                             ol [] (List.map (\ie -> li [] [text ("name: " ++ ie.name ++ " change: " ++ (toString ie.change))])
                                                             account.incomeEvents)])
                   model)

view : Model -> Html Msg
view model =
  div [] [div [] (accountInputs model), div [] [showModel model]]
