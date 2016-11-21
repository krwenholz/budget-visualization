-- TODO: accounts need to interact with one another
  -- accounts contain their own income events that get to act on that account and the master account
  -- allow users to order the events/accounts
-- TODO: pretty line graph of account balances (updates on new input, submit to start and focus based later)
  -- would be nice to include optional horizontal bars for each event type, marking when important
    -- events happened

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String exposing (toFloat, toInt)
import Array exposing (Array)
import Maybe exposing (withDefault)


main = Html.beginnerProgram { model = model, update = update, view = view }


-- MODEL
type alias IncomeEvent = {
  name : String,
  change : Int }

type alias Account = {
  name : String,
  initialValue : Float,
  incomeEvents : Array IncomeEvent }

newAccount : Account
newAccount = Account "" 0 (Array.fromList([ IncomeEvent "" 0 ]))

type alias Model = Array Account

model : Model
model = Array.fromList([newAccount])

type alias UpdateAccountDetails = {
  accountNum : Int,
  name : String,
  initialValue : Float }

type alias UpdateIncomeEventDetails = {
  accountNum : Int,
  eventNum : Int,
  name : String,
  change : Int }

type alias DeleteIncomeEventDetails = {
  accountNum : Int,
  eventNum : Int }

type Msg =
  NewAccountMsg
  | NewIncomeEventMsg Int
  | UpdateAccountMsg UpdateAccountDetails
  | UpdateIncomeEventMsg UpdateIncomeEventDetails
  | DeleteAccountMsg Int
  | DeleteIncomeEventMsg DeleteIncomeEventDetails


-- UPDATE
removeFromList i xs =
    (List.take i xs) ++ (List.drop (i+1) xs)

removeFromArray i =
    Array.toList >> removeFromList i >> Array.fromList

getAccount : Int -> Array Account -> Account
getAccount accountNum accounts =
    withDefault newAccount (Array.get accountNum accounts)

newIncomeEvent : Array Account -> Int -> Array Account
newIncomeEvent accounts accountNum =
  let
    account = getAccount accountNum accounts
    newEvent = IncomeEvent "" 0
  in
    Array.set accountNum
              { account | incomeEvents = (Array.push newEvent account.incomeEvents) }
              model

deleteIncomeEvent : Array Account -> DeleteIncomeEventDetails -> Array Account
deleteIncomeEvent accounts { accountNum, eventNum }=
  let
    account = getAccount accountNum accounts
  in
    Array.set accountNum
              { account | incomeEvents = (removeFromArray eventNum account.incomeEvents) }
              model

updateAccount : Array Account -> UpdateAccountDetails -> Array Account
updateAccount accounts { accountNum, name, initialValue } =
  let
    account = getAccount accountNum accounts
  in
    Array.set accountNum { account | name = name, initialValue = initialValue } accounts

updateIncomeEvent : Array Account -> UpdateIncomeEventDetails -> Array Account
updateIncomeEvent accounts msg =
  let
    account = getAccount msg.accountNum accounts
    newEvent = IncomeEvent msg.name msg.change
  in
    Array.set msg.accountNum
              { account | incomeEvents = Array.set msg.eventNum newEvent account.incomeEvents }
              accounts

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateAccountMsg updateMsg ->
      updateAccount model updateMsg
    UpdateIncomeEventMsg updateMsg ->
      updateIncomeEvent model updateMsg
    NewAccountMsg ->
      Array.push (newAccount) model
    NewIncomeEventMsg accountNum ->
      newIncomeEvent model accountNum
    DeleteAccountMsg accountNum ->
      removeFromArray accountNum model
    DeleteIncomeEventMsg deleteMsg ->
      deleteIncomeEvent model deleteMsg


-- VIEW
-- TODO: use the list data types
buildIncomeEventInputs : Array IncomeEvent -> Int -> Html Msg
buildIncomeEventInputs incomeEvents accountNum =
  ul [] (List.append (Array.toList
      (Array.indexedMap (\index ie ->
        li []
          [ input [ type_ "text",
                    placeholder ie.name,
                    onInput (\newName -> UpdateIncomeEventMsg (UpdateIncomeEventDetails accountNum
                                                                                        index
                                                                                        newName
                                                                                        ie.change)) ]
                  []
          , input [ type_ "number",
                    placeholder (toString ie.change),
                    onInput (\change -> UpdateIncomeEventMsg
                                          (UpdateIncomeEventDetails accountNum
                                                                    index
                                                                    ie.name
                                                                    (Result.withDefault 0 (String.toInt change)))) ]
                  []
          ]
      ) incomeEvents))
    [ button [ onClick (NewIncomeEventMsg accountNum) ] [ text "New income event" ] ])

buildAccountInputs : Array Account -> List (Html Msg)
buildAccountInputs accounts =
  List.append (Array.toList
    (Array.indexedMap (\index account ->
      div []
        [ input [ type_ "text", placeholder account.name,
                 onInput (\newName -> UpdateAccountMsg (UpdateAccountDetails index
                                                                             newName
                                                                             account.initialValue)) ]
               []
        , input [ type_ "number", placeholder (toString account.initialValue),
                  onInput (\value -> UpdateAccountMsg
                                       (UpdateAccountDetails index
                                                             account.name
                                                             (Result.withDefault 0 (String.toFloat value)))) ]
                []
        , buildIncomeEventInputs account.incomeEvents index
        ]
    ) accounts))
    [ button [ onClick NewAccountMsg ] [ text "New account" ] ]

-- TODO: function to compute graph based on current model! Hooray!
-- TODO: delete buttons
-- TODO: function to set + button displays based on the accounts and income events that require names
        -- hover text could tell folks they need to input names

showEvents events =
  Array.map (\ie -> li [] [text ("name: " ++ ie.name ++ " change: " ++ (toString ie.change))]) events

showModel : Model -> Html a
showModel model =
  ol [] (Array.toList (Array.map (\account -> li [] [(text ("name: " ++ account.name  ++ " val: " ++ (toString account.initialValue))),
                                                          ol [] (Array.toList (showEvents account.incomeEvents))])
                                 model))

view : Model -> Html Msg
view model =
  div [] [div [] (buildAccountInputs model), div [] [showModel model]]
