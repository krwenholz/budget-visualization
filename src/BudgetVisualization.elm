port module BudgetVisualization exposing (..)

import Account
import Array exposing (Array, map, indexedMap, push, set, get)
import BudgetMath
import DataStructureHelp exposing (removeFromArray)
import Html exposing (Html, text, div, input, ul, li, button)
import Html.Attributes exposing (placeholder, type_, id, width, height, class, attribute)
import Html.Events exposing (onInput, onClick)
import Maybe exposing (withDefault)
import Navigation exposing (Location)
import RouteUrl exposing (App, UrlChange, RouteUrlProgram)
import RouteUrl.Builder exposing (Builder, builder, path, appendToPath, toUrlChange, insertQuery)


-- TODO: should take input from URL (grab it from Javascript) and make it
-- our model


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias UpdateAcountMsg =
    { accountNum : Int
    , update : Account.Msg
    }


type Msg
    = UpdateAccount UpdateAcountMsg
    | DeleteAccount Int
    | NewAccount


type alias Model =
    Array Account.Model


init : ( Model, Cmd Msg )
init =
    ( Array.fromList [ (Account.init) ], Cmd.none )


port output : ( BudgetMath.Model, Model ) -> Cmd msg


updateAccounts : Msg -> Model -> Model
updateAccounts action accounts =
    case action of
        UpdateAccount { accountNum, update } ->
            let
                account =
                    get accountNum accounts |> withDefault Account.init
            in
                set accountNum (Account.update update account) accounts

        DeleteAccount accountNum ->
            removeFromArray accountNum accounts

        NewAccount ->
            push Account.init accounts


update : Msg -> Model -> ( Model, Cmd msg )
update action accounts =
    let
        updatedModel =
            updateAccounts action accounts
    in
        ( updatedModel, output ( BudgetMath.asData updatedModel, updatedModel ) )


accountListItem : Int -> Account.Model -> Html Msg
accountListItem index account =
    li []
        [ div []
            [ button
                [ onClick (DeleteAccount index) ]
                [ text "-" ]
            , Html.map
                (\update ->
                    UpdateAccount
                        { accountNum = index
                        , update = update
                        }
                )
              <|
                Account.view account
            ]
        ]


accountsList : Array Account.Model -> Html Msg
accountsList accounts =
    ul [ class "accountList" ]
        (Array.toList <|
            indexedMap (\index account -> accountListItem index account)
                accounts
        )


copyModelJavascript : Model -> String
copyModelJavascript model =
    "window.prompt(\"Copy to clipboard: Ctrl+C, Enter\", '" ++ (toString model) ++ "');"


view : Model -> Html Msg
view accounts =
    div
        []
        [ accountsList accounts
        , button [ onClick NewAccount ] [ text "+" ]
        , button [ attribute "onclick" (copyModelJavascript accounts) ] [ text "Save for later" ]
        ]
