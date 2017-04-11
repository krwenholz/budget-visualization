port module BudgetVisualization exposing (..)

import Account
import Array exposing (Array, map, indexedMap, push, set, get)
import BudgetMath
import DataStructureHelp exposing (removeFromArray)
import Html exposing (Html, text, div, input, ul, li, button)
import Html.Attributes exposing (placeholder, type_, id, width, height, class)
import Html.Events exposing (onInput, onClick)
import Maybe exposing (withDefault)
import Navigation exposing (Location)
import RouteUrl exposing (App, UrlChange, RouteUrlProgram)
import RouteUrl.Builder exposing (Builder, builder, path, appendToPath, toUrlChange, insertQuery)


-- TODO: should take input from URL (grab it from Javascript) and make it
-- our model


main =
    Html.programWithFlags
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


type alias Flags =
    Maybe Model


init : Flags -> ( Model, Cmd Msg )
init flags =
    case flags of
        Just model ->
            ( model, Cmd.none )

        Nothing ->
            ( Array.fromList [ (Account.init) ], Cmd.none )



-- TODO: Should export the model and the extrapolation


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


view : Model -> Html Msg
view accounts =
    div
        []
        [ accountsList accounts
        , button [ onClick NewAccount ] [ text "+" ]
        ]
