port module BudgetVisualization exposing (..)

import Account
import Array exposing (Array, map, indexedMap, push, set, get)
import BudgetMath
import DataStructureHelp exposing (removeFromArray)
import Html exposing (Html, text, div, input, ul, li, button)
import Html.Attributes exposing (placeholder, type_, id, class, attribute)
import Html.Events exposing (onInput, onClick)
import Json.Decode as Decode
import Json.Decode.Pipeline as DPipeline
import Json.Encode as Encode
import Json.Encode as Encode
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
    | RestoreAccounts String


type alias Model =
    Array Account.Model


init : ( Model, Cmd Msg )
init =
    ( Array.fromList [ (Account.init) ], Cmd.none )


encode : Model -> Encode.Value
encode model =
    Encode.array <| Array.map Account.encode model


decode : Decode.Decoder Model
decode =
    (Decode.array Account.decode)


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

        RestoreAccounts jsonAccounts ->
            case Decode.decodeString decode jsonAccounts of
                Err _ ->
                    Array.fromList [ (Account.init) ]

                --accounts
                Ok newModel ->
                    newModel


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
    "window.prompt(\"Copy to clipboard: Ctrl+C, Enter\", '" ++ (Encode.encode 0 <| encode model) ++ "');"


view : Model -> Html Msg
view accounts =
    div
        []
        [ accountsList accounts
        , button [ onClick NewAccount ] [ text "+" ]
        , button [ attribute "onclick" (copyModelJavascript accounts), class "saveButton" ] [ text "Save for later" ]
        , input
            [ type_ "text"
            , placeholder "Restore your saved session by pasting it here."
            , onInput RestoreAccounts
            , class "accountInput"
            ]
            []
        ]
