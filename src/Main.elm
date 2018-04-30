module Main exposing (..)

import Html exposing (Html, text, div, label, input, p, section, span, i, a, button)
import Html.Attributes exposing (src, class, type_, value)
import Html.Events exposing (onClick, onInput, on)
import Task


---- MODEL ----


type alias Item =
    { description : String
    , hours : Int
    , id : Int
    , checked : Bool
    }


initItem : Item
initItem =
    { checked = False, description = "", id = 1, hours = 0 }


type alias Model =
    { items : List Item, nextId : Int }


init : ( Model, Cmd Msg )
init =
    ( { items = [], nextId = 2 }
    , Cmd.batch
        [ Task.succeed (AddItem "Setup CI") |> Task.perform identity
        , Task.succeed (AddItem "Setup Project") |> Task.perform identity
        ]
    )



---- UPDATE ----


type Msg
    = AddItem String
    | RemoveItem Int
    | UpdateHours Int String
    | UpdateDescription Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddItem description ->
            let
                newItem =
                    { initItem | id = model.nextId, description = description }

                nextId =
                    model.nextId + 1
            in
                ( { model | items = (List.append model.items [ newItem ]), nextId = nextId }, Cmd.none )

        RemoveItem id ->
            let
                items =
                    List.filter (\item -> item.id /= id) model.items
            in
                ( { model | items = items }, Cmd.none )

        UpdateHours id hours ->
            let
                updateHours item =
                    if (item.id == id) then
                        { item | hours = (String.toInt hours |> Result.toMaybe |> Maybe.withDefault 0) }
                    else
                        item

                items =
                    List.map updateHours model.items
            in
                ( { model | items = items }, Cmd.none )

        UpdateDescription id description ->
            let
                updateDescription item =
                    if (item.id == id) then
                        { item | description = description }
                    else
                        item

                items =
                    List.map updateDescription model.items
            in
                ( { model | items = items }, Cmd.none )



---- VIEW ----


viewItem : Item -> Html Msg
viewItem item =
    div [ class "panel-block" ]
        [ div [ class "columns" ]
            [ -- div [ class "column" ]
              -- [ label [ class "checkbox" ]
              --     [ input [ type_ "checkbox" ] []
              --     ]
              -- ]
              -- ,
              div [ class "column" ]
                [ input [ type_ "text", value item.description, onInput (UpdateDescription item.id) ] []
                ]
            , div [ class "column" ]
                [ input [ type_ "number", toString item.hours |> value, onInput (UpdateHours item.id) ] []
                ]
            , a [ class "column", onClick (RemoveItem item.id) ]
                [ span [ class "panel-icon" ]
                    [ i [ class "fas fa-minus-circle" ] []
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "has-text-right" ] [ button [ class "button is-primary", onClick (AddItem "") ] [ text "Add Item" ] ]
                , div [ class "panel" ]
                    [ p [ class "panel-heading" ] [ text "Items" ]
                    , div []
                        (if (List.length model.items) > 0 then
                            (List.map viewItem model.items)
                         else
                            [ text "No Items to show" ]
                        )
                    , div [ class "panel-block" ]
                        [ div [ class "columns has-text-right" ]
                            [ div [ class "column" ] [ text "Total Hours" ]
                            , div [ class "column" ] [ ((List.map .hours model.items) |> List.foldr (+) 0) |> toString |> text ]
                            ]
                        ]
                    ]
                ]
            ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
