module Main exposing (..)

import Html exposing (Html, text, div, label, input, p, section, span, i, a, button)
import Html.Attributes exposing (src, class, type_, value)
import Html.Events exposing (onClick, onInput, on)
import Task
import List.Extra
import Item


---- MODEL ----


type alias Model =
    { items : List Item.Item, nextId : Int }


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
    | ItemAction Int Item.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddItem description ->
            let
                initItem =
                    Item.initItem

                newItem =
                    { initItem | id = model.nextId, description = description }

                nextId =
                    model.nextId + 1
            in
                ( { model | items = (List.append model.items [ newItem ]), nextId = nextId }, Cmd.none )

        ItemAction id childAction ->
            case childAction of
                Item.RemoveItem id ->
                    let
                        items =
                            List.filter (\item -> item.id /= id) model.items
                    in
                        ( { model | items = items }, Cmd.none )

                _ ->
                    let
                        index =
                            case (List.Extra.findIndex (\item -> item.id == id) model.items) of
                                Just index ->
                                    index

                                Nothing ->
                                    -1

                        selectedItem =
                            case (List.Extra.getAt index model.items) of
                                Just item ->
                                    item

                                Nothing ->
                                    Item.initItem

                        ( updatedItem, cmdMsg ) =
                            Item.update childAction selectedItem

                        items =
                            List.Extra.setAt index updatedItem model.items
                    in
                        ( { model | items = items }, Cmd.map (ItemAction id) cmdMsg )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ section [ class "section" ]
            [ div [ class "container " ]
                [ div [ class "has-text-right" ] [ button [ class "button is-primary", onClick (AddItem "") ] [ text "Add Item" ] ]
                , div [ class "panel" ]
                    [ p [ class "panel-heading" ] [ text "Items" ]
                    , div []
                        (if (List.length model.items) > 0 then
                            (List.map (\item -> Html.map (ItemAction item.id) (Item.view item)) model.items)
                         else
                            [ text "No Items to show" ]
                        )
                    , div [ class "panel-block" ]
                        [ div [ class "columns" ]
                            [ div [ class "column is-four-fifths" ] [ text "Total Hours" ]
                            , div [ class "column is-one-fifths" ] [ ((List.map .hours model.items) |> List.foldr (+) 0) |> toString |> text ]
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
