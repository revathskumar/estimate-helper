module Main exposing (..)

import Html exposing (Html, text, div, label, input, p, section, span, i, a, button, nav)
import Html.Attributes exposing (src, class, type_, value)
import Html.Events exposing (onClick, onInput, on)
import Task
import List.Extra
import Item


---- MODEL ----


type alias Model =
    { items : List Item.Item, nextId : Int, hoursPerDay : Int }


init : ( Model, Cmd Msg )
init =
    ( { items = [], nextId = 2, hoursPerDay = 8 }
    , Cmd.batch
        [ Task.succeed (AddItem "Setup & provision production server") |> Task.perform identity
        , Task.succeed (AddItem "Setup CI") |> Task.perform identity
        , Task.succeed (AddItem "Setup Project Skeleton") |> Task.perform identity
        ]
    )



---- UPDATE ----


type Msg
    = AddItem String
    | UpdateHoursPerDay String
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

        UpdateHoursPerDay hours ->
            let
                hoursPerDay =
                    String.toInt hours |> Result.toMaybe |> Maybe.withDefault 0
            in
                ( { model | hoursPerDay = hoursPerDay }, Cmd.none )

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
    let
        totalHours =
            ((List.map .hours model.items) |> List.foldr (+) 0)

        totalDays =
            toFloat totalHours / toFloat model.hoursPerDay
    in
        div []
            [ section [ class "section" ]
                [ div [ class "container " ]
                    [ nav [ class "level" ]
                        [ div [ class "level-left" ]
                            [ span [ class "panel-icon icon-font" ] [ i [ class "fas fa-stopwatch" ] [] ]
                            , span [ class "panel-text" ] [ text "Working hours/day" ]
                            , span [ class "panel-text column is-2" ] [ input [ type_ "number", class "input", toString model.hoursPerDay |> value, onInput UpdateHoursPerDay ] [] ]
                            ]
                        , div [ class "level-right" ]
                            [ button [ class "button is-primary", onClick (AddItem "") ] [ text "Add Item" ]
                            ]
                        ]
                    , div [ class "panel" ]
                        [ p [ class "panel-heading" ] [ text "Items" ]
                        , (if (List.length model.items) > 0 then
                            div [] (List.map (\item -> Html.map (ItemAction item.id) (Item.view item)) model.items)
                           else
                            div [ class "panel-block" ] [ text "No Items to show" ]
                          )
                        , div [ class "panel-block" ]
                            [ span [ class "panel-icon icon-font" ] [ i [ class "fas fa-clock" ] [] ]
                            , span [ class "panel-text" ] [ text "Total Hours" ]
                            , span [ class "panel-text has-text-right" ] [ totalHours |> toString |> text ]
                            ]
                        , div [ class "panel-block" ]
                            [ span [ class "panel-icon icon-font" ] [ i [ class "fas fa-calendar-alt" ] [] ]
                            , span [ class "panel-text" ] [ text "Total Days" ]
                            , span [ class "panel-text has-text-right" ] [ toString (ceiling totalDays) |> text ]
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
