module Item exposing (..)

import Html exposing (Html, text, div, label, input, p, section, span, i, a, button)
import Html.Attributes exposing (src, class, type_, value, title)
import Html.Events exposing (onClick, onInput, on)


type alias Item =
    { description : String
    , hours : Int
    , id : Int
    , checked : Bool
    }


initItem : Item
initItem =
    { checked = False, description = "", id = 1, hours = 0 }


type Msg
    = UpdateHours String
    | UpdateDescription String
    | RemoveItem Int


update : Msg -> Item -> ( Item, Cmd Msg )
update msg item =
    case msg of
        UpdateHours hours ->
            let
                updatedHours =
                    (String.toInt hours |> Result.toMaybe |> Maybe.withDefault 0)
            in
                ( { item | hours = updatedHours }, Cmd.none )

        UpdateDescription description ->
            ( { item | description = description }, Cmd.none )

        RemoveItem id ->
            ( item, Cmd.none )


view : Item -> Html Msg
view item =
    div [ class "panel-block" ]
        [ div [ class "columns" ]
            [ div [ class "column is-7" ]
                [ input [ type_ "text", class "input", value item.description, onInput UpdateDescription ] []
                ]
            , div [ class "column is-5" ]
                [ input [ type_ "number", class "input hours-input", toString item.hours |> value, onInput UpdateHours ] []
                , span [ class "panel-icon icon-font remove-icon", title "Remove this Item", onClick (RemoveItem item.id) ]
                    [ i [ class "fas fa-minus-circle" ] []
                    ]
                ]
            ]
        ]
