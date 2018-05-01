port module Main exposing (..)

import Html exposing (Html, text, div, label, input, p, section, span, i, a, button, nav)
import Html.Attributes exposing (src, class, type_, value, classList)
import Html.Events exposing (onClick, onInput, on)
import Random
import Task
import Json.Decode
import Json.Encode
import List.Extra
import Item


---- MODEL ----


type NotificationType
    = Error
    | Success


type alias Flags =
    { initData : { items : List Item.Item, nextId : Int, hoursPerDay : Int, id : Int } }


type alias Notification =
    { type_ : NotificationType, message : String }


type alias Model =
    { items : List Item.Item, nextId : Int, hoursPerDay : Int, id : Int, notification : Notification }


initModel : Model
initModel =
    { items = [], nextId = 2, hoursPerDay = 8, id = 0, notification = { type_ = Success, message = "" } }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initData =
            flags.initData
    in
        ( { initModel | items = initData.items, nextId = initData.nextId, hoursPerDay = initData.hoursPerDay, id = initData.id }
        , if initData.id > 0 then
            Cmd.none
          else
            Cmd.batch
                [ Random.generate NewId (Random.int 1000 10000)
                , Task.succeed (AddItem "Setup & provision production server") |> Task.perform identity
                , Task.succeed (AddItem "Setup CI") |> Task.perform identity
                , Task.succeed (AddItem "Setup Project Skeleton") |> Task.perform identity
                ]
        )



---- UPDATE ----


type Msg
    = AddItem String
    | UpdateHoursPerDay String
    | ItemAction Int Item.Msg
    | SaveEstimate
    | NewId Int
    | UpdateNotification Notification
    | ClearNotification


modelToValue : Model -> Json.Encode.Value
modelToValue model =
    Json.Encode.object
        [ ( "items", Json.Encode.list (List.map itemToValue model.items) )
        , ( "nextId", Json.Encode.int model.nextId )
        , ( "id", Json.Encode.int model.id )
        , ( "hoursPerDay", Json.Encode.int model.hoursPerDay )
        ]


itemToValue : Item.Item -> Json.Encode.Value
itemToValue item =
    Json.Encode.object
        [ ( "description", Json.Encode.string item.description )
        , ( "hours", Json.Encode.int item.hours )
        , ( "id", Json.Encode.int item.id )
        , ( "checked", Json.Encode.bool item.checked )
        ]


decodeNotification : Json.Decode.Decoder Notification
decodeNotification =
    Json.Decode.map2 Notification
        (Json.Decode.field "type" Json.Decode.string
            |> Json.Decode.andThen decodeNotificationType
        )
        (Json.Decode.field "message" Json.Decode.string)


decodeResponse : Json.Decode.Value -> Msg
decodeResponse json =
    case Json.Decode.decodeValue decodeNotification json of
        Result.Ok notification ->
            UpdateNotification notification

        Result.Err _ ->
            UpdateNotification { type_ = Success, message = "" }


decodeNotificationType : String -> Json.Decode.Decoder NotificationType
decodeNotificationType ntype =
    case ntype of
        "Error" ->
            Json.Decode.succeed Error

        "Success" ->
            Json.Decode.succeed Success

        _ ->
            Json.Decode.fail ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewId id ->
            ( { model | id = id }, Cmd.none )

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

        SaveEstimate ->
            ( model, saveEstimate (modelToValue model) )

        UpdateNotification notification ->
            ( { model | notification = notification }, Cmd.none )

        ClearNotification ->
            let
                notification =
                    model.notification

                updatedNotification =
                    { notification | message = "" }
            in
                ( { model | notification = updatedNotification }, Cmd.none )

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
                    [ (if (model.notification.message /= "") then
                        div [ classList [ ( "notification", True ), ( "is-danger", model.notification.type_ == Error ), ( "is-success", model.notification.type_ == Success ) ] ]
                            [ button [ class "delete", onClick ClearNotification ] []
                            , text model.notification.message
                            ]
                       else
                        div [] []
                      )
                    , nav [ class "level" ]
                        [ div [ class "level-left" ]
                            [ span [ class "panel-icon icon-font" ] [ i [ class "fas fa-stopwatch" ] [] ]
                            , span [ class "panel-text" ] [ text "Working hours/day" ]
                            , span [ class "panel-text column is-2" ] [ input [ type_ "number", class "input", toString model.hoursPerDay |> value, onInput UpdateHoursPerDay ] [] ]
                            ]
                        , div [ class "level-right" ]
                            [ div [ class "level-item" ] [ button [ class "button is-primary", onClick (AddItem "") ] [ text "Add Item" ] ]
                            , div [ class "level-item" ] [ button [ class "button is-success", onClick (SaveEstimate) ] [ text "Save Estimate" ] ]
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


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveNotification decodeResponse


port saveEstimate : Json.Encode.Value -> Cmd msg


port receiveNotification : (Json.Decode.Value -> msg) -> Sub msg


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
