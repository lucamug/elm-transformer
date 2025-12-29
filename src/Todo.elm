port module Todo exposing
    ( Entry
    , Flags
    , Mode(..)
    , Model
    , Msg
    , Visibility(..)
    , init
    , main
    , update
    , view
    )

-- Modified from: https://github.com/dwayne/elm-todos/blob/master/src/Main.elm

import Browser
import Browser.Dom
import Codec
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import Task


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { description : String, entries : List Entry, mode : Mode, uid : Int, visibility : Visibility }


type Mode
    = Edit Int String
    | Normal


type alias Entry =
    { completed : Bool, description : String, uid : Int }


type Visibility
    = Active
    | All
    | Completed


type alias Flags =
    Json.Encode.Value


init : Flags -> ( Model, Cmd msg )
init flags =
    ( case Json.Decode.decodeValue decodeModel flags of
        Ok (Just model) ->
            model

        _ ->
            { description = ""
            , entries = []
            , mode = Normal
            , uid = 0
            , visibility = All
            }
    , Cmd.none
    )



-- UPDATE


type Msg
    = BlurredEntry
    | ChangedDescription String
    | ChangedEntryDescription Int String
    | ChangedVisibility Visibility
    | CheckedEntry Int Bool
    | CheckedMarkAllCompleted Bool
    | ClickedRemoveButton Int
    | ClickedRemoveCompletedEntriesButton
    | DoubleClickedDescription Int String
    | EscapedEntry
    | FocusedEntry
    | SubmittedDescription
    | SubmittedEditedDescription


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( nextModel, cmd ) =
            update_ msg model
    in
    ( nextModel
    , Cmd.batch
        [ cmd
        , save (encodeModel nextModel)
        ]
    )


update_ : Msg -> Model -> ( Model, Cmd Msg )
update_ msg model =
    case msg of
        BlurredEntry ->
            ( { model | mode = Normal }
            , Cmd.none
            )

        ChangedDescription description ->
            ( { model | description = description }
            , Cmd.none
            )

        ChangedEntryDescription uid description ->
            ( { model | mode = Edit uid description }
            , Cmd.none
            )

        ChangedVisibility visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
            )

        CheckedEntry uid isChecked ->
            let
                updateEntry : Entry -> Entry
                updateEntry entry =
                    if uid == entry.uid then
                        { entry | completed = isChecked }

                    else
                        entry
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        CheckedMarkAllCompleted isChecked ->
            let
                updateEntry : Entry -> Entry
                updateEntry entry =
                    { entry | completed = isChecked }
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        ClickedRemoveButton uid ->
            ( { model | entries = List.filter (\entry -> entry.uid /= uid) model.entries }
            , Cmd.none
            )

        ClickedRemoveCompletedEntriesButton ->
            ( { model | entries = List.filter (not << .completed) model.entries }
            , Cmd.none
            )

        DoubleClickedDescription uid description ->
            ( { model | mode = Edit uid description }
            , focus (entryEditId uid) FocusedEntry
            )

        EscapedEntry ->
            ( { model | mode = Normal }
            , Cmd.none
            )

        FocusedEntry ->
            ( model
            , Cmd.none
            )

        SubmittedDescription ->
            let
                cleanDescription : String
                cleanDescription =
                    String.trim model.description
            in
            if String.isEmpty cleanDescription then
                ( model
                , Cmd.none
                )

            else
                ( { model
                    | description = ""
                    , entries = model.entries ++ [ { completed = False, description = cleanDescription, uid = model.uid } ]
                    , uid = model.uid + 1
                  }
                , Cmd.none
                )

        SubmittedEditedDescription ->
            case model.mode of
                Edit uid description ->
                    let
                        cleanDescription : String
                        cleanDescription =
                            String.trim description
                    in
                    if String.isEmpty cleanDescription then
                        ( { model
                            | entries = List.filter (\entry -> entry.uid /= uid) model.entries
                            , mode = Normal
                          }
                        , Cmd.none
                        )

                    else
                        let
                            updateEntry : Entry -> Entry
                            updateEntry entry =
                                if uid == entry.uid then
                                    { entry | description = cleanDescription }

                                else
                                    entry
                        in
                        ( { model
                            | entries = List.map updateEntry model.entries
                            , mode = Normal
                          }
                        , Cmd.none
                        )

                Normal ->
                    ( model
                    , Cmd.none
                    )



-- PORTS


port save : Json.Encode.Value -> Cmd msg



-- CODECS


codecModel : Codec.Codec Model
codecModel =
    Codec.object
        (\uid entries visibility ->
            { description = ""
            , entries = entries
            , mode = Normal
            , uid = uid
            , visibility = visibility
            }
        )
        |> Codec.field "uid" .uid Codec.int
        |> Codec.field "entries" .entries (Codec.list codecEntry)
        |> Codec.field "visibility" .visibility codecVisibility
        |> Codec.buildObject


codecVisibility : Codec.Codec Visibility
codecVisibility =
    Codec.custom
        (\v1 v2 v3 value ->
            case value of
                Active ->
                    v2

                All ->
                    v1

                Completed ->
                    v3
        )
        |> Codec.variant0 "All" All
        |> Codec.variant0 "Active" Active
        |> Codec.variant0 "Completed" Completed
        |> Codec.buildCustom


codecEntry : Codec.Codec Entry
codecEntry =
    Codec.object
        (\v1 v2 v3 ->
            { completed = v3
            , description = v2
            , uid = v1
            }
        )
        |> Codec.field "uid" .uid Codec.int
        |> Codec.field "description" .description Codec.string
        |> Codec.field "completed" .completed Codec.bool
        |> Codec.buildObject


encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Codec.encoder codecModel model


decodeModel : Json.Decode.Decoder (Maybe Model)
decodeModel =
    Json.Decode.nullable <|
        Codec.decoder codecModel



-- VIEW


view : Model -> Html.Html Msg
view { description, entries, mode, visibility } =
    Html.div []
        [ Html.section [ Html.Attributes.class "todoapp" ] <|
            viewPrompt description
                :: viewMain mode visibility entries
        , viewFooter
        ]


viewPrompt : String -> Html.Html Msg
viewPrompt description =
    Html.header [ Html.Attributes.class "header" ]
        [ Html.h1 [] [ Html.text "todos" ]
        , Html.form [ Html.Events.onSubmit SubmittedDescription ]
            [ Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.autofocus True
                , Html.Attributes.placeholder "What needs to be done?"
                , Html.Attributes.class "new-todo"
                , Html.Attributes.value description
                , Html.Events.onInput ChangedDescription
                ]
                []
            ]
        ]


viewMain : Mode -> Visibility -> List Entry -> List (Html.Html Msg)
viewMain mode visibility entries =
    if List.isEmpty entries then
        []

    else
        [ Html.section [ Html.Attributes.class "main" ]
            [ Html.input
                [ Html.Attributes.type_ "checkbox"
                , Html.Attributes.id "toggle-all"
                , Html.Attributes.class "toggle-all"
                , Html.Attributes.checked (List.all .completed entries)
                , Html.Events.onCheck CheckedMarkAllCompleted
                ]
                []
            , Html.label [ Html.Attributes.for "toggle-all" ] [ Html.text "Mark all as completed" ]
            , Html.ul [ Html.Attributes.class "todo-list" ] <|
                List.map
                    (\entry ->
                        Html.li
                            [ Html.Attributes.classList
                                [ ( "completed", entry.completed )
                                , ( "editing", isEditing mode entry )
                                ]
                            ]
                            [ viewEntry mode entry ]
                    )
                    (keep visibility entries)
            ]
        , Html.footer [ Html.Attributes.class "footer" ] <|
            List.concat
                [ [ viewStatus entries
                  , viewVisibilityFilters visibility
                  ]
                , viewClearCompleted entries
                ]
        ]


viewEntry : Mode -> Entry -> Html.Html Msg
viewEntry mode entry =
    case mode of
        Edit uid description ->
            if uid == entry.uid then
                viewEntryEdit uid description

            else
                viewEntryNormal entry

        Normal ->
            viewEntryNormal entry


viewEntryNormal : Entry -> Html.Html Msg
viewEntryNormal { completed, description, uid } =
    Html.div [ Html.Attributes.class "view" ]
        [ Html.input
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.checked completed
            , Html.Attributes.class "toggle"
            , Html.Events.onCheck (CheckedEntry uid)
            ]
            []
        , Html.label [ Html.Events.onDoubleClick (DoubleClickedDescription uid description) ]
            [ Html.text description ]
        , Html.button
            [ Html.Attributes.type_ "button"
            , Html.Attributes.class "destroy"
            , Html.Events.onClick (ClickedRemoveButton uid)
            ]
            []
        ]


viewEntryEdit : Int -> String -> Html.Html Msg
viewEntryEdit uid description =
    Html.form [ Html.Events.onSubmit SubmittedEditedDescription ]
        [ Html.input
            [ Html.Attributes.type_ "text"
            , Html.Attributes.id (entryEditId uid)
            , Html.Attributes.value description
            , Html.Attributes.class "edit"
            , Html.Events.onInput (ChangedEntryDescription uid)
            , Html.Events.onBlur BlurredEntry
            , onEsc EscapedEntry
            ]
            []
        ]


viewStatus : List Entry -> Html.Html msg
viewStatus entries =
    let
        n : Int
        n =
            entries
                |> List.filter (not << .completed)
                |> List.length
    in
    Html.span [ Html.Attributes.class "todo-count" ]
        [ Html.strong [] [ Html.text (String.fromInt n) ]
        , Html.text <| " " ++ pluralize n "item" "items" ++ " left"
        ]


viewVisibilityFilters : Visibility -> Html.Html Msg
viewVisibilityFilters selected =
    Html.ul [ Html.Attributes.class "filters" ]
        [ Html.li [] [ viewVisibilityFilter "All" All selected ]
        , Html.li [] [ viewVisibilityFilter "Active" Active selected ]
        , Html.li [] [ viewVisibilityFilter "Completed" Completed selected ]
        ]


viewVisibilityFilter : String -> Visibility -> Visibility -> Html.Html Msg
viewVisibilityFilter name current selected =
    Html.a
        [ Html.Attributes.href "#"
        , Html.Events.onClick (ChangedVisibility current)
        , Html.Attributes.classList [ ( "selected", current == selected ) ]
        ]
        [ Html.text name ]


viewClearCompleted : List Entry -> List (Html.Html Msg)
viewClearCompleted entries =
    let
        completedEntries : List { completed : Bool, description : String, uid : Int }
        completedEntries =
            List.filter .completed entries

        numCompletedEntries : Int
        numCompletedEntries =
            List.length completedEntries
    in
    if numCompletedEntries == 0 then
        []

    else
        [ Html.button
            [ Html.Attributes.type_ "button"
            , Html.Attributes.class "clear-completed"
            , Html.Events.onClick ClickedRemoveCompletedEntriesButton
            ]
            [ Html.text <| "Clear completed (" ++ String.fromInt numCompletedEntries ++ ")" ]
        ]


viewFooter : Html.Html msg
viewFooter =
    Html.footer [ Html.Attributes.class "info" ]
        [ Html.p [] [ Html.text "Double-click to edit a todo" ]
        ]



-- HELPERS


entryEditId : Int -> String
entryEditId uid =
    "entry-edit-" ++ String.fromInt uid


isEditing : Mode -> Entry -> Bool
isEditing mode entry =
    case mode of
        Edit uid _ ->
            uid == entry.uid

        Normal ->
            False


keep : Visibility -> List Entry -> List Entry
keep visibility entries =
    case visibility of
        Active ->
            List.filter (not << .completed) entries

        All ->
            entries

        Completed ->
            List.filter .completed entries


pluralize : Int -> String -> String -> String
pluralize n singular plural =
    if n == 1 then
        singular

    else
        plural


focus : String -> msg -> Cmd msg
focus id msg =
    Browser.Dom.focus id
        |> Task.attempt (\_ -> msg)


onEsc : msg -> Html.Attribute msg
onEsc msg =
    let
        decoder : Json.Decode.Decoder msg
        decoder =
            Html.Events.keyCode
                |> Json.Decode.andThen
                    (\n ->
                        case n of
                            27 ->
                                Json.Decode.succeed msg

                            _ ->
                                Json.Decode.fail "ignored"
                    )
    in
    Html.Events.on "keydown" decoder
