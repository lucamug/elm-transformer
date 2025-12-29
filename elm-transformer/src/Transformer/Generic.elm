module Transformer.Generic exposing
    ( Attr(..)
    , Generic(..)
    , Node(..)
    , classPrefix
    , column
    , el
    , inputButton
    , inputCheckboxLabelRight
    , inputRadioLabelRight
    , inputTextLabelRight
    , row
    , text
    , viewElem
    , viewHtml
    )

import Element
import Element.Input as Input
import Html
import Html.Attributes as HA
import Html.Events as HE


text : Generic -> String -> Node msg
text generic =
    case generic of
        GElem ->
            NodeElem << Element.text

        GHtml ->
            NodeHtml << Html.text


inputRadioLabelRight :
    Generic
    -> List (Attr msg)
    -> { label : String, onChange : String -> msg, options : List String, selected : Maybe String }
    -> Node msg
inputRadioLabelRight generic attrs args =
    case generic of
        GElem ->
            NodeElem <|
                Input.radio (attributesElmUi attrs)
                    { label = Input.labelRight [] <| Element.text args.label
                    , onChange = args.onChange
                    , options = List.map (\name -> Input.option name (Element.text name)) args.options
                    , selected = args.selected
                    }

        GHtml ->
            NodeHtml <|
                Html.div [ class "column" ] <|
                    List.map
                        (\s ->
                            Html.label []
                                [ Html.input
                                    [ HA.type_ "radio"
                                    , HA.value s
                                    , HA.checked (args.selected == Just s)
                                    , HE.onClick (args.onChange s)
                                    ]
                                    []
                                , Html.text s
                                ]
                        )
                        args.options


inputCheckboxLabelRight :
    Generic
    -> List (Attr msg)
    -> { checked : Bool, icon : Bool -> Element.Element msg, label : String, onChange : Bool -> msg }
    -> Node msg
inputCheckboxLabelRight generic attrs args =
    case generic of
        GElem ->
            NodeElem <|
                Input.checkbox (attributesElmUi attrs)
                    { checked = args.checked
                    , icon = args.icon
                    , label = Input.labelRight [] <| Element.text args.label
                    , onChange = args.onChange
                    }

        GHtml ->
            NodeHtml <|
                Html.label []
                    [ Html.input
                        (attributesHtml attrs
                            ++ [ HA.type_ "checkbox"
                               , HA.checked args.checked
                               , HE.onClick (args.onChange (not args.checked))
                               ]
                        )
                        []
                    , Html.text args.label
                    ]


inputTextLabelRight :
    Generic
    -> List (Attr msg)
    -> { label : String, onChange : String -> msg, placeholder : Maybe (Input.Placeholder msg), text : String }
    -> Node msg
inputTextLabelRight generic attrs args =
    case generic of
        GElem ->
            NodeElem <|
                Input.text (attributesElmUi attrs)
                    { label = Input.labelRight [] <| Element.text args.label
                    , onChange = args.onChange
                    , placeholder = args.placeholder
                    , text = args.text
                    }

        GHtml ->
            NodeHtml <|
                Html.label []
                    [ Html.input
                        (attributesHtml attrs
                            ++ [ HA.type_ "text"
                               , HA.value args.text
                               , HE.onInput args.onChange
                               ]
                        )
                        []
                    , Html.text args.label
                    ]


inputButton :
    Generic
    -> List (Attr msg)
    -> { label : String, onPress : Maybe msg }
    -> Node msg
inputButton generic attrs args =
    case generic of
        GElem ->
            NodeElem <|
                Input.button (attributesElmUi attrs)
                    { label = Element.text args.label
                    , onPress = args.onPress
                    }

        GHtml ->
            NodeHtml <|
                Html.button
                    (attributesHtml attrs
                        ++ (case args.onPress of
                                Just onPress ->
                                    [ HE.onClick onPress ]

                                Nothing ->
                                    []
                           )
                    )
                    [ Html.text args.label ]


el : Generic -> List (Attr msg) -> Node msg -> Node msg
el generic attrs child =
    case generic of
        GElem ->
            NodeElem <|
                Element.el (attributesElmUi attrs)
                    (elementElement [ child ] |> List.head |> Maybe.withDefault Element.none)

        GHtml ->
            NodeHtml <|
                Html.div (attributesHtml attrs)
                    (elementHtml [ child ])


column : Generic -> List (Attr msg) -> List (Node msg) -> Node msg
column generic attrs children =
    case generic of
        GElem ->
            NodeElem <|
                Element.column (attributesElmUi attrs)
                    (elementElement children)

        GHtml ->
            NodeHtml <|
                Html.div (attributesHtml attrs ++ [ class "column" ])
                    (elementHtml children)


row : Generic -> List (Attr msg) -> List (Node msg) -> Node msg
row generic attrs children =
    case generic of
        GElem ->
            NodeElem <| Element.row (attributesElmUi attrs) (elementElement children)

        GHtml ->
            NodeHtml <|
                Html.div (attributesHtml attrs ++ [ class "row" ])
                    (elementHtml children)



-- HELPERS


class : String -> Html.Attribute msg
class name =
    HA.class (classPrefix ++ name)


classPrefix : String
classPrefix =
    "elm-transformer-"


type Generic
    = GElem
    | GHtml


type Node msg
    = NodeElem (Element.Element msg)
    | NodeHtml (Html.Html msg)


type Attr msg
    = AttrElem (Element.Attribute msg)
    | AttrHtml (Html.Attribute msg)


elementElement : List (Node msg) -> List (Element.Element msg)
elementElement children =
    children
        |> List.filterMap
            (\a ->
                case a of
                    NodeElem attr ->
                        Just attr

                    NodeHtml _ ->
                        Nothing
            )


elementHtml : List (Node msg) -> List (Html.Html msg)
elementHtml children =
    children
        |> List.filterMap
            (\a ->
                case a of
                    NodeElem _ ->
                        Nothing

                    NodeHtml attr ->
                        Just attr
            )


attributesHtml : List (Attr msg) -> List (Html.Attribute msg)
attributesHtml attrs =
    attrs
        |> List.filterMap
            (\a ->
                case a of
                    AttrElem _ ->
                        Nothing

                    AttrHtml attr ->
                        Just attr
            )


attributesElmUi : List (Attr msg) -> List (Element.Attribute msg)
attributesElmUi attrs =
    attrs
        |> List.filterMap
            (\a ->
                case a of
                    AttrElem attr ->
                        Just attr

                    AttrHtml _ ->
                        Nothing
            )


viewElem : (Generic -> Node msg) -> Element.Element msg
viewElem view_ =
    case view_ GElem of
        NodeElem ok ->
            ok

        NodeHtml _ ->
            Element.text ""


viewHtml : (Generic -> Node msg) -> Html.Html msg
viewHtml view_ =
    case view_ GHtml of
        NodeElem _ ->
            Html.text ""

        NodeHtml ok ->
            ok
