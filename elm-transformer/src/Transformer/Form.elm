module Transformer.Form exposing
    ( viewFormElement
    , viewFormElementAsHtml
    , viewFormHtml
    )

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Hex
import Hex.Convert
import Html
import Html.Attributes
import Transformer.Generic as TG
import Transformer.Internal as TI



--
-- VIEW
--


rootElementClass : String
rootElementClass =
    TG.classPrefix ++ "form"


viewFormHtml : TI.Value -> Html.Html TI.Msg
viewFormHtml value =
    Html.div [ Html.Attributes.class rootElementClass ]
        [ TG.viewHtml (viewValue [] value) ]


viewFormElement : TI.Value -> Element.Element TI.Msg
viewFormElement value =
    TG.viewElem <| viewValue [] value


viewFormElementAsHtml : TI.Value -> Html.Html TI.Msg
viewFormElementAsHtml value =
    layout [ htmlAttribute <| Html.Attributes.class rootElementClass ] <|
        TG.viewElem (viewValue [] value)


viewValue : TI.Ancestors -> TI.Value -> TG.Generic -> TG.Node TI.Msg
viewValue ancestors value generic =
    case value of
        TI.VBool v ->
            TG.inputCheckboxLabelRight generic
                []
                { checked = v
                , icon = Input.defaultCheckbox
                , label = "Bool"
                , onChange = TI.ChangeBool ancestors
                }

        TI.VBytes v ->
            let
                asHexString : String
                asHexString =
                    TI.fromBytesToStringHex v

                asListOfInts : List Int
                asListOfInts =
                    TI.fromStringHexToListOfInts asHexString

                asUtf8String : String
                asUtf8String =
                    TI.fromListOfIntsToStringUtf8 asListOfInts
            in
            TG.row generic
                (convertElemAttrs [ spacing 16 ] ++ convertHtmlAttrs [])
                [ TG.inputTextLabelRight generic
                    (convertElemAttrs attrsInputText)
                    { label = "Bytes as Hex"
                    , onChange = \bytesAsHex -> TI.ChangeBytes ancestors (TI.fromStringHexToBytesWithDefault v bytesAsHex)
                    , placeholder = Nothing
                    , text = asHexString
                    }
                , TG.inputTextLabelRight generic
                    (convertElemAttrs attrsInputText)
                    { label = "as UTF8"
                    , onChange = \bytesAsUtf8 -> TI.ChangeBytes ancestors (TI.fromStringUtf8ToBytes bytesAsUtf8)
                    , placeholder = Nothing
                    , text = asUtf8String
                    }
                , TG.inputTextLabelRight generic
                    (convertElemAttrs attrsInputText)
                    { label = "as integers"
                    , onChange =
                        \bytesAsUtf8 ->
                            bytesAsUtf8
                                |> String.split ","
                                |> isOnlyInts
                                |> Maybe.map (List.map (\s -> String.padLeft 2 '0' (Hex.toString s)))
                                |> Maybe.map String.concat
                                -- Is "toUpper" needed?
                                |> Maybe.map String.toUpper
                                |> Maybe.andThen Hex.Convert.toBytes
                                |> Maybe.withDefault v
                                |> TI.ChangeBytes ancestors
                    , placeholder = Nothing
                    , text = String.join "," (List.map String.fromInt asListOfInts)
                    }
                ]

        TI.VChar v ->
            TG.row generic
                (convertElemAttrs [ spacing 16 ])
                [ TG.inputTextLabelRight generic
                    (convertElemAttrs (attrsInputText ++ [ Font.alignRight, width <| px 35 ]))
                    { label = "Char"
                    , onChange = TI.ChangeChar ancestors
                    , placeholder = Nothing
                    , text = String.fromChar v
                    }
                , TG.inputTextLabelRight generic
                    (convertElemAttrs (attrsInputText ++ [ Font.alignRight, width <| px 80 ]))
                    { label = "as code"
                    , onChange =
                        \codeAsString ->
                            TI.ChangeChar ancestors
                                (codeAsString |> String.toInt |> Maybe.map Char.fromCode |> Maybe.map String.fromChar |> Maybe.withDefault (String.fromChar v))
                    , placeholder = Nothing
                    , text = String.fromInt (Char.toCode v)
                    }
                ]

        TI.VCustom ( customTypeName, customTypeInstances ) ( string_, list_ ) ->
            TG.column generic
                (convertElemAttrs attrsColumn)
                [ TG.el generic (convertElemAttrs (attrsType ++ [ Font.color <| rgb 0.8 0 0.7, Background.color <| rgba 0.8 0.5 0.8 0.2 ])) <|
                    TG.text generic (Maybe.withDefault "Custom Type" customTypeName)
                , TG.row generic
                    (convertElemAttrs [ spacing 16 ])
                    (TG.inputRadioLabelRight generic
                        (convertElemAttrs [ spacing 16 ])
                        { label = ""
                        , onChange = TI.ChangeCustomType (( value, "" ) :: ancestors)
                        , options = List.map (\( name, _ ) -> name) customTypeInstances
                        , selected = Just string_
                        }
                        :: List.indexedMap
                            (\index i ->
                                viewValue
                                    (( value, String.fromInt index ) :: ancestors)
                                    i
                                    generic
                            )
                            list_
                    )
                ]

        TI.VFloat v ->
            TG.inputTextLabelRight generic
                (convertElemAttrs (attrsInputText ++ [ Font.alignRight ]))
                { label = "Float"
                , onChange = TI.ChangeFloat ancestors
                , placeholder = Nothing
                , text = String.fromFloat v
                }

        TI.VInt v ->
            TG.inputTextLabelRight generic
                (convertElemAttrs (attrsInputText ++ [ Font.alignRight ]))
                { label = "Int"
                , onChange = TI.ChangeInt ancestors
                , placeholder = Nothing
                , text = String.fromInt v
                }

        TI.VList listDefinition list_ ->
            TG.column generic
                (convertElemAttrs attrsColumn)
                ((TG.el generic (convertElemAttrs attrsType) <| TG.text generic listDefinition.name)
                    :: viewListMenuWhenEmpty generic list_ value ancestors
                    ++ viewListMenu generic listDefinition.isDictOrSet list_ value ancestors
                    ++ (list_
                            |> List.indexedMap
                                (\index v ->
                                    TG.row generic
                                        (convertElemAttrs [ spacing 16 ])
                                        [ viewRow generic value (String.fromInt (index + 1)) ancestors v
                                        , actionButtonHelper generic value ancestors (String.fromInt index) "Delete" TI.ItemDelete
                                        ]
                                )
                       )
                    ++ viewListMenu generic listDefinition.isDictOrSet list_ value ancestors
                )

        TI.VRecord name dict_ ->
            TG.column generic
                (convertElemAttrs attrsColumn)
                ((TG.el generic (convertElemAttrs attrsType) <| TG.text generic (Maybe.withDefault "Record" name))
                    :: (dict_ |> Dict.toList |> List.map (\( k, v ) -> viewRow generic value k ancestors v))
                )

        TI.VString v ->
            TG.inputTextLabelRight generic
                (convertElemAttrs attrsInputText)
                { label = "String"
                , onChange = TI.ChangeString ancestors
                , placeholder = Nothing
                , text = v
                }

        TI.VTriple ( v1, v2, v3 ) ->
            TG.row generic
                (convertElemAttrs [ spacing 4 ])
                [ TG.el generic (convertElemAttrs [ Font.size 22 ]) <| TG.text generic "("
                , viewValue (( value, "0" ) :: ancestors) v1 generic
                , TG.el generic (convertElemAttrs [ Font.size 22 ]) <| TG.text generic ", "
                , viewValue (( value, "1" ) :: ancestors) v2 generic
                , TG.el generic (convertElemAttrs [ Font.size 22 ]) <| TG.text generic ", "
                , viewValue (( value, "2" ) :: ancestors) v3 generic
                , TG.el generic (convertElemAttrs [ Font.size 22 ]) <| TG.text generic ")"
                ]

        TI.VTuple ( v1, v2 ) ->
            TG.row generic
                (convertElemAttrs [ spacing 4 ])
                [ TG.el generic (convertElemAttrs [ Font.size 22 ]) <| TG.text generic "("
                , viewValue (( value, "0" ) :: ancestors) v1 generic
                , TG.el generic (convertElemAttrs [ Font.size 22 ]) <| TG.text generic ", "
                , viewValue (( value, "1" ) :: ancestors) v2 generic
                , TG.el generic (convertElemAttrs [ Font.size 22 ]) <| TG.text generic ")"
                ]


viewRow : TG.Generic -> TI.Value -> String -> List ( TI.Value, String ) -> TI.Value -> TG.Node TI.Msg
viewRow generic value k ancestors v =
    TG.row generic
        (convertElemAttrs [ spacing 16 ])
        [ TG.el generic
            (convertElemAttrs
                [ alignTop
                , paddingXY 0 16
                , width <| px 100
                ]
            )
            (TG.text generic <| k ++ ":")
        , viewValue (( value, k ) :: ancestors) v generic
        ]


viewListMenuWhenEmpty : TG.Generic -> List a -> TI.Value -> TI.Ancestors -> List (TG.Node TI.Msg)
viewListMenuWhenEmpty generic list_ value ancestors =
    if List.length list_ <= 1 then
        let
            actionButton : String -> (TI.Ancestors -> msg) -> TG.Node msg
            actionButton =
                actionButtonHelper generic value ancestors ""
        in
        [ TG.row generic
            (convertElemAttrs [ spacing 16 ])
            [ actionButton "Add" (TI.ItemAdd TI.Bottom) ]
        ]

    else
        []


actionButtonHelper : TG.Generic -> TI.Value -> TI.Ancestors -> String -> String -> (TI.Ancestors -> msg) -> TG.Node msg
actionButtonHelper generic value ancestors key label msg =
    TG.inputButton generic
        (convertElemAttrs (attrsButton ++ [ alignRight ]))
        { label = label
        , onPress = Just <| msg (( value, key ) :: ancestors)
        }


isOnlyInts : List String -> Maybe (List Int)
isOnlyInts strings =
    strings
        |> List.foldr
            (\string maybeAcc ->
                case maybeAcc of
                    Just acc ->
                        case String.toInt string of
                            Just int ->
                                Just (int :: acc)

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
            )
            (Just [])


viewListMenu : TG.Generic -> Bool -> List TI.Value -> TI.Value -> TI.Ancestors -> List (TG.Node TI.Msg)
viewListMenu generic isDictOrSet list_ value ancestors =
    if List.length list_ <= 1 then
        []

    else
        let
            actionButton : String -> (TI.Ancestors -> msg) -> TG.Node msg
            actionButton =
                actionButtonHelper generic value ancestors ""
        in
        [ TG.row generic
            (convertElemAttrs [ spacing 16 ])
            (if isDictOrSet then
                [ actionButton "Add" (TI.ItemAdd TI.Bottom) ]

             else
                [ actionButton "Add ↓" (TI.ItemAdd TI.Bottom)
                , actionButton "Add ↑" (TI.ItemAdd TI.Top)
                ]
                    ++ (if TI.isListComparable list_ then
                            [ actionButton "Sort ↓" TI.SortDown
                            , actionButton "Sort ↑" TI.SortUp
                            ]

                        else
                            []
                       )
                    ++ [ actionButton "Reverse" TI.Reverse ]
            )
        ]


convertElemAttrs : List (Attribute msg) -> List (TG.Attr msg)
convertElemAttrs =
    List.map TG.AttrElem


convertHtmlAttrs : List (Html.Attribute msg) -> List (TG.Attr msg)
convertHtmlAttrs =
    List.map TG.AttrHtml



-- ATTRIBUTES


attrsColumn : List (Attribute msg)
attrsColumn =
    [ spacing 8
    , Border.width 2
    , Border.color <| rgba 0 0 0 0.2
    , Border.rounded 8
    , padding 8
    , Background.color <| rgba 0 0 0 0.05
    ]


attrsInputText : List (Attribute msg)
attrsInputText =
    [ width <| px 150
    , padding 8
    , Background.color <| rgb 1 1 1
    , Border.width 2
    , Border.rounded 4
    ]


attrsType : List (Attribute msg)
attrsType =
    [ Font.bold
    , Font.color <| rgb 0 0.5 0.8
    , padding 8
    , Background.color <| rgba 0 0.5 0.8 0.08
    , width fill
    , Border.rounded 4
    ]


attrsButton : List (Attribute msg)
attrsButton =
    [ Border.width 2
    , paddingXY 8 8
    , Border.rounded 4
    , Border.color <| rgba 0 0 0 0.2
    , Background.color <| rgba 0 0 0 0.05
    ]
