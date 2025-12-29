module Renderer exposing (renderer)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Attributes
import Markdown.Block as Block
import Markdown.Html
import Markdown.Renderer
import SyntaxHighlight


viewCode : String -> Html.Html msg
viewCode elmCode =
    SyntaxHighlight.elm elmCode
        |> Result.map (SyntaxHighlight.toBlockHtml (Just 10))
        |> Result.withDefault
            (Html.pre []
                [ Html.code [] [ Html.text elmCode ] ]
            )


renderer : Markdown.Renderer.Renderer (Element msg)
renderer =
    { blockQuote =
        \children ->
            paragraph
                [ Border.widthEach { bottom = 0, left = 10, right = 0, top = 0 }
                , padding 10
                , Border.color (rgb255 145 145 145)
                , Background.color (rgb255 245 245 245)
                ]
                children
    , codeBlock = codeBlock
    , codeSpan = code
    , emphasis = \content -> paragraph [ Font.italic ] content
    , hardLineBreak = Html.br [] [] |> html
    , heading = heading
    , html = Markdown.Html.oneOf []
    , image = \image_ -> image [ width fill ] { description = image_.alt, src = image_.src }
    , link =
        \{ destination } body ->
            newTabLink []
                { label =
                    paragraph
                        [ Font.color (rgb255 0 0 255)
                        , htmlAttribute (Html.Attributes.style "overflow-wrap" "break-word")
                        , htmlAttribute (Html.Attributes.style "word-break" "break-word")
                        ]
                        body
                , url = destination
                }
    , orderedList =
        \startingIndex items ->
            column [ spacing 15 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            paragraph [ spacing 5 ]
                                [ paragraph [ alignTop ]
                                    (text (String.fromInt (index + startingIndex) ++ " ") :: itemBlocks)
                                ]
                        )
                )
    , paragraph = paragraph [ spacing 15 ]
    , strikethrough = \content -> paragraph [ Font.strike ] content
    , strong = \content -> paragraph [ Font.bold ] content
    , table = column []
    , tableBody = column []
    , tableCell =
        \_ children ->
            paragraph tableBorder
                children
    , tableHeader =
        column
            [ Font.bold
            , width fill
            , Font.center
            ]
    , tableHeaderCell =
        \_ children ->
            paragraph tableBorder
                children
    , tableRow = row [ height fill, width fill ]
    , text = \value -> paragraph [] [ text value ]
    , thematicBreak = none
    , unorderedList =
        \items ->
            column [ paddingXY 10 0, spacing 8 ]
                (items
                    |> List.map
                        (\(Block.ListItem task children) ->
                            paragraph []
                                [ row [ alignTop ]
                                    ((case task of
                                        Block.CompletedTask ->
                                            Input.defaultCheckbox True

                                        Block.IncompleteTask ->
                                            Input.defaultCheckbox False

                                        Block.NoTask ->
                                            text "•"
                                     )
                                        :: text " "
                                        :: children
                                    )
                                ]
                        )
                )
    }


tableBorder : List (Attr () msg)
tableBorder =
    [ Border.color (rgb255 223 226 229)
    , Border.width 1
    , Border.solid
    , paddingXY 6 13
    , height fill
    ]


rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower


heading : { children : List (Element msg), level : Block.HeadingLevel, rawText : String } -> Element msg
heading { children, level, rawText } =
    paragraph
        [ Font.size
            (case level of
                Block.H1 ->
                    36

                Block.H2 ->
                    24

                _ ->
                    20
            )
        , Font.bold
        , Region.heading (Block.headingLevelToInt level)
        , htmlAttribute
            (Html.Attributes.attribute "name" (rawTextToId rawText))
        , htmlAttribute
            (Html.Attributes.id (rawTextToId rawText))
        , paddingXY 0 32
        , Font.color <| rgb255 18 147 216
        ]
        children


code : String -> Element msg
code snippet =
    el
        [ Background.color <| rgba 0 0 0 0.08
        , Border.rounded 2
        , Border.width 1
        , Border.color <| rgba 0 0.5 0.7 0.5
        , Font.color <| rgba 0 0.3 0.5 1
        , paddingXY 3 1
        , Font.family [ Font.monospace ]
        , Font.size 14
        ]
        (text snippet)


codeBlock : { body : String, language : Maybe String } -> Element msg
codeBlock details =
    el [] <| html <| viewCode details.body



-- paragraph
--     [ Background.color (rgba 0 0 0 0.03)
--     , htmlAttribute (Html.Attributes.style "white-space" "pre")
--     , htmlAttribute (Html.Attributes.style "overflow-wrap" "break-word")
--     , htmlAttribute (Html.Attributes.style "word-break" "break-word")
--     , padding 20
--     ]
--     [ text details.body ]
