module Main exposing (main)

import Array exposing (Array)
import Browser
import Bytes exposing (Bytes)
import Bytes.Encode
import Codec
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Html.Events
import Http exposing (Error)
import Markdown.Parser
import Markdown.Renderer
import Renderer
import Set exposing (Set)
import SyntaxHighlight
import Transformer as T
import Transformer.Codec
import Url exposing (Protocol, Url)



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val01 =
    String


m01 :
    { getter : { b | val01 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val01 : b } -> { c | val01 : b })
    , t : T.Transformer Val01
    }
m01 =
    { getter = .val01
    , meta =
        { elmType = "String"
        , elmTypeDefinition = viewCode "String"
        , key = 1
        , msgMap = Msg01
        , transformerEquivalent = viewCode "T.string"
        }
    , setter = \b a -> { a | val01 = b }
    , t = T.string
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val02 =
    Int


m02 :
    { getter : { b | val02 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val02 : b } -> { c | val02 : b })
    , t : T.Transformer Val02
    }
m02 =
    { getter = .val02
    , meta =
        { elmType = "Int"
        , elmTypeDefinition = viewCode "Int"
        , key = 2
        , msgMap = Msg02
        , transformerEquivalent = viewCode "T.int"
        }
    , setter = \b a -> { a | val02 = b }
    , t = T.int
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val03 =
    Float


m03 :
    { getter : { b | val03 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val03 : b } -> { c | val03 : b })
    , t : T.Transformer Val03
    }
m03 =
    { getter = .val03
    , meta =
        { elmType = "Float"
        , elmTypeDefinition = viewCode "Float"
        , key = 3
        , msgMap = Msg03
        , transformerEquivalent = viewCode "T.float"
        }
    , setter = \b a -> { a | val03 = b }
    , t = T.float
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val04 =
    Bool


m04 :
    { getter : { b | val04 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val04 : b } -> { c | val04 : b })
    , t : T.Transformer Val04
    }
m04 =
    { getter = .val04
    , meta =
        { elmType = "Bool"
        , elmTypeDefinition = viewCode "Bool"
        , key = 4
        , msgMap = Msg04
        , transformerEquivalent = viewCode "T.bool"
        }
    , setter = \b a -> { a | val04 = b }
    , t = T.bool
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val05 =
    Char


m05 :
    { getter : { b | val05 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val05 : b } -> { c | val05 : b })
    , t : T.Transformer Val05
    }
m05 =
    { getter = .val05
    , meta =
        { elmType = "Char"
        , elmTypeDefinition = viewCode "Char"
        , key = 5
        , msgMap = Msg05
        , transformerEquivalent = viewCode "T.char"
        }
    , setter = \b a -> { a | val05 = b }
    , t = T.char
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val06 =
    ( String, Int )


m06 :
    { getter : { b | val06 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val06 : b } -> { c | val06 : b })
    , t : T.Transformer Val06
    }
m06 =
    { getter = .val06
    , meta =
        { elmType = "( String, Int )"
        , elmTypeDefinition = viewCode "( String, Int )"
        , key = 6
        , msgMap = Msg06
        , transformerEquivalent = viewCode "T.tuple ( T.string, T.int )"
        }
    , setter = \b a -> { a | val06 = b }
    , t = T.tuple ( T.string, T.int )
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val07 =
    ( String, Int, Bool )


m07 :
    { getter : { b | val07 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val07 : b } -> { c | val07 : b })
    , t : T.Transformer Val07
    }
m07 =
    { getter = .val07
    , meta =
        { elmType = "( String, Int, Bool )"
        , elmTypeDefinition = viewCode "( String, Int, Bool )"
        , key = 7
        , msgMap = Msg07
        , transformerEquivalent = viewCode "T.triple ( T.string, T.int, T.bool )"
        }
    , setter = \b a -> { a | val07 = b }
    , t = T.triple ( T.string, T.int, T.bool )
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val08 =
    List String


m08 :
    { getter : { b | val08 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val08 : b } -> { c | val08 : b })
    , t : T.Transformer Val08
    }
m08 =
    { getter = .val08
    , meta =
        { elmType = "List String"
        , elmTypeDefinition = viewCode "List String"
        , key = 8
        , msgMap = Msg08
        , transformerEquivalent = viewCode "T.list T.string"
        }
    , setter = \b a -> { a | val08 = b }
    , t = T.list T.string
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val09 =
    List Int


m09 :
    { getter : { b | val09 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val09 : b } -> { c | val09 : b })
    , t : T.Transformer Val09
    }
m09 =
    { getter = .val09
    , meta =
        { elmType = "List Int"
        , elmTypeDefinition = viewCode "List Int"
        , key = 9
        , msgMap = Msg09
        , transformerEquivalent = viewCode "T.list T.int"
        }
    , setter = \b a -> { a | val09 = b }
    , t = T.list T.int
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val10 =
    Dict Int String


m10 :
    { getter : { b | val10 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val10 : b } -> { c | val10 : b })
    , t : T.Transformer Val10
    }
m10 =
    { getter = .val10
    , meta =
        { elmType = "Dict Int String"
        , elmTypeDefinition = viewCode "Dict Int String"
        , key = 10
        , msgMap = Msg10
        , transformerEquivalent = viewCode "T.dict T.int T.string"
        }
    , setter = \b a -> { a | val10 = b }
    , t = T.dict T.int T.string
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val11 =
    Dict String String


m11 :
    { getter : { b | val11 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val11 : b } -> { c | val11 : b })
    , t : T.Transformer Val11
    }
m11 =
    { getter = .val11
    , meta =
        { elmType = "Dict String String"
        , elmTypeDefinition = viewCode "Dict String String"
        , key = 11
        , msgMap = Msg11
        , transformerEquivalent = viewCode "T.dict T.string T.string"
        }
    , setter = \b a -> { a | val11 = b }
    , t = T.dict T.string T.string
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val12 =
    Set String


m12 :
    { getter : { b | val12 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val12 : b } -> { c | val12 : b })
    , t : T.Transformer Val12
    }
m12 =
    { getter = .val12
    , meta =
        { elmType = "Set String"
        , elmTypeDefinition = viewCode "Set String"
        , key = 12
        , msgMap = Msg12
        , transformerEquivalent = viewCode "T.set T.string"
        }
    , setter = \b a -> { a | val12 = b }
    , t = T.set T.string
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val13 =
    Maybe String


m13 :
    { getter : { b | val13 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val13 : b } -> { c | val13 : b })
    , t : T.Transformer Val13
    }
m13 =
    { getter = .val13
    , meta =
        { elmType = "Maybe String"
        , elmTypeDefinition = viewCode "Maybe String"
        , key = 13
        , msgMap = Msg13
        , transformerEquivalent = viewCode "T.maybe T.string"
        }
    , setter = \b a -> { a | val13 = b }
    , t = T.maybe T.string
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val14 =
    Result String Int


m14 :
    { getter : { b | val14 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val14 : b } -> { c | val14 : b })
    , t : T.Transformer Val14
    }
m14 =
    { getter = .val14
    , meta =
        { elmType = "Result String Int"
        , elmTypeDefinition = viewCode "Result String Int"
        , key = 14
        , msgMap = Msg14
        , transformerEquivalent = viewCode "T.result T.string T.int"
        }
    , setter = \b a -> { a | val14 = b }
    , t = T.result T.string T.int
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val15 =
    Direction


type Direction
    = Down
    | Left
    | Right
    | Up


d15 : String
d15 =
    """type Direction
    = Down
    | Left
    | Right
    | Up
"""


transformerDirection : T.Transformer Direction
transformerDirection =
    let
        all : List Direction
        all =
            [ Up, Down, Left, Right ]

        encoder : Direction -> T.CustomTypeAsTuple
        encoder customType =
            case customType of
                Down ->
                    ( "Down", [] )

                Left ->
                    ( "Left", [] )

                Right ->
                    ( "Right", [] )

                Up ->
                    ( "Up", [] )

        decoder : T.CustomTypeAsTuple -> Direction
        decoder tuple =
            case tuple of
                ( "Up", _ ) ->
                    Up

                ( "Down", _ ) ->
                    Down

                ( "Left", _ ) ->
                    Left

                _ ->
                    Right
    in
    T.helperForCustomTypes
        { all = all
        , decoder = decoder
        , encoder = encoder
        , name = Just "Direction"
        }


t15 : String
t15 =
    """transformerDirection : T.Transformer Direction
transformerDirection =
    let
        all : List Direction
        all =
            [ Up, Down, Left, Right ]

        encoder : Direction -> T.CustomTypeAsTuple
        encoder customType =
            case customType of
                Down ->
                    ( "Down", [] )

                Left ->
                    ( "Left", [] )

                Right ->
                    ( "Right", [] )

                Up ->
                    ( "Up", [] )

        decoder : T.CustomTypeAsTuple -> Direction
        decoder tuple =
            case tuple of
                ( "Up", _ ) ->
                    Up

                ( "Down", _ ) ->
                    Down

                ( "Left", _ ) ->
                    Left

                _ ->
                    Right
    in
    T.helperForCustomTypes
        { all = all
        , decoder = decoder
        , encoder = encoder
        , name = Just "Direction"
        }
"""


m15 :
    { getter : { b | val15 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val15 : b } -> { c | val15 : b })
    , t : T.Transformer Val15
    }
m15 =
    { getter = .val15
    , meta =
        { elmType = "Custom type: Direction"
        , elmTypeDefinition = viewCode d15
        , key = 15
        , msgMap = Msg15
        , transformerEquivalent = viewCode t15
        }
    , setter = \b a -> { a | val15 = b }
    , t = transformerDirection
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val16 =
    Rgb


type Rgb
    = Rgb Int Int Int


d16 : String
d16 =
    """type Rgb
    = Rgb Int Int Int
"""


transformerRgb : T.Transformer Rgb
transformerRgb =
    let
        all : List Rgb
        all =
            [ Rgb 0 0 0 ]

        encoder : Rgb -> T.CustomTypeAsTuple
        encoder (Rgb p1 p2 p3) =
            ( "Rgb", [ T.encode T.int p1, T.encode T.int p2, T.encode T.int p3 ] )

        decoder : T.CustomTypeAsTuple -> Rgb
        decoder tuple =
            case tuple of
                ( "Rgb", p1 :: p2 :: p3 :: _ ) ->
                    Rgb (T.decode T.int p1) (T.decode T.int p2) (T.decode T.int p3)

                _ ->
                    Rgb 0 0 0
    in
    T.helperForCustomTypes
        { all = all
        , decoder = decoder
        , encoder = encoder
        , name = Just "Rgb"
        }


t16 : String
t16 =
    """transformerRgb : T.Transformer Rgb
transformerRgb =
    let
        all : List Rgb
        all =
            [ Rgb 0 0 0 ]

        encoder : Rgb -> T.CustomTypeAsTuple
        encoder (Rgb p1 p2 p3) =
            ( "Rgb", [ T.encode T.int p1, T.encode T.int p2, T.encode T.int p3 ] )

        decoder : T.CustomTypeAsTuple -> Rgb
        decoder tuple =
            case tuple of
                ( "Rgb", p1 :: p2 :: p3 :: _ ) ->
                    Rgb (T.decode T.int p1) (T.decode T.int p2) (T.decode T.int p3)

                _ ->
                    Rgb 0 0 0
    in
    T.helperForCustomTypes
        { all = all
        , decoder = decoder
        , encoder = encoder
        , name = Just "Rgb"
        }
"""


m16 :
    { getter : { b | val16 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val16 : b } -> { c | val16 : b })
    , t : T.Transformer Val16
    }
m16 =
    { getter = .val16
    , meta =
        { elmType = "Custom type: Rgb"
        , elmTypeDefinition = viewCode d16
        , key = 16
        , msgMap = Msg16
        , transformerEquivalent = viewCode t16
        }
    , setter = \b a -> { a | val16 = b }
    , t = transformerRgb
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val17 =
    Color


type Color
    = Green Int String
    | Multicolor (List Color)
    | Red Bool
    | Yellow


d17 : String
d17 =
    """type Color
    = Green Int String
    | Multicolor (List Color)
    | Red Bool
    | Yellow
"""


transformerColor : T.Transformer Color
transformerColor =
    let
        all : List Color
        all =
            [ Red False
            , Yellow
            , Green 0 ""
            , Multicolor []
            ]

        encoder : Color -> T.CustomTypeAsTuple
        encoder customType =
            case customType of
                Green p1 p2 ->
                    ( "Green", [ T.encode T.int p1, T.encode T.string p2 ] )

                Multicolor p1 ->
                    ( "Multicolor", [ T.encode (T.list transformerColor) p1 ] )

                Red p1 ->
                    ( "Red", [ T.encode T.bool p1 ] )

                Yellow ->
                    ( "Yellow", [] )

        decoder : T.CustomTypeAsTuple -> Color
        decoder tuple =
            case tuple of
                ( "Green", p1 :: p2 :: _ ) ->
                    Green (T.decode T.int p1) (T.decode T.string p2)

                ( "Multicolor", p1 :: _ ) ->
                    Multicolor (T.decode (T.list transformerColor) p1)

                ( "Red", p1 :: _ ) ->
                    Red (T.decode T.bool p1)

                _ ->
                    Yellow
    in
    T.helperForCustomTypes
        { all = all
        , decoder = decoder
        , encoder = encoder
        , name = Just "Color"
        }


t17 : String
t17 =
    """transformerColor : T.Transformer Color
transformerColor =
    let
        all : List Color
        all =
            [ Red False
            , Yellow
            , Green 0 ""
            , Multicolor []
            ]

        encoder : Color -> T.CustomTypeAsTuple
        encoder customType =
            case customType of
                Green p1 p2 ->
                    ( "Green", [ T.encode T.int p1, T.encode T.string p2 ] )

                Multicolor p1 ->
                    ( "Multicolor", [ T.encode (T.list transformerColor) p1 ] )

                Red p1 ->
                    ( "Red", [ T.encode T.bool p1 ] )

                Yellow ->
                    ( "Yellow", [] )

        decoder : T.CustomTypeAsTuple -> Color
        decoder tuple =
            case tuple of
                ( "Green", p1 :: p2 :: _ ) ->
                    Green (T.decode T.int p1) (T.decode T.string p2)

                ( "Multicolor", p1 :: _ ) ->
                    Multicolor (T.decode (T.list transformerColor) p1)

                ( "Red", p1 :: _ ) ->
                    Red (T.decode T.bool p1)

                _ ->
                    Yellow
    in
    T.helperForCustomTypes
        { all = all
        , decoder = decoder
        , encoder = encoder
        , name = Just "Color"
        }
"""


m17 :
    { getter : { b | val17 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val17 : b } -> { c | val17 : b })
    , t : T.Transformer Val17
    }
m17 =
    { getter = .val17
    , meta =
        { elmType = "Custom type: Color"
        , elmTypeDefinition = viewCode d17
        , key = 17
        , msgMap = Msg17
        , transformerEquivalent = viewCode t17
        }
    , setter = \b a -> { a | val17 = b }
    , t = transformerColor
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val18 =
    RecordSimple


type alias RecordSimple =
    { val1 : String
    , val2 : Bool
    }


d18 : String
d18 =
    """type alias RecordSimple =
    { val1 : String
    , val2 : Bool
    }
"""


transformerRecordSimple : T.Transformer RecordSimple
transformerRecordSimple =
    let
        meta =
            { val1 = { key = "val1", tr = T.string }
            , val2 = { key = "val2", tr = T.bool }
            }

        encoder : RecordSimple -> T.RecordAsDict
        encoder record =
            Dict.fromList
                [ ( meta.val1.key, T.encode meta.val1.tr record.val1 )
                , ( meta.val2.key, T.encode meta.val2.tr record.val2 )
                ]

        decoder : T.RecordAsDict -> RecordSimple
        decoder dict =
            { val1 = T.field meta.val1.tr meta.val1.key dict
            , val2 = T.field meta.val2.tr meta.val2.key dict
            }
    in
    T.helperForRecords
        { decoder = decoder
        , encoder = encoder
        , name = Just "RecordSimple"
        }


t18 : String
t18 =
    """transformerRecordSimple : T.Transformer RecordSimple
transformerRecordSimple =
    let
        meta =
            { val1 = { key = "val1", tr = T.string }
            , val2 = { key = "val2", tr = T.bool }
            }

        encoder : RecordSimple -> T.RecordAsDict
        encoder record =
            Dict.fromList
                [ ( meta.val1.key, T.encode meta.val1.tr record.val1 )
                , ( meta.val2.key, T.encode meta.val2.tr record.val2 )
                ]

        decoder : T.RecordAsDict -> RecordSimple
        decoder dict =
            { val1 = T.field meta.val1.tr meta.val1.key dict
            , val2 = T.field meta.val2.tr meta.val2.key dict
            }
    in
    T.helperForRecords
        { decoder = decoder
        , encoder = encoder
        , name = Just "RecordSimple"
        }
"""


m18 :
    { getter : { b | val18 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val18 : b } -> { c | val18 : b })
    , t : T.Transformer Val18
    }
m18 =
    { getter = .val18
    , meta =
        { elmType = "Record: RecordSimple"
        , elmTypeDefinition = viewCode d18
        , key = 18
        , msgMap = Msg18
        , transformerEquivalent = viewCode t18
        }
    , setter = \b a -> { a | val18 = b }
    , t = transformerRecordSimple
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val19 =
    RecordOfRecords


type alias RecordOfRecords =
    { val1 : List RecordSimple
    }


d19 : String
d19 =
    """type alias RecordOfRecords =
    { val1 : List RecordSimple
    }
"""


transformerRecordOfRecords : T.Transformer RecordOfRecords
transformerRecordOfRecords =
    let
        meta =
            { val1 = { key = "val2", tr = T.list transformerRecordSimple } }

        encoder : RecordOfRecords -> T.RecordAsDict
        encoder record =
            Dict.fromList [ ( meta.val1.key, T.encode meta.val1.tr record.val1 ) ]

        decoder : T.RecordAsDict -> RecordOfRecords
        decoder dict =
            { val1 = T.field meta.val1.tr meta.val1.key dict }
    in
    T.helperForRecords
        { decoder = decoder
        , encoder = encoder
        , name = Just "RecordOfRecords"
        }


t19 : String
t19 =
    """transformerRecordOfRecords : T.Transformer RecordOfRecords
transformerRecordOfRecords =
    let
        meta =
            { val1 = { key = "val2", tr = T.list transformerRecordSimple } }

        encoder : RecordOfRecords -> T.RecordAsDict
        encoder record =
            Dict.fromList [ ( meta.val1.key, T.encode meta.val1.tr record.val1 ) ]

        decoder : T.RecordAsDict -> RecordOfRecords
        decoder dict =
            { val1 = T.field meta.val1.tr meta.val1.key dict }
    in
    T.helperForRecords
        { decoder = decoder
        , encoder = encoder
        , name = Just "RecordOfRecords"
        }
"""


m19 :
    { getter : { b | val19 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val19 : b } -> { c | val19 : b })
    , t : T.Transformer Val19
    }
m19 =
    { getter = .val19
    , meta =
        { elmType = "Record: RecordOfRecords"
        , elmTypeDefinition = viewCode d19
        , key = 19
        , msgMap = Msg19
        , transformerEquivalent = viewCode t19
        }
    , setter = \b a -> { a | val19 = b }
    , t = transformerRecordOfRecords
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val20 =
    Bytes


m20 :
    { getter : { b | val20 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val20 : b } -> { c | val20 : b })
    , t : T.Transformer Val20
    }
m20 =
    { getter = .val20
    , meta =
        { elmType = "Bytes"
        , elmTypeDefinition = viewCode "Bytes"
        , key = 20
        , msgMap = Msg20
        , transformerEquivalent = viewCode "T.bytes"
        }
    , setter = \b a -> { a | val20 = b }
    , t = T.bytes
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val21 =
    Array String


m21 :
    { getter : { b | val21 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val21 : b } -> { c | val21 : b })
    , t : T.Transformer Val21
    }
m21 =
    { getter = .val21
    , meta =
        { elmType = "Array String"
        , elmTypeDefinition = viewCode "Array String"
        , key = 21
        , msgMap = Msg21
        , transformerEquivalent = viewCode "T.array T.string"
        }
    , setter = \b a -> { a | val21 = b }
    , t = T.array T.string
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val22 =
    Dict String String


m22 :
    { getter : { b | val22 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val22 : b } -> { c | val22 : b })
    , t : T.Transformer Val22
    }
m22 =
    { getter = .val22
    , meta =
        { elmType = "Dict String String"
        , elmTypeDefinition = viewCode "Dict String String"
        , key = 22
        , msgMap = Msg22
        , transformerEquivalent = viewCode "T.dict T.string T.string"
        }
    , setter = \b a -> { a | val22 = b }
    , t = T.dict T.string T.string
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val23 =
    Url


d23 : String
d23 =
    """-- As defined in
-- https://package.elm-lang.org/packages/elm/url/latest

type alias Url =
    { protocol : Protocol
    , host : String
    , port_ : Maybe Int
    , path : String
    , query : Maybe String
    , fragment : Maybe String
    }

type Protocol
    = Http
    | Https
"""


transformerUrl : T.Transformer Url
transformerUrl =
    let
        meta =
            { fragment = { key = "fragment", tr = T.maybe T.string }
            , host = { key = "host", tr = T.string }
            , path = { key = "path", tr = T.string }
            , port_ = { key = "port_", tr = T.maybe T.int }
            , protocol = { key = "protocol", tr = transformerHProtocol }
            , query = { key = "query", tr = T.maybe T.string }
            }

        encoder : Url -> T.RecordAsDict
        encoder record =
            Dict.fromList
                [ ( meta.protocol.key, T.encode meta.protocol.tr record.protocol )
                , ( meta.host.key, T.encode meta.host.tr record.host )
                , ( meta.port_.key, T.encode meta.port_.tr record.port_ )
                , ( meta.path.key, T.encode meta.path.tr record.path )
                , ( meta.query.key, T.encode meta.query.tr record.query )
                , ( meta.fragment.key, T.encode meta.fragment.tr record.fragment )
                ]

        decoder : T.RecordAsDict -> Url
        decoder dict =
            { fragment = T.field meta.fragment.tr meta.fragment.key dict
            , host = T.field meta.host.tr meta.host.key dict
            , path = T.field meta.path.tr meta.path.key dict
            , port_ = T.field meta.port_.tr meta.port_.key dict
            , protocol = T.field meta.protocol.tr meta.protocol.key dict
            , query = T.field meta.query.tr meta.query.key dict
            }
    in
    T.helperForRecords
        { decoder = decoder
        , encoder = encoder
        , name = Just "Url.Url"
        }


transformerHProtocol : T.Transformer Protocol
transformerHProtocol =
    let
        all : List Protocol
        all =
            [ Url.Http, Url.Https ]

        encoder : Protocol -> T.CustomTypeAsTuple
        encoder customType =
            case customType of
                Url.Http ->
                    ( "Http", [] )

                Url.Https ->
                    ( "Https", [] )

        decoder : T.CustomTypeAsTuple -> Protocol
        decoder tuple =
            case tuple of
                ( "Http", _ ) ->
                    Url.Http

                _ ->
                    Url.Https
    in
    T.helperForCustomTypes
        { all = all
        , decoder = decoder
        , encoder = encoder
        , name = Just "Url.Protocol"
        }


t23 : String
t23 =
    """transformerUrl : T.Transformer Url
transformerUrl =
    let
        meta =
            { fragment = { key = "fragment", tr = T.maybe T.string }
            , host = { key = "host", tr = T.string }
            , path = { key = "path", tr = T.string }
            , port_ = { key = "port_", tr = T.maybe T.int }
            , protocol = { key = "protocol", tr = transformerHProtocol }
            , query = { key = "query", tr = T.maybe T.string }
            }

        encoder : Url -> T.RecordAsDict
        encoder record =
            Dict.fromList
                [ ( meta.protocol.key, T.encode meta.protocol.tr record.protocol )
                , ( meta.host.key, T.encode meta.host.tr record.host )
                , ( meta.port_.key, T.encode meta.port_.tr record.port_ )
                , ( meta.path.key, T.encode meta.path.tr record.path )
                , ( meta.query.key, T.encode meta.query.tr record.query )
                , ( meta.fragment.key, T.encode meta.fragment.tr record.fragment )
                ]

        decoder : T.RecordAsDict -> Url
        decoder dict =
            { fragment = T.field meta.fragment.tr meta.fragment.key dict
            , host = T.field meta.host.tr meta.host.key dict
            , path = T.field meta.path.tr meta.path.key dict
            , port_ = T.field meta.port_.tr meta.port_.key dict
            , protocol = T.field meta.protocol.tr meta.protocol.key dict
            , query = T.field meta.query.tr meta.query.key dict
            }
    in
    T.helperForRecords
        { decoder = decoder
        , encoder = encoder
        , name = Just "Url.Url"
        }


transformerHProtocol : T.Transformer Protocol
transformerHProtocol =
    let
        all : List Protocol
        all =
            [ Url.Http, Url.Https ]

        encoder : Protocol -> T.CustomTypeAsTuple
        encoder customType =
            case customType of
                Url.Http ->
                    ( "Http", [] )

                Url.Https ->
                    ( "Https", [] )

        decoder : T.CustomTypeAsTuple -> Protocol
        decoder tuple =
            case tuple of
                ( "Http", _ ) ->
                    Url.Http

                _ ->
                    Url.Https
    in
    T.helperForCustomTypes
        { all = all
        , decoder = decoder
        , encoder = encoder
        , name = Just "Url.Protocol"
        }
"""


m23 :
    { getter : { b | val23 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val23 : b } -> { c | val23 : b })
    , t : T.Transformer Val23
    }
m23 =
    { getter = .val23
    , meta =
        { elmType = "Url.Url"
        , elmTypeDefinition = viewCode d23
        , key = 23
        , msgMap = Msg23
        , transformerEquivalent = viewCode t23
        }
    , setter = \b a -> { a | val23 = b }
    , t = transformerUrl
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Val24 =
    Error


d24 : String
d24 =
    """-- As defined in 
-- https://package.elm-lang.org/packages/elm/http/latest

type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int
    | BadBody String
"""


transformerError : T.Transformer Error
transformerError =
    let
        all : List Error
        all =
            [ Http.BadBody ""
            , Http.BadStatus 500
            , Http.BadUrl ""
            , Http.NetworkError
            , Http.Timeout
            ]

        encoder : Error -> T.CustomTypeAsTuple
        encoder customType =
            case customType of
                Http.BadBody p1 ->
                    ( "BadBody", [ T.encode T.string p1 ] )

                Http.BadStatus p1 ->
                    ( "BadStatus", [ T.encode T.int p1 ] )

                Http.BadUrl p1 ->
                    ( "BadUrl", [ T.encode T.string p1 ] )

                Http.NetworkError ->
                    ( "NetworkError", [] )

                Http.Timeout ->
                    ( "Timeout", [] )

        decoder : T.CustomTypeAsTuple -> Error
        decoder tuple =
            case tuple of
                ( "BadBody", p1 :: _ ) ->
                    Http.BadBody (T.decode T.string p1)

                ( "BadStatus", p1 :: _ ) ->
                    Http.BadStatus (T.decode T.int p1)

                ( "BadUrl", p1 :: _ ) ->
                    Http.BadUrl (T.decode T.string p1)

                ( "NetworkError", _ ) ->
                    Http.NetworkError

                _ ->
                    Http.Timeout
    in
    T.helperForCustomTypes
        { all = all
        , decoder = decoder
        , encoder = encoder
        , name = Just "Http.Error"
        }


t24 : String
t24 =
    """transformerError : T.Transformer Error
transformerError =
    let
        all : List Error
        all =
            [ Http.BadBody ""
            , Http.BadStatus 500
            , Http.BadUrl ""
            , Http.NetworkError
            , Http.Timeout
            ]

        encoder : Error -> T.CustomTypeAsTuple
        encoder customType =
            case customType of
                Http.BadBody p1 ->
                    ( "BadBody", [ T.encode T.string p1 ] )

                Http.BadStatus p1 ->
                    ( "BadStatus", [ T.encode T.int p1 ] )

                Http.BadUrl p1 ->
                    ( "BadUrl", [ T.encode T.string p1 ] )

                Http.NetworkError ->
                    ( "NetworkError", [] )

                Http.Timeout ->
                    ( "Timeout", [] )

        decoder : T.CustomTypeAsTuple -> Error
        decoder tuple =
            case tuple of
                ( "BadBody", p1 :: _ ) ->
                    Http.BadBody (T.decode T.string p1)

                ( "BadStatus", p1 :: _ ) ->
                    Http.BadStatus (T.decode T.int p1)

                ( "BadUrl", p1 :: _ ) ->
                    Http.BadUrl (T.decode T.string p1)

                ( "NetworkError", _ ) ->
                    Http.NetworkError

                _ ->
                    Http.Timeout
    in
    T.helperForCustomTypes
        { all = all
        , decoder = decoder
        , encoder = encoder
        , name = Just "Http.Error"
        }
"""


m24 :
    { getter : { b | val24 : a } -> a
    , meta : Meta
    , setter : b -> ({ c | val24 : b } -> { c | val24 : b })
    , t : T.Transformer Val24
    }
m24 =
    { getter = .val24
    , meta =
        { elmType = "Http.Error"
        , elmTypeDefinition = viewCode d24
        , key = 24
        , msgMap = Msg24
        , transformerEquivalent = viewCode t24
        }
    , setter = \b a -> { a | val24 = b }
    , t = transformerError
    }



-- o -- o -- o -- o -- o -- o -- o -- o -- o --


type alias Meta =
    { elmType : String
    , elmTypeDefinition : Html.Html T.Msg
    , key : Int
    , msgMap : T.Msg -> Msg
    , transformerEquivalent : Html.Html T.Msg
    }


type alias Model =
    { elmUi : Bool
    , val01 : Val01
    , val02 : Val02
    , val03 : Val03
    , val04 : Val04
    , val05 : Val05
    , val06 : Val06
    , val07 : Val07
    , val08 : Val08
    , val09 : Val09
    , val10 : Val10
    , val11 : Val11
    , val12 : Val12
    , val13 : Val13
    , val14 : Val14
    , val15 : Val15
    , val16 : Val16
    , val17 : Val17
    , val18 : Val18
    , val19 : Val19
    , val20 : Val20
    , val21 : Val21
    , val22 : Val22
    , val23 : Val23
    , val24 : Val24
    }


type Msg
    = ChangeJson (Maybe Val22)
    | ChangeUrl String
    | Msg01 T.Msg
    | Msg02 T.Msg
    | Msg03 T.Msg
    | Msg04 T.Msg
    | Msg05 T.Msg
    | Msg06 T.Msg
    | Msg07 T.Msg
    | Msg08 T.Msg
    | Msg09 T.Msg
    | Msg10 T.Msg
    | Msg11 T.Msg
    | Msg12 T.Msg
    | Msg13 T.Msg
    | Msg14 T.Msg
    | Msg15 T.Msg
    | Msg16 T.Msg
    | Msg17 T.Msg
    | Msg18 T.Msg
    | Msg19 T.Msg
    | Msg20 T.Msg
    | Msg21 T.Msg
    | Msg22 T.Msg
    | Msg23 T.Msg
    | Msg24 T.Msg
    | ToggleElmUi


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeJson maybeVal22 ->
            case maybeVal22 of
                Just val22 ->
                    m22.setter val22 model

                Nothing ->
                    model

        ChangeUrl string_ ->
            { model | val23 = Maybe.withDefault model.val23 <| Url.fromString string_ }

        Msg01 msg_ ->
            updateHelper msg_ model m01

        Msg02 msg_ ->
            updateHelper msg_ model m02

        Msg03 msg_ ->
            updateHelper msg_ model m03

        Msg04 msg_ ->
            updateHelper msg_ model m04

        Msg05 msg_ ->
            updateHelper msg_ model m05

        Msg06 msg_ ->
            updateHelper msg_ model m06

        Msg07 msg_ ->
            updateHelper msg_ model m07

        Msg08 msg_ ->
            updateHelper msg_ model m08

        Msg09 msg_ ->
            updateHelper msg_ model m09

        Msg10 msg_ ->
            updateHelper msg_ model m10

        Msg11 msg_ ->
            updateHelper msg_ model m11

        Msg12 msg_ ->
            updateHelper msg_ model m12

        Msg13 msg_ ->
            updateHelper msg_ model m13

        Msg14 msg_ ->
            updateHelper msg_ model m14

        Msg15 msg_ ->
            updateHelper msg_ model m15

        Msg16 msg_ ->
            updateHelper msg_ model m16

        Msg17 msg_ ->
            updateHelper msg_ model m17

        Msg18 msg_ ->
            updateHelper msg_ model m18

        Msg19 msg_ ->
            updateHelper msg_ model m19

        Msg20 msg_ ->
            updateHelper msg_ model m20

        Msg21 msg_ ->
            updateHelper msg_ model m21

        Msg22 msg_ ->
            updateHelper msg_ model m22

        Msg23 msg_ ->
            updateHelper msg_ model m23

        Msg24 msg_ ->
            updateHelper msg_ model m24

        ToggleElmUi ->
            { model | elmUi = not model.elmUi }


updateHelper :
    T.Msg
    -> Model
    ->
        { x
            | setter : a -> Model -> Model
            , t : T.Transformer a
        }
    -> Model
updateHelper msg_ model x =
    case T.update msg_ of
        Just value ->
            x.setter (T.decode x.t value) model

        _ ->
            model


init : Model
init =
    { elmUi = True
    , val01 = "Hello!"
    , val02 = 42
    , val03 = pi
    , val04 = False
    , val05 = '💙'
    , val06 = ( "", 0 )
    , val07 = ( "", 0, False )
    , val08 = []
    , val09 = []
    , val10 = Dict.empty
    , val11 = Dict.empty
    , val12 = Set.empty
    , val13 = Nothing
    , val14 = Ok 200
    , val15 = Up
    , val16 = Rgb 0 0 0
    , val17 = Yellow
    , val18 = { val1 = "", val2 = False }
    , val19 = { val1 = [] }
    , val20 = Bytes.Encode.encode <| Bytes.Encode.string "💙"
    , val21 = Array.empty
    , val22 = Dict.empty
    , val23 =
        { fragment = Nothing
        , host = "example.com"
        , path = ""
        , port_ = Nothing
        , protocol = Url.Https
        , query = Nothing
        }
    , val24 = Http.NetworkError
    }


view : Model -> Html.Html Msg
view model =
    layout
        [ Font.family [ Font.sansSerif ]
        , Font.size 16
        , padding 16
        ]
    <|
        column [ spacing 16 ]
            [ markdownExplanation1
            , el [] <| html <| counterCode
            , markdownExplanation2
            , viewExample model m22
            , markdownExplanation3
            , el [ width <| px 500 ] <|
                html <|
                    Html.textarea
                        [ Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "height" "300px"
                        , Html.Attributes.style "padding" "16px"
                        , Html.Events.onInput
                            (\string_ ->
                                case Codec.decodeString Transformer.Codec.codecValue string_ of
                                    Err _ ->
                                        ChangeJson Nothing

                                    Ok ok ->
                                        ChangeJson (Just (T.decode m22.t ok))
                            )
                        , Html.Attributes.value
                            (Codec.encodeToString 4
                                Transformer.Codec.codecValue
                                (T.encode
                                    m22.t
                                    (m22.getter model)
                                )
                            )
                        ]
                        []
            , markdownExplanation4
            , viewExamples model
            , newTabLink [ Font.size 13, Font.underline, Font.color <| rgba 0 0 0 0.5 ] { label = text "GitHub", url = repo }
            , html <| SyntaxHighlight.useTheme SyntaxHighlight.gitHub
            , html <| Html.node "style" [] [ Html.text <| cssCode ++ css ]
            ]


toggleCheckboxWidget :
    { offColor : Element.Color
    , onColor : Element.Color
    , sliderColor : Element.Color
    , toggleHeight : Int
    , toggleWidth : Int
    }
    -> Bool
    -> Element msg
toggleCheckboxWidget { offColor, onColor, sliderColor, toggleHeight, toggleWidth } checked =
    -- From https://ellie-app.com/85HbWTjCGWha1
    let
        pad : number
        pad =
            3

        sliderSize : Int
        sliderSize =
            toggleHeight - 2 * pad
    in
    el
        [ Background.color <|
            if checked then
                onColor

            else
                offColor
        , width <| px <| toggleWidth
        , height <| px <| toggleHeight
        , Border.rounded 14
        , htmlAttribute <| Html.Attributes.style "box-shadow" "rgba(0, 0, 0, 0.5) 0px 3px 2px -1px inset"
        , inFront <|
            el [ height fill ] <|
                el
                    [ Background.color sliderColor
                    , Border.rounded <| sliderSize // 2
                    , width <| px <| sliderSize
                    , height <| px <| sliderSize
                    , centerY
                    , moveRight pad
                    , htmlAttribute <| Html.Attributes.style "transition" ".2s"
                    , htmlAttribute <| Html.Attributes.style "box-shadow" "rgba(0, 0, 0, 0.5) 0px 3px 2px -1px"
                    , htmlAttribute <|
                        if checked then
                            let
                                translation : String
                                translation =
                                    (toggleWidth - sliderSize - (pad * 2) + 3)
                                        |> String.fromInt
                            in
                            Html.Attributes.style "transform" <| "translateX(" ++ translation ++ "px)"

                        else
                            Html.Attributes.class ""
                    ]
                <|
                    text ""
        ]
    <|
        text ""


cssCode : String
cssCode =
    """
.elmsh
    { padding: 8px
    ; border-radius: 8px
    ; background-color : rgba(0,0,0,0.05)
    ; border: 1px solid lightGray
    ; line-height: 1.2rem
    ; margin: 0
    ; font-size: 14px
    }
"""


css : String
css =
    """.elm-transformer-form
    { font-family: monospace
    ; font-size: 14px
    ; border: 1px solid lightgray
    ; padding: 16px
    ; border-radius: 8px
    }

.elm-transformer-form div,
.elm-transformer-form label
    { align-content: center }

.elm-transformer-form input
    { margin: 0 16px 0 0
    ; font-family: monospace
    ; font-size 13px
    ; padding: 4px
    }

.elm-transformer-form label,
.elm-transformer-form button
    { cursor: pointer }

.elm-transformer-column
    { gap: 16px
    ; display: flex
    ; flex-direction: column
    }

.elm-transformer-row
    { gap: 16px
    ; display: flex
    ; flex-direction: row
    }"""


repo : String
repo =
    "https://github.com/lucamug/elm-transformer/blob/main/"


markdownExplanation1 : Element msg
markdownExplanation1 =
    docs <| """# elm-transformer

A library for visualizing and editing any Elm data structure. It's useful for observing how an application reacts to real-time changes in its internal data.

## Design Goals

Provide a way to visualize and edit a single piece of data, multiple pieces of data, or the entire `Model` via a web form with minimal effort.

## Example 1 - Editing the Model of the Counter Application

Starting from the [canonical counter application](https://elm-lang.org/examples/buttons), let's add all the necessary components to make the `Model` editable.

See the highlighted lines that have been added.

[Code](""" ++ repo ++ """src/Counter.elm) - [Demo](counter.html)
"""


counterCode : Html.Html msg
counterCode =
    viewCodeCounter <| """module Counter exposing (main)

import Browser
import Html exposing (Html, button, div, map, text)
import Html.Events exposing (onClick)
import Transformer as T

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }

type alias Model =
    Int

transformerModel : T.Transformer Model
transformerModel =
    T.int

init : Model
init =
    0

type Msg
    = Decrement
    | Increment
    | MsgTransformer T.Msg

update : Msg -> Model -> Model
update msg model =
    case msg of
        Decrement ->
            model - 1

        Increment ->
            model + 1

        MsgTransformer msgTransformer ->
            T.update msgTransformer
                |> Maybe.map (T.decode transformerModel)
                |> Maybe.withDefault model

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        , map MsgTransformer (T.viewFormElmUiAsHtml (T.encode transformerModel model))
        ]
"""


markdownExplanation2 : Element msg
markdownExplanation2 =
    docs <| """
## Example 2 - Wrapping an Existing Application to Edit the Model

In this example, we add an extra layer of Elm code to the common To-do List Application. The original application [is not modified](todo.html), but is simply called from the wrapper.

[Code](""" ++ repo ++ """src/TodoWrapper.elm) - [Demo](todo-wrapped.html)

## How To Use

* Copy the `elm-transformer` folder into your project.
* Add `elm-transformer/src` to the `source-directories` in your `elm.json` file.
* Add these dependencies, if not already present in your project:
  * `elm/bytes`
  * `elmcraft/core-extra`
  * `jxxcarlson/hex`
  * `mdgriffith/elm-ui`
  * `miniBill/elm-codec`
  * `rtfeldman/elm-hex`
  * `zwilias/elm-utf-tools`
* Import the library. In these examples, it's imported with the alias **T**: `import Transformer as T`.
* Add a new message that has `T.Msg` as its payload.
* Add a section in your `update` function to handle this new message (see the Counter example).
* Create a `Transformer` for the data you wish to edit.
* Add the form to your view via `T.viewForm` (see the Counter example).

## Flow

* Suppose `a` is the Elm data that needs to be edited.
* Create a transformer for `a`, such as `transformer: Transformer a`.
* Convert `a` to `Value` by passing the transformer and `a` to `encode : Transformer a -> (a -> Value)`.
* Provide this `Value` to `viewForm : Value -> Html.Html Msg` to render the form.
* When a `Msg` arrives, use the `update : Msg -> Maybe Value` function to get the new `Value`.
* Convert the `Value` back to `a` using `decode : Transformer a -> (Value -> a)`.
* Store the new `a` in your `Model`. If `a` represents the entire `Model`, replace the entire `Model` with it.

## Views

There are three different types of view generators:

### 1. `viewForm : Value -> Html.Html Msg`

This outputs simple HTML. You will need to add some CSS to make it look nice, for example:

```
""" ++ css ++ """
```

### 2. `viewFormElmUiAsHtml : Value -> Html.Html Msg`

This also outputs HTML, but it is generated using the [`elm-ui` library](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/). It does not require any extra styling. Be aware that it may conflict with your existing styling.

### 3. `viewFormElmUi : Value -> Element.Element Msg`

This outputs an `elm-ui` **Element**, which is useful for including the form in an application already built using `elm-ui`.

## Codec

The library provides `Transformer.Codec.codecValue`, a **Codec** useful for serializing the `Value` type to JSON. The serialization is not optimized as it contains the overhead of metadata.

For example, for a `""" ++ "Dict String String" ++ """` type of data:

"""


markdownExplanation3 : Element msg
markdownExplanation3 =
    docs <| """This is how it would be serialized by the Codec (editable):"""


markdownExplanation4 : Element msg
markdownExplanation4 =
    docs <| """## Notes

* This library requires writing additional code to describe the data; it does not rely on a code generator for this purpose.
* The generated form performs validation to ensure that the edited data remains valid at all times. This validation is not the standard approach where users can type freely and errors pop up below the input field. Instead, this library prevents user input if it would immediately invalidate the data. If this occurs, it's typically necessary to add or remove multiple characters at once (e.g., using copy/paste) to transition between valid states.
* Editing an item to have an ID identical to an existing item's ID may cause it to be overwritten or deleted, in case of structures such as Dicts or Sets.
* The `update` function has a slightly different type signature compared to standard `update` functions in TEA. It does not require the model, as all necessary information is already contained in the message.
* This library is not intended to be a generic form generator, as customization is very limited and performance may not be optimal. There are other libraries specifically designed with such goals in mind, like [dillonkearns/elm-form](https://package.elm-lang.org/packages/dillonkearns/elm-form/latest/).
* If you squint, you can think of a `Transformer` as analogous to a [`Codec`](https://package.elm-lang.org/packages/miniBill/elm-codec/latest/Codec#Codec), if you are familiar with [that library](https://package.elm-lang.org/packages/miniBill/elm-codec/latest/). It contains the information to encode and decode a type into another type.
* If you want to edit independently different pieces of data that are not connected, set up multiple message types. See [this code](""" ++ repo ++ "src/Main.elm" ++ """) for an example.

## Examples of Transformers

This is a list of built-in transformers and examples of custom transformers that need to be created for more complex data structures, such as custom types or records.
"""


viewCodeCounter : String -> Html.Html msg
viewCodeCounter elmCode =
    SyntaxHighlight.elm elmCode
        |> Result.map (SyntaxHighlight.highlightLines (Just SyntaxHighlight.Highlight) 5 6)
        |> Result.map (SyntaxHighlight.highlightLines (Just SyntaxHighlight.Highlight) 14 17)
        |> Result.map (SyntaxHighlight.highlightLines (Just SyntaxHighlight.Highlight) 25 26)
        |> Result.map (SyntaxHighlight.highlightLines (Just SyntaxHighlight.Highlight) 36 40)
        |> Result.map (SyntaxHighlight.highlightLines (Just SyntaxHighlight.Highlight) 47 48)
        |> Result.map (SyntaxHighlight.toBlockHtml (Just 1))
        |> Result.withDefault (Html.pre [] [ Html.code [] [ Html.text elmCode ] ])


viewCode : String -> Html.Html msg
viewCode elmCode =
    SyntaxHighlight.elm elmCode
        |> Result.map (SyntaxHighlight.toBlockHtml (Just 10))
        |> Result.withDefault
            (Html.pre []
                [ Html.code [] [ Html.text elmCode ] ]
            )


viewExamples : Model -> Element Msg
viewExamples model =
    column [ spacing 32, paddingXY 0 40 ]
        (List.intersperse
            (el [ Background.color <| rgba 0 0 0 0.1, width fill, height <| px 4 ] <| none)
            [ viewExample model m01 -- string
            , viewExample model m04 -- bool
            , viewExample model m02 -- int
            , viewExample model m03 -- float
            , viewExample model m05 -- char
            , viewExample model m20 -- bytes
            , viewExample model m06 -- tuple ( string, int )
            , viewExample model m07 -- triple ( string, int, bool )
            , viewExample model m08 -- list string
            , viewExample model m09 -- list int
            , viewExample model m21 -- array string
            , viewExample model m12 -- set string
            , viewExample model m11 -- dict string string
            , viewExample model m10 -- dict int string
            , viewExample model m13 -- maybe string
            , viewExample model m14 -- result string int
            , viewExample model m15 -- Direction
            , viewExample model m16 -- Rgb
            , viewExample model m17 -- Color
            , viewExample model m18 -- RecordSimple
            , viewExample model m19 -- RecordOfRecords
            , column [ spacing 16 ]
                [ viewExample model m23 -- Url.Url
                , viewRowTable "Edit as Url"
                    (Input.text [ width <| px 400 ]
                        { label = Input.labelHidden ""
                        , onChange = \s -> ChangeUrl s
                        , placeholder = Nothing
                        , text = Url.toString model.val23
                        }
                    )
                ]
            , viewExample model m24 -- Http.Error
            ]
        )


viewExample :
    Model
    -> { d | getter : Model -> a, meta : Meta, t : T.Transformer a }
    -> Element Msg
viewExample model mStuff =
    column [ spacing 16 ]
        [ map mStuff.meta.msgMap <|
            column [ spacing 12 ]
                [ el [ Font.bold, Font.size 18, paddingEach { bottom = 32, left = 0, right = 0, top = 0 } ] <| text mStuff.meta.elmType
                , viewRowTable "Type definition"
                    (el [] <| html <| mStuff.meta.elmTypeDefinition)
                , viewRowTable "T.Transformer"
                    (el [] <| html <| mStuff.meta.transformerEquivalent)
                , viewRowTable "Debug.toString"
                    (paragraph []
                        [ el []
                            (mStuff.getter model
                                |> Debug.toString
                                |> viewCode
                                |> html
                            )
                        ]
                    )
                , viewRowTable "T.Transformer.viewForm"
                    (if model.elmUi then
                        T.viewFormElmUi (T.encode mStuff.t (mStuff.getter model))

                     else
                        el [] <| html <| T.viewForm (T.encode mStuff.t (mStuff.getter model))
                    )
                ]
        , row
            [ spacing 8
            , Element.Events.onClick ToggleElmUi
            , pointer
            , Font.color <| rgba 0 0 0 0.4
            , Font.size 13
            , Font.family [ Font.monospace ]
            ]
            [ el [] <| text "HTML"
            , toggleCheckboxWidget
                { offColor = rgba 0 0 0 0.2
                , onColor = rgba 0 0.4 0.8 0.7
                , sliderColor = rgba 1 1 1 0.8
                , toggleHeight = 16
                , toggleWidth = 32
                }
                model.elmUi
            , el [] <| text "ELM-UI"
            ]
        ]


viewRowTable : String -> Element msg -> Element msg
viewRowTable string content =
    row [ spacing 16 ]
        [ el [ width <| px 180, alignTop ] <| text string
        , content
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


docs : String -> Element msg
docs markdown =
    case markdownToElements markdown of
        Err errors ->
            text errors

        Ok rendered ->
            column [ spacing 16 ]
                rendered


markdownToElements : String -> Result String (List (Element msg))
markdownToElements markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError (\error -> error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
        |> Result.andThen (Markdown.Renderer.render Renderer.renderer)
