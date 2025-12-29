module Transformer.Internal exposing
    ( Ancestor
    , Ancestors
    , CustomTypeAsTuple
    , CustomTypeDefinition
    , CustomTypePayload
    , ListDefinition
    , Msg(..)
    , Name
    , Position(..)
    , RecordAsDict
    , Transformer(..)
    , Value(..)
    , fromBytesToStringHex
    , fromListOfIntsToStringUtf8
    , fromStringHexToBytes
    , fromStringHexToBytesWithDefault
    , fromStringHexToListOfInts
    , fromStringUtf8ToBytes
    , isListComparable
    )

import Bytes exposing (Bytes)
import Bytes.Encode
import Dict
import Hex
import Hex.Convert
import String.UTF8


type Value
    = VBool Bool
    | VBytes Bytes
    | VChar Char
    | VCustom CustomTypeDefinition CustomTypeAsTuple
    | VFloat Float
    | VInt Int
    | VList ListDefinition (List Value)
    | VRecord Name RecordAsDict
    | VString String
    | VTriple ( Value, Value, Value )
    | VTuple ( Value, Value )


type alias ListDefinition =
    { isDictOrSet : Bool, name : String }


type alias Name =
    Maybe String


type alias CustomTypeDefinition =
    ( Name, List CustomTypeAsTuple )


type alias CustomTypeAsTuple =
    ( String, CustomTypePayload )


type alias RecordAsDict =
    Dict.Dict String Value


type alias CustomTypePayload =
    List Value


type alias Ancestors =
    List Ancestor


type alias Ancestor =
    ( Value, String )


type Msg
    = ChangeBool Ancestors Bool
    | ChangeBytes Ancestors Bytes.Bytes
    | ChangeChar Ancestors String
    | ChangeCustomType Ancestors String
    | ChangeFloat Ancestors String
    | ChangeInt Ancestors String
    | ChangeString Ancestors String
    | ItemAdd Position Ancestors
    | ItemDelete Ancestors
    | Reverse Ancestors
    | SortDown Ancestors
    | SortUp Ancestors


type Position
    = Bottom
    | Top


type Transformer a
    = Transformer
        { decoder : Value -> a
        , encoder : a -> Value
        }


isListComparable : List Value -> Bool
isListComparable list_ =
    case List.head list_ of
        Just value ->
            isComparable value

        Nothing ->
            False


isComparable : Value -> Bool
isComparable value =
    case value of
        VChar _ ->
            True

        VFloat _ ->
            True

        VInt _ ->
            True

        VList _ v ->
            isListComparable v

        VString _ ->
            True

        _ ->
            False



-- Bytes


fromStringUtf8ToBytes : String -> Bytes.Bytes
fromStringUtf8ToBytes =
    Bytes.Encode.encode << Bytes.Encode.string


fromStringHexToListOfInts : String -> List Int
fromStringHexToListOfInts asHexString_ =
    asHexString_
        |> Hex.Convert.blocks 2
        |> List.map (String.toLower >> Hex.fromString)
        |> List.filterMap Result.toMaybe


fromListOfIntsToStringUtf8 : List Int -> String
fromListOfIntsToStringUtf8 asListOfInts_ =
    asListOfInts_
        |> String.UTF8.toString
        |> Result.withDefault "Error"


fromStringHexToBytesWithDefault : Bytes.Bytes -> String -> Bytes.Bytes
fromStringHexToBytesWithDefault defaultBytes string =
    Maybe.withDefault defaultBytes (Hex.Convert.toBytes string)


fromBytesToStringHex : Bytes.Bytes -> String
fromBytesToStringHex bytes =
    Hex.Convert.toString bytes


fromStringHexToBytes : String -> Bytes.Bytes
fromStringHexToBytes string =
    Maybe.withDefault (fromStringUtf8ToBytes "💙 Error 💙") (Hex.Convert.toBytes string)
