module Transformer exposing
    ( Transformer, Value, Msg
    , string, bool, int, float, char, bytes
    , tuple, triple, list, array, set, dict, maybe, result
    , helperForRecords, RecordAsDict, field
    , helperForCustomTypes, CustomTypeAsTuple
    , decode, encode
    , viewForm, viewFormElmUi, viewFormElmUiAsHtml
    , update
    , map
    , listWithName, arrayWithName, setWithName, dictWithName, defaultValue
    )

{-|


# Definition

@docs Transformer, Value, Msg


# Primitives

@docs string, bool, int, float, char, bytes


# Data Structures

@docs tuple, triple, list, array, set, dict, maybe, result


# Records

@docs helperForRecords, RecordAsDict, field


# Custom Types

@docs helperForCustomTypes, CustomTypeAsTuple


# Decode/Encode

@docs decode, encode


# Views

@docs viewForm, viewFormElmUi, viewFormElmUiAsHtml


# update

@docs update


# Mapping

@docs map


# Miscellaneous

@docs listWithName, arrayWithName, setWithName, dictWithName, defaultValue

-}

-- For alternatives:
--
-- *  https://github.com/lamdera/fusion/blob/main/src/Fusion.elm

import Array
import Bytes
import Bytes.Encode
import Dict
import Element
import Html
import List.Extra
import Set
import Transformer.Form as TF
import Transformer.Internal as TI


{-| -}
type alias Value =
    TI.Value


{-| -}
type alias CustomTypeAsTuple =
    TI.CustomTypeAsTuple


{-| -}
type alias RecordAsDict =
    TI.RecordAsDict


type alias Ancestors =
    TI.Ancestors


{-| -}
type alias Transformer a =
    TI.Transformer a


{-| -}
type alias Msg =
    TI.Msg


{-| -}
viewForm : Value -> Html.Html Msg
viewForm =
    TF.viewFormHtml


{-| -}
viewFormElmUi : Value -> Element.Element Msg
viewFormElmUi v =
    TF.viewFormElmUi v


{-| -}
viewFormElmUiAsHtml : Value -> Html.Html Msg
viewFormElmUiAsHtml =
    TF.viewFormElmUiAsHtml


helper : Ancestors -> Value -> Value
helper ancestors value =
    case ancestors of
        [] ->
            value

        ( ancestorValue, ancestorkey ) :: xs ->
            --
            -- Here is where the magic happen
            --
            -- This section update the value and then it keep updating
            -- the parents until it reaches the root.
            --
            helper xs
                (case ancestorValue of
                    TI.VCustom customTypeDefinition ( name, payload ) ->
                        let
                            newPayload : TI.CustomTypePayload
                            newPayload =
                                case String.toInt ancestorkey of
                                    Just int_ ->
                                        List.Extra.setAt int_ value payload

                                    Nothing ->
                                        payload
                        in
                        TI.VCustom customTypeDefinition ( name, newPayload )

                    TI.VList listDefinition list_ ->
                        case String.toInt ancestorkey of
                            Just k ->
                                TI.VList listDefinition (List.Extra.setAt (k - 1) value list_)

                            Nothing ->
                                ancestorValue

                    TI.VRecord name dict_ ->
                        TI.VRecord name (Dict.insert ancestorkey value dict_)

                    TI.VTriple ( a, b, c ) ->
                        if ancestorkey == "0" then
                            TI.VTriple ( value, b, c )

                        else if ancestorkey == "1" then
                            TI.VTriple ( a, value, c )

                        else
                            TI.VTriple ( a, b, value )

                    TI.VTuple ( a, b ) ->
                        if ancestorkey == "0" then
                            TI.VTuple ( value, b )

                        else
                            TI.VTuple ( a, value )

                    _ ->
                        ancestorValue
                )


sortList : List Value -> List Value
sortList list_ =
    case List.head list_ of
        Just (TI.VString _) ->
            list_
                |> List.map (decode string)
                |> List.sort
                |> List.map (encode string)

        Just (TI.VInt _) ->
            list_
                |> List.map (decode int)
                |> List.sort
                |> List.map (encode int)

        Just (TI.VFloat _) ->
            list_
                |> List.map (decode float)
                |> List.sort
                |> List.map (encode float)

        Just (TI.VChar _) ->
            list_
                |> List.map (decode char)
                |> List.sort
                |> List.map (encode char)

        _ ->
            list_


{-| -}
update : Msg -> Maybe Value
update msg =
    case msg of
        TI.ChangeBool ancestors boolSubmittedByUser ->
            Just <| helper ancestors (TI.VBool boolSubmittedByUser)

        TI.ChangeBytes ancestors bytesSubmittedByUser ->
            Just <| helper ancestors (TI.VBytes bytesSubmittedByUser)

        TI.ChangeChar ancestors stringSubmittedByUser ->
            Maybe.map (\char_ -> helper ancestors (TI.VChar char_)) (stringSubmittedByUser |> String.toList |> List.head)

        TI.ChangeCustomType ancestors stringSubmittedByUser ->
            case ancestors of
                ( ancestorValue, _ ) :: xs ->
                    case ancestorValue of
                        TI.VCustom ( customTypeName, customTypeInstances ) ( name, _ ) ->
                            if name == stringSubmittedByUser then
                                -- User clicked on the already selected custom type
                                Nothing

                            else
                                let
                                    newCustom : Maybe Value
                                    newCustom =
                                        customTypeInstances |> List.filter (\( name_, _ ) -> name_ == stringSubmittedByUser) |> List.head |> Maybe.map (TI.VCustom ( customTypeName, customTypeInstances ))
                                in
                                Maybe.map (\nv -> helper xs nv) newCustom

                        _ ->
                            Nothing

                [] ->
                    Nothing

        TI.ChangeFloat ancestors stringSubmittedByUser ->
            Maybe.map (\float_ -> helper ancestors (TI.VFloat float_)) (String.toFloat stringSubmittedByUser)

        TI.ChangeInt ancestors stringSubmittedByUser ->
            Maybe.map (\int_ -> helper ancestors (TI.VInt int_)) (String.toInt stringSubmittedByUser)

        TI.ChangeString ancestors stringSubmittedByUser ->
            Just <| helper ancestors (TI.VString stringSubmittedByUser)

        TI.ItemAdd position ancestors ->
            case ancestors of
                ( ancestorValue, _ ) :: xs ->
                    Just <|
                        helper xs
                            (case ancestorValue of
                                TI.VList listDefinition list_ ->
                                    let
                                        newValue : Value
                                        newValue =
                                            case List.head list_ of
                                                Just (TI.VInt _) ->
                                                    TI.VInt (List.length list_ + 1)

                                                Just (TI.VFloat _) ->
                                                    TI.VFloat (toFloat (List.length list_ + 1))

                                                Just (TI.VTuple ( v1, _ )) ->
                                                    let
                                                        first : TI.Value
                                                        first =
                                                            case v1 of
                                                                TI.VFloat _ ->
                                                                    TI.VFloat (toFloat (List.length list_ + 1))

                                                                TI.VInt _ ->
                                                                    TI.VInt (List.length list_ + 1)

                                                                _ ->
                                                                    TI.VString (paddedId (List.length list_ + 1))
                                                    in
                                                    TI.VTuple ( first, defaultValue )

                                                _ ->
                                                    TI.VString (paddedId (List.length list_ + 1))
                                    in
                                    case position of
                                        TI.Bottom ->
                                            TI.VList listDefinition (list_ ++ [ newValue ])

                                        TI.Top ->
                                            TI.VList listDefinition (newValue :: list_)

                                _ ->
                                    ancestorValue
                            )

                [] ->
                    Nothing

        TI.ItemDelete ancestors ->
            case ancestors of
                ( ancestorValue, ancestorkey ) :: xs ->
                    Just <|
                        helper xs
                            (case ( ancestorValue, String.toInt ancestorkey ) of
                                ( TI.VList listDefinition list_, Just int_ ) ->
                                    TI.VList listDefinition (List.Extra.removeAt int_ list_)

                                _ ->
                                    ancestorValue
                            )

                [] ->
                    Nothing

        TI.Reverse ancestors ->
            case ancestors of
                ( ancestorValue, ancestorkey ) :: xs ->
                    Just <|
                        helper xs
                            (case ( ancestorValue, ancestorkey ) of
                                ( TI.VList listDefinition list_, _ ) ->
                                    TI.VList listDefinition (list_ |> List.reverse)

                                _ ->
                                    ancestorValue
                            )

                [] ->
                    Nothing

        TI.SortDown ancestors ->
            case ancestors of
                ( ancestorValue, ancestorkey ) :: xs ->
                    Just <|
                        helper xs
                            (case ( ancestorValue, ancestorkey ) of
                                ( TI.VList listDefinition list_, _ ) ->
                                    TI.VList listDefinition (sortList list_)

                                _ ->
                                    ancestorValue
                            )

                [] ->
                    Nothing

        TI.SortUp ancestors ->
            case ancestors of
                ( ancestorValue, ancestorkey ) :: xs ->
                    Just <|
                        helper xs
                            (case ( ancestorValue, ancestorkey ) of
                                ( TI.VList listDefinition list_, _ ) ->
                                    TI.VList listDefinition
                                        (list_ |> sortList |> List.reverse)

                                _ ->
                                    ancestorValue
                            )

                [] ->
                    Nothing


paddedId : Int -> String
paddedId int_ =
    "_" ++ String.padLeft 3 '0' (String.fromInt int_) ++ "_"


{-| -}
string : Transformer String
string =
    TI.Transformer
        { decoder =
            \value ->
                case value of
                    TI.VString string_ ->
                        string_

                    _ ->
                        ""
        , encoder = TI.VString
        }


{-| -}
bytes : Transformer Bytes.Bytes
bytes =
    TI.Transformer
        { decoder =
            \value ->
                case value of
                    TI.VBytes bytes_ ->
                        bytes_

                    _ ->
                        Bytes.Encode.encode <| Bytes.Encode.string "💙"
        , encoder = TI.VBytes
        }


{-| -}
char : Transformer Char
char =
    TI.Transformer
        { decoder =
            \value ->
                case value of
                    TI.VChar char_ ->
                        char_

                    _ ->
                        Char.fromCode 0
        , encoder = TI.VChar
        }


{-| -}
int : Transformer Int
int =
    TI.Transformer
        { decoder =
            \value ->
                case value of
                    TI.VInt int_ ->
                        int_

                    _ ->
                        0
        , encoder = TI.VInt
        }


{-| -}
float : Transformer Float
float =
    TI.Transformer
        { decoder =
            \value ->
                case value of
                    TI.VFloat float_ ->
                        float_

                    _ ->
                        0
        , encoder = TI.VFloat
        }


{-| -}
bool : Transformer Bool
bool =
    TI.Transformer
        { decoder =
            \value ->
                case value of
                    TI.VBool bool_ ->
                        bool_

                    _ ->
                        False
        , encoder = TI.VBool
        }


listWithDefinition : TI.ListDefinition -> Transformer a -> Transformer (List a)
listWithDefinition listDefinition (TI.Transformer converterA) =
    TI.Transformer
        { decoder =
            \value ->
                case value of
                    TI.VList _ list_ ->
                        List.map converterA.decoder list_

                    _ ->
                        []
        , encoder = \list_ -> TI.VList listDefinition (List.map converterA.encoder list_)
        }


{-| -}
listWithName : String -> Transformer a -> Transformer (List a)
listWithName name =
    listWithDefinition { isDictOrSet = False, name = name }


{-| -}
list : Transformer a -> Transformer (List a)
list =
    listWithName "List"


{-| -}
dictWithName : String -> Transformer comparable -> Transformer b -> Transformer (Dict.Dict comparable b)
dictWithName name ta tb =
    map
        Dict.fromList
        Dict.toList
        (listWithDefinition { isDictOrSet = True, name = name } (tuple ( ta, tb )))


{-| -}
dict : Transformer comparable -> Transformer b -> Transformer (Dict.Dict comparable b)
dict =
    dictWithName "Dict"


{-| -}
setWithName : String -> Transformer comparable -> Transformer (Set.Set comparable)
setWithName name ta =
    map
        Set.fromList
        Set.toList
        (listWithDefinition { isDictOrSet = True, name = name } ta)


{-| -}
set : Transformer comparable -> Transformer (Set.Set comparable)
set =
    setWithName "Set"


{-| -}
arrayWithName : String -> Transformer a -> Transformer (Array.Array a)
arrayWithName name ta =
    map
        Array.fromList
        Array.toList
        (listWithDefinition { isDictOrSet = False, name = name } ta)


{-| -}
array : Transformer a -> Transformer (Array.Array a)
array =
    arrayWithName "Array"


{-| -}
tuple : ( Transformer a, Transformer b ) -> Transformer ( a, b )
tuple ( TI.Transformer converterA, TI.Transformer converterB ) =
    TI.Transformer
        { decoder =
            \value ->
                case value of
                    TI.VTuple ( a_, b_ ) ->
                        ( converterA.decoder a_, converterB.decoder b_ )

                    _ ->
                        ( converterA.decoder (TI.VString (paddedId 1)), converterB.decoder defaultValue )
        , encoder = \( a, b ) -> TI.VTuple ( converterA.encoder a, converterB.encoder b )
        }


{-| -}
defaultValue : Value
defaultValue =
    TI.VBool False


{-| -}
triple : ( Transformer a, Transformer b, Transformer c ) -> Transformer ( a, b, c )
triple ( TI.Transformer converterA, TI.Transformer converterB, TI.Transformer converterC ) =
    TI.Transformer
        { decoder =
            \value ->
                case value of
                    TI.VTriple ( a, b, c ) ->
                        ( converterA.decoder a, converterB.decoder b, converterC.decoder c )

                    _ ->
                        ( converterA.decoder defaultValue, converterB.decoder defaultValue, converterC.decoder defaultValue )
        , encoder = \( a, b, c ) -> TI.VTriple ( converterA.encoder a, converterB.encoder b, converterC.encoder c )
        }


{-| -}
maybe : Transformer a -> Transformer (Maybe a)
maybe transformerA =
    let
        all : List (Maybe a)
        all =
            [ Just (decode transformerA (encode bool False))
            , Nothing
            ]

        encoder : Maybe a -> TI.CustomTypeAsTuple
        encoder =
            \maybeA ->
                case maybeA of
                    Just a ->
                        ( "Just", [ encode transformerA a ] )

                    Nothing ->
                        ( "Nothing", [] )

        decoder : TI.CustomTypeAsTuple -> Maybe a
        decoder =
            \value ->
                case value of
                    ( "Just", p1 :: _ ) ->
                        Just (decode transformerA p1)

                    _ ->
                        Nothing
    in
    helperForCustomTypes
        { all = all
        , decoder = decoder
        , encoder = encoder
        , name = Just "Maybe"
        }


{-| -}
result : Transformer err -> Transformer ok -> Transformer (Result err ok)
result transformerErr transformerOk =
    let
        all : List (Result err ok)
        all =
            [ Ok (decode transformerOk (encode bool False))
            , Err (decode transformerErr (encode bool False))
            ]

        encoder : Result err ok -> TI.CustomTypeAsTuple
        encoder =
            \result_ ->
                case result_ of
                    Err err ->
                        ( "Err", [ encode transformerErr err ] )

                    Ok ok ->
                        ( "Ok", [ encode transformerOk ok ] )

        decoder : TI.CustomTypeAsTuple -> Result err ok
        decoder =
            \value ->
                case value of
                    ( "Ok", p1 :: _ ) ->
                        Ok (decode transformerOk p1)

                    ( "Err", p1 :: _ ) ->
                        Err (decode transformerErr p1)

                    _ ->
                        Err (decode transformerErr (encode bool False))
    in
    helperForCustomTypes
        { all = all
        , decoder = decoder
        , encoder = encoder
        , name = Just "Result"
        }


{-| -}
map : (a -> b) -> (b -> a) -> Transformer a -> Transformer b
map atob btoa (TI.Transformer transformerA) =
    TI.Transformer
        { decoder = \value -> atob (transformerA.decoder value)
        , encoder = \b -> transformerA.encoder (btoa b)
        }


{-| -}
field : Transformer a -> String -> Dict.Dict String Value -> a
field (TI.Transformer converterA) key dict_ =
    case Dict.get key dict_ of
        Just value ->
            converterA.decoder value

        _ ->
            converterA.decoder (TI.VInt 0)


{-| -}
encode : Transformer a -> (a -> Value)
encode (TI.Transformer transformer) =
    transformer.encoder


{-| -}
decode : Transformer a -> (Value -> a)
decode (TI.Transformer transformer) =
    transformer.decoder


{-| -}
helperForRecords :
    { decoder : TI.RecordAsDict -> a
    , encoder : a -> TI.RecordAsDict
    , name : Maybe String
    }
    -> Transformer a
helperForRecords args =
    TI.Transformer
        { decoder =
            \value ->
                case value of
                    TI.VRecord _ dict_ ->
                        args.decoder dict_

                    _ ->
                        args.decoder Dict.empty
        , encoder = \a -> TI.VRecord args.name (args.encoder a)
        }


{-| -}
helperForCustomTypes :
    { all : List a
    , decoder : TI.CustomTypeAsTuple -> a
    , encoder : a -> TI.CustomTypeAsTuple
    , name : Maybe String
    }
    -> Transformer a
helperForCustomTypes args =
    TI.Transformer
        { decoder =
            \value ->
                case value of
                    TI.VCustom _ customType ->
                        args.decoder customType

                    _ ->
                        args.decoder ( "", [] )
        , encoder = \a -> TI.VCustom ( args.name, List.map args.encoder args.all ) (args.encoder a)
        }
