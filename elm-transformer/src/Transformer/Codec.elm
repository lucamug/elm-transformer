module Transformer.Codec exposing (codecValue)

import Bytes
import Codec
import Transformer.Internal as TI


codecValue : Codec.Codec TI.Value
codecValue =
    Codec.recursive <|
        \c ->
            Codec.custom
                (\v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 value ->
                    case value of
                        TI.VBool p1 ->
                            v2 p1

                        TI.VBytes p1 ->
                            v11 p1

                        TI.VChar p1 ->
                            v5 p1

                        TI.VCustom p1 p2 ->
                            v10 p1 p2

                        TI.VFloat p1 ->
                            v4 p1

                        TI.VInt p1 ->
                            v3 p1

                        TI.VList p1 p2 ->
                            v8 p1 p2

                        TI.VRecord p1 p2 ->
                            v9 p1 p2

                        TI.VString p1 ->
                            v1 p1

                        TI.VTriple p1 ->
                            v7 p1

                        TI.VTuple p1 ->
                            v6 p1
                )
                |> Codec.variant1 "string" TI.VString Codec.string
                |> Codec.variant1 "bool" TI.VBool Codec.bool
                |> Codec.variant1 "int" TI.VInt Codec.int
                |> Codec.variant1 "float" TI.VFloat Codec.float
                |> Codec.variant1 "char" TI.VChar Codec.char
                |> Codec.variant1 "tuple" TI.VTuple (Codec.tuple c c)
                |> Codec.variant1 "triple" TI.VTriple (Codec.triple c c c)
                |> Codec.variant2 "list" TI.VList codeListDefinition (Codec.list c)
                |> Codec.variant2 "record" TI.VRecord (Codec.maybe Codec.string) (Codec.dict c)
                |> Codec.variant2 "custom" TI.VCustom (codecCustomTypeDefinition c) (codecCustomType c)
                |> Codec.variant1 "bytes" TI.VBytes codecBytes
                |> Codec.buildCustom


codecBytes : Codec.Codec Bytes.Bytes
codecBytes =
    Codec.map
        TI.fromStringHexToBytes
        TI.fromBytesToStringHex
        Codec.string


codeListDefinition : Codec.Codec TI.ListDefinition
codeListDefinition =
    Codec.object
        (\v1 v2 ->
            { isDictOrSet = v2
            , name = v1
            }
        )
        |> Codec.field "name" .name Codec.string
        |> Codec.field "isDictOrSet" .isDictOrSet Codec.bool
        |> Codec.buildObject


codecCustomType : Codec.Codec TI.Value -> Codec.Codec TI.CustomTypeAsTuple
codecCustomType c =
    Codec.tuple Codec.string (Codec.list c)


codecCustomTypeDefinition : Codec.Codec TI.Value -> Codec.Codec TI.CustomTypeDefinition
codecCustomTypeDefinition c =
    Codec.tuple (Codec.maybe Codec.string) (Codec.list (codecCustomType c))
