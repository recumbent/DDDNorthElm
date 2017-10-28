module Aisle exposing (..)

import Json.Decode as Json


type Aisle
    = None
    | Number Int


intToAisle : number -> Aisle
intToAisle i =
    case i of
        0 ->
            None

        _ ->
            Number i


intToAisleDec : number -> Json.Decoder Aisle
intToAisleDec i =
    Json.succeed (intToAisle i)


aisleDecoder : Json.Decoder Aisle
aisleDecoder =
    Json.int
        |> Json.andThen intToAisleDec


aisleCompare : Aisle -> Aisle -> Order
aisleCompare a b =
    case a of
        None ->
            case b of
                None ->
                    EQ

                _ ->
                    LT

        Number an ->
            case b of
                None ->
                    GT

                Number bn ->
                    compare an bn
