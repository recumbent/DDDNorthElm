module Aisle exposing (..)

import Json.Decode as Json

type Aisle =
    None
    | Number Int

intToAisle : number -> Aisle
intToAisle i =
    case i of
        0 -> None
        _ -> Number i

intToAisleDec : number -> Json.Decoder Aisle
intToAisleDec i =
     Json.succeed (intToAisle i)

aisleDecoder : Json.Decoder Aisle
aisleDecoder = 
    Json.int
    |> Json.andThen intToAisleDec