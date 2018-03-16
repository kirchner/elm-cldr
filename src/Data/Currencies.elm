module Data.Currencies exposing (Currencies, decoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias Currencies =
    Dict String Currency


type alias Currency =
    { displayName : String
    , displayNameCountOne : Maybe String
    , displayNameCountOther : Maybe String
    , symbol : String
    }


decoder : Decoder Currencies
decoder =
    Decode.dict currencyDecoder


currencyDecoder : Decoder Currency
currencyDecoder =
    Decode.succeed Currency
        |> Decode.required "displayName" Decode.string
        |> Decode.optional "displayName-count-one" (Decode.map Just Decode.string) Nothing
        |> Decode.optional "displayName-count-other" (Decode.map Just Decode.string) Nothing
        |> Decode.required "symbol" Decode.string
