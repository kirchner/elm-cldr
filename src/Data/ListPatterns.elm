module Data.ListPatterns exposing (ListPattern, ListPatterns, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias ListPatterns =
    { standard : ListPattern
    , or : ListPattern
    , standardShort : ListPattern
    , unit : ListPattern
    , unitNarrow : ListPattern
    , unitShort : ListPattern
    }


type alias ListPattern =
    { start : String
    , middle : String
    , end : String
    , two : String
    }


decoder : Decoder ListPatterns
decoder =
    Decode.succeed ListPatterns
        |> Decode.required "listPattern-type-standard" listPatternDecoder
        |> Decode.required "listPattern-type-or" listPatternDecoder
        |> Decode.required "listPattern-type-standard-short" listPatternDecoder
        |> Decode.required "listPattern-type-unit" listPatternDecoder
        |> Decode.required "listPattern-type-unit-narrow" listPatternDecoder
        |> Decode.required "listPattern-type-unit-short" listPatternDecoder


listPatternDecoder : Decoder ListPattern
listPatternDecoder =
    Decode.succeed ListPattern
        |> Decode.required "start" Decode.string
        |> Decode.required "middle" Decode.string
        |> Decode.required "end" Decode.string
        |> Decode.required "2" Decode.string
