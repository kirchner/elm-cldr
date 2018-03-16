module Data.Delimiters exposing (Delimiters, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias Delimiters =
    { quotationStart : String
    , quotationEnd : String
    , alternateQuotationStart : String
    , alternateQuotationEnd : String
    }


decoder : Decoder Delimiters
decoder =
    Decode.succeed Delimiters
        |> Decode.required "quotationStart" Decode.string
        |> Decode.required "quotationEnd" Decode.string
        |> Decode.required "alternateQuotationStart" Decode.string
        |> Decode.required "alternateQuotationEnd" Decode.string
