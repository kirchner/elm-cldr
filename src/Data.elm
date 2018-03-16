module Data
    exposing
        ( Data
        , RawData
        , decode
        , rawDataDecoder
        )

import Char
import Data.Currencies as Currencies exposing (Currencies)
import Data.Delimiters as Delimiters exposing (Delimiters)
import Data.ListPatterns as ListPatterns exposing (ListPatterns)
import Data.Numbers as Numbers exposing (Numbers)
import Data.PluralRules as PluralRules exposing (PluralRules)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, decodeString)
import Json.Decode.Pipeline as Decode


type alias RawData =
    { main : Dict String RawLocaleData
    , cardinals : String
    , ordinals : String
    }


type alias RawLocaleData =
    { delimiters : String
    , listPatterns : String
    , numbers : String
    , currencies : String
    }


rawDataDecoder : Decoder RawData
rawDataDecoder =
    Decode.succeed RawData
        |> Decode.required "rawData" (Decode.dict rawLocaleDataDecoder)
        |> Decode.required "cardinals" Decode.string
        |> Decode.required "ordinals" Decode.string


rawLocaleDataDecoder : Decoder RawLocaleData
rawLocaleDataDecoder =
    Decode.succeed RawLocaleData
        |> Decode.required "delimiters" Decode.string
        |> Decode.required "listPatterns" Decode.string
        |> Decode.required "numbers" Decode.string
        |> Decode.required "currencies" Decode.string


type alias Data =
    { main : Dict (List String) LocaleData
    , cardinals : Dict String PluralRules
    , ordinals : Dict String PluralRules
    }


type alias LocaleData =
    { delimiters : Delimiters
    , listPatterns : ListPatterns
    , numbers : Numbers
    , currencies : Currencies
    }


decode : RawData -> Result String Data
decode rawData =
    Result.map3 Data
        (Dict.foldl
            (\localeCode { delimiters, listPatterns, numbers, currencies } result ->
                Ok LocaleData
                    |> decodeAt [ "main", localeCode, "delimiters" ] Delimiters.decoder delimiters
                    |> decodeAt [ "main", localeCode, "listPatterns" ] ListPatterns.decoder listPatterns
                    |> decodeAt [ "main", localeCode, "numbers" ] Numbers.decoder numbers
                    |> decodeAt [ "main", localeCode, "numbers", "currencies" ] Currencies.decoder currencies
                    |> Result.andThen
                        (\localeData ->
                            result
                                |> Result.map
                                    (Dict.insert
                                        (localeCode
                                            |> String.split "-"
                                            |> List.map
                                                (\element ->
                                                    if String.all Char.isDigit element then
                                                        "N" ++ element
                                                    else
                                                        element
                                                )
                                        )
                                        localeData
                                    )
                        )
            )
            (Ok Dict.empty)
            rawData.main
        )
        (rawData.cardinals
            |> Decode.decodeString
                (Decode.at [ "supplemental", "plurals-type-cardinal" ]
                    (Decode.dict PluralRules.decoder)
                )
        )
        (rawData.ordinals
            |> Decode.decodeString
                (Decode.at [ "supplemental", "plurals-type-ordinal" ]
                    (Decode.dict PluralRules.decoder)
                )
        )



---- HELPER


decodeAt : List String -> Decoder a -> String -> Result String (a -> data) -> Result String data
decodeAt fields aDecoder rawJson resultFunc =
    resultFunc
        |> Result.andThen
            (\func ->
                rawJson
                    |> decodeString (Decode.at fields aDecoder)
                    |> Result.map func
            )
