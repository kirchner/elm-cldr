module Data.Numbers
    exposing
        ( CurrencyFormats
        , DecimalFormat
        , DecimalFormats
        , MinimalPairs
        , NumberFormat
        , NumberingSystem
            ( Arab
            , Arabext
            , Armn
            , Beng
            , Cakm
            , Cyrl
            , Deva
            , Ethi
            , Geor
            , Grek
            , Gujr
            , Guru
            , Hanidec
            , Hans
            , Hant
            , Hebr
            , Jpan
            , Khmr
            , Knda
            , Laoo
            , Latn
            , Mlym
            , Mymr
            , Orya
            , Taml
            , Tamldec
            , Telu
            , Thai
            , Tibt
            , Vaii
            )
        , Numbers
        , PercentFormats
        , ScientificFormats
        , Symbols
        , decoder
        )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias Numbers =
    { defaultNumberingSystem : NumberingSystem
    , nativeNumberingSystem : NumberingSystem
    , traditionalNumberingSystem : Maybe NumberingSystem
    , minimumGroupingDigits : Int
    , symbols : Dict String Symbols
    , decimalFormats : Dict String DecimalFormats
    , scientificFormats : Dict String ScientificFormats
    , percentFormats : Dict String PercentFormats
    , currencyFormats : Dict String CurrencyFormats
    , minimalPairs : MinimalPairs
    }


type NumberingSystem
    = Arab
    | Arabext
    | Armn
    | Beng
    | Cakm
    | Cyrl
    | Deva
    | Ethi
    | Geor
    | Grek
    | Gujr
    | Guru
    | Hanidec
    | Hans
    | Hant
    | Hebr
    | Jpan
    | Khmr
    | Knda
    | Laoo
    | Latn
    | Mlym
    | Mymr
    | Orya
    | Taml
    | Tamldec
    | Telu
    | Thai
    | Tibt
    | Vaii


type alias Symbols =
    { decimal : String
    , group : String
    , list : String
    , percentSign : String
    , plusSign : String
    , minusSign : String
    , exponential : String
    , superscriptingExponent : String
    , perMille : String
    , infinity : String
    , nan : String
    , timeSeparator : String
    }


type alias DecimalFormats =
    { standard : NumberFormat
    , long : DecimalFormat
    , short : DecimalFormat
    }


type alias DecimalFormat =
    { for1000 : DecimalPluralForms
    , for10000 : DecimalPluralForms
    , for100000 : DecimalPluralForms
    , for1000000 : DecimalPluralForms
    , for10000000 : DecimalPluralForms
    , for100000000 : DecimalPluralForms
    , for1000000000 : DecimalPluralForms
    , for10000000000 : DecimalPluralForms
    , for100000000000 : DecimalPluralForms
    , for1000000000000 : DecimalPluralForms
    , for10000000000000 : DecimalPluralForms
    , for100000000000000 : DecimalPluralForms
    }


type alias DecimalPluralForms =
    { zero : Maybe String
    , one : Maybe String
    , two : Maybe String
    , few : Maybe String
    , many : Maybe String
    , other : String
    }


type alias ScientificFormats =
    { standard : NumberFormat
    }


type alias PercentFormats =
    { standard : NumberFormat
    }


type alias CurrencyFormats =
    { standard : NumberFormat
    , accounting : NumberFormat
    , short : DecimalFormat
    , unitPatternCountOne : String
    , unitPatternCountOther : String
    }


type alias MinimalPairs =
    { pluralMinimalPairsCountOne : Maybe String
    , pluralMinimalPairsCountOther : String
    , zero : Maybe String
    , one : Maybe String
    , two : Maybe String
    , few : Maybe String
    , many : Maybe String
    , other : String
    }



---- NUMBER FORMAT


type alias NumberFormat =
    { positivePattern : NumberPattern
    , negativePattern : Maybe NumberPattern
    }


type alias NumberPattern =
    { prefix : String
    , suffix : String
    , primaryGroupingSize : Maybe Int
    , secondaryGroupingSize : Maybe Int
    , minimalIntegerCount : Int
    , minimalFractionCount : Int
    , maximalFractionCount : Int
    }


parseNumberFormat : String -> Result String NumberFormat
parseNumberFormat pattern =
    case String.split ";" pattern of
        positivePattern :: [] ->
            parseNumberPattern positivePattern
                |> Result.map
                    (\positivePattern ->
                        { positivePattern = positivePattern
                        , negativePattern = Nothing
                        }
                    )

        positivePattern :: negativePattern :: [] ->
            Result.map2
                (\positivePattern negativePattern ->
                    { positivePattern = positivePattern
                    , negativePattern = Just negativePattern
                    }
                )
                (parseNumberPattern positivePattern)
                (parseNumberPattern negativePattern)

        _ ->
            Err "bad number format"


parseNumberPattern : String -> Result String NumberPattern
parseNumberPattern pattern =
    case pattern |> String.split "." of
        integerPattern :: [] ->
            let
                ( primaryGroupingSize, secondaryGroupingSize ) =
                    case integerPattern |> String.split "," of
                        firstGroup :: [] ->
                            ( Nothing, Nothing )

                        firstGroup :: secondGroup :: [] ->
                            ( secondGroup
                                |> String.length
                                |> Just
                            , Nothing
                            )

                        firstGroup :: secondGroup :: thirdGroup :: [] ->
                            ( thirdGroup
                                |> String.length
                                |> Just
                            , secondGroup
                                |> String.length
                                |> Just
                            )

                        _ ->
                            Debug.crash "their is an error in your number pattern"
            in
            { prefix = ""
            , suffix = ""
            , primaryGroupingSize = primaryGroupingSize
            , secondaryGroupingSize = secondaryGroupingSize
            , minimalIntegerCount =
                integerPattern
                    |> String.filter (\char -> char == '0')
                    |> String.length
            , minimalFractionCount = 0
            , maximalFractionCount = 0
            }
                |> Ok

        integerPattern :: fractionalPattern :: [] ->
            let
                ( primaryGroupingSize, secondaryGroupingSize ) =
                    case integerPattern |> String.split "," of
                        firstGroup :: [] ->
                            ( Nothing, Nothing )

                        firstGroup :: secondGroup :: [] ->
                            ( secondGroup
                                |> String.length
                                |> Just
                            , Nothing
                            )

                        firstGroup :: secondGroup :: thirdGroup :: [] ->
                            ( thirdGroup
                                |> String.length
                                |> Just
                            , secondGroup
                                |> String.length
                                |> Just
                            )

                        _ ->
                            Debug.crash "their is an error in your number pattern"
            in
            { prefix = ""
            , suffix = ""
            , primaryGroupingSize = primaryGroupingSize
            , secondaryGroupingSize = secondaryGroupingSize
            , minimalIntegerCount =
                integerPattern
                    |> String.filter (\char -> char == '0')
                    |> String.length
            , minimalFractionCount =
                fractionalPattern
                    |> String.filter (\char -> char == '0')
                    |> String.length
            , maximalFractionCount =
                fractionalPattern
                    |> String.length
            }
                |> Ok

        _ ->
            Err "bad pattern"



---- DECODER


decoder : Decoder Numbers
decoder =
    Decode.succeed Numbers
        |> Decode.required "defaultNumberingSystem" numberingSystemDecoder
        |> Decode.requiredAt [ "otherNumberingSystems", "native" ] numberingSystemDecoder
        |> Decode.optionalAt [ "otherNumberingSystems", "traditional" ]
            (numberingSystemDecoder |> Decode.map Just)
            Nothing
        |> Decode.required "minimumGroupingDigits" stringIntDecoder
        |> decodeForNumberSystems "symbols" symbolsDecoder
        |> decodeForNumberSystems "decimalFormats" decimalFormatsDecoder
        |> decodeForNumberSystems "scientificFormats" scientificFormatsDecoder
        |> decodeForNumberSystems "percentFormats" percentFormatsDecoder
        |> decodeForNumberSystems "currencyFormats" currencyFormatsDecoder
        |> Decode.required "minimalPairs" minimalPairsDecoder


numberingSystemDecoder : Decoder NumberingSystem
numberingSystemDecoder =
    Decode.string
        |> Decode.andThen
            (\numberingSystem ->
                case numberingSystem of
                    "arab" ->
                        Decode.succeed Arab

                    "arabext" ->
                        Decode.succeed Arabext

                    "armn" ->
                        Decode.succeed Armn

                    "beng" ->
                        Decode.succeed Beng

                    "cakm" ->
                        Decode.succeed Cakm

                    "cyrl" ->
                        Decode.succeed Cyrl

                    "deva" ->
                        Decode.succeed Deva

                    "ethi" ->
                        Decode.succeed Ethi

                    "geor" ->
                        Decode.succeed Geor

                    "grek" ->
                        Decode.succeed Grek

                    "gujr" ->
                        Decode.succeed Gujr

                    "guru" ->
                        Decode.succeed Guru

                    "hanidec" ->
                        Decode.succeed Hanidec

                    "hans" ->
                        Decode.succeed Hans

                    "hant" ->
                        Decode.succeed Hant

                    "hebr" ->
                        Decode.succeed Hebr

                    "jpan" ->
                        Decode.succeed Jpan

                    "khmr" ->
                        Decode.succeed Khmr

                    "knda" ->
                        Decode.succeed Knda

                    "laoo" ->
                        Decode.succeed Laoo

                    "latn" ->
                        Decode.succeed Latn

                    "mlym" ->
                        Decode.succeed Mlym

                    "mymr" ->
                        Decode.succeed Mymr

                    "orya" ->
                        Decode.succeed Orya

                    "taml" ->
                        Decode.succeed Taml

                    "tamldec" ->
                        Decode.succeed Tamldec

                    "telu" ->
                        Decode.succeed Telu

                    "thai" ->
                        Decode.succeed Thai

                    "tibt" ->
                        Decode.succeed Tibt

                    "vaii" ->
                        Decode.succeed Vaii

                    _ ->
                        Decode.fail (numberingSystem ++ " is not a valid numberingSystem")
            )


symbolsDecoder : Decoder Symbols
symbolsDecoder =
    Decode.succeed Symbols
        |> Decode.required "decimal" Decode.string
        |> Decode.required "group" Decode.string
        |> Decode.required "list" Decode.string
        |> Decode.required "percentSign" Decode.string
        |> Decode.required "plusSign" Decode.string
        |> Decode.required "minusSign" Decode.string
        |> Decode.required "exponential" Decode.string
        |> Decode.required "superscriptingExponent" Decode.string
        |> Decode.required "perMille" Decode.string
        |> Decode.required "infinity" Decode.string
        |> Decode.required "nan" Decode.string
        |> Decode.required "timeSeparator" Decode.string


decimalFormatsDecoder : Decoder DecimalFormats
decimalFormatsDecoder =
    Decode.succeed DecimalFormats
        |> Decode.required "standard" numberFormatDecoder
        |> Decode.requiredAt [ "long", "decimalFormat" ] decimalFormatDecoder
        |> Decode.requiredAt [ "short", "decimalFormat" ] decimalFormatDecoder


numberFormatDecoder : Decoder NumberFormat
numberFormatDecoder =
    Decode.string
        |> Decode.andThen
            (\rawNumberFormat ->
                case parseNumberFormat rawNumberFormat of
                    Ok numberFormat ->
                        Decode.succeed numberFormat

                    Err error ->
                        Decode.fail error
            )


decimalFormatDecoder : Decoder DecimalFormat
decimalFormatDecoder =
    Decode.succeed DecimalFormat
        |> decimalPluralFormsDecoder "1000-count"
        |> decimalPluralFormsDecoder "10000-count"
        |> decimalPluralFormsDecoder "100000-count"
        |> decimalPluralFormsDecoder "1000000-count"
        |> decimalPluralFormsDecoder "10000000-count"
        |> decimalPluralFormsDecoder "100000000-count"
        |> decimalPluralFormsDecoder "1000000000-count"
        |> decimalPluralFormsDecoder "10000000000-count"
        |> decimalPluralFormsDecoder "100000000000-count"
        |> decimalPluralFormsDecoder "1000000000000-count"
        |> decimalPluralFormsDecoder "10000000000000-count"
        |> decimalPluralFormsDecoder "100000000000000-count"


decimalPluralFormsDecoder count =
    Decode.custom
        (Decode.succeed DecimalPluralForms
            |> Decode.optional (count ++ "-zero") (Decode.map Just Decode.string) Nothing
            |> Decode.optional (count ++ "-one") (Decode.map Just Decode.string) Nothing
            |> Decode.optional (count ++ "-two") (Decode.map Just Decode.string) Nothing
            |> Decode.optional (count ++ "-few") (Decode.map Just Decode.string) Nothing
            |> Decode.optional (count ++ "-many") (Decode.map Just Decode.string) Nothing
            |> Decode.required (count ++ "-other") Decode.string
        )


scientificFormatsDecoder : Decoder ScientificFormats
scientificFormatsDecoder =
    Decode.succeed ScientificFormats
        |> Decode.required "standard" numberFormatDecoder


percentFormatsDecoder : Decoder PercentFormats
percentFormatsDecoder =
    Decode.succeed PercentFormats
        |> Decode.required "standard" numberFormatDecoder


currencyFormatsDecoder : Decoder CurrencyFormats
currencyFormatsDecoder =
    Decode.succeed CurrencyFormats
        |> Decode.required "standard" numberFormatDecoder
        |> Decode.required "accounting" numberFormatDecoder
        |> Decode.requiredAt [ "short", "standard" ] decimalFormatDecoder
        |> Decode.required "unitPattern-count-one" Decode.string
        |> Decode.required "unitPattern-count-other" Decode.string


minimalPairsDecoder : Decoder MinimalPairs
minimalPairsDecoder =
    Decode.succeed MinimalPairs
        |> Decode.optional "pluralMinimalPairs-count-one" (Decode.map Just Decode.string) Nothing
        |> Decode.required "pluralMinimalPairs-count-other" Decode.string
        |> Decode.optional "zero" (Decode.map Just Decode.string) Nothing
        |> Decode.optional "one" (Decode.map Just Decode.string) Nothing
        |> Decode.optional "two" (Decode.map Just Decode.string) Nothing
        |> Decode.optional "few" (Decode.map Just Decode.string) Nothing
        |> Decode.optional "many" (Decode.map Just Decode.string) Nothing
        |> Decode.required "other" Decode.string



---- HELPER


decodeForNumberSystems : String -> Decoder a -> Decoder (Dict String a -> b) -> Decoder b
decodeForNumberSystems prefix aDecoder funcDecoder =
    funcDecoder
        |> Decode.custom
            (numberSystems
                |> List.foldl
                    (\numberingSystem aDictDecoder ->
                        Decode.oneOf
                            [ Decode.field (prefix ++ "-numberSystem-" ++ numberingSystem) aDecoder
                                |> Decode.andThen
                                    (\a ->
                                        aDictDecoder
                                            |> Decode.map (Dict.insert numberingSystem a)
                                    )
                            , aDictDecoder
                            ]
                    )
                    (Decode.succeed Dict.empty)
            )


numberSystems : List String
numberSystems =
    [ "arab"
    , "arabext"
    , "armn"
    , "beng"
    , "cakm"
    , "cyrl"
    , "deva"
    , "ethi"
    , "geor"
    , "grek"
    , "gujr"
    , "guru"
    , "hanidec"
    , "hans"
    , "hant"
    , "hebr"
    , "jpan"
    , "khmr"
    , "knda"
    , "laoo"
    , "latn"
    , "mlym"
    , "mymr"
    , "orya"
    , "taml"
    , "tamldec"
    , "telu"
    , "thai"
    , "tibt"
    , "vaii"
    ]


stringIntDecoder : Decoder Int
stringIntDecoder =
    Decode.string
        |> Decode.andThen
            (\stringInt ->
                case String.toInt stringInt of
                    Ok int ->
                        Decode.succeed int

                    Err error ->
                        Decode.fail error
            )
