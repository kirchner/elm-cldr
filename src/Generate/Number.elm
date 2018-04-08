module Generate.Number exposing (generate)

import Data.Function exposing (Function(Exposed, Internal))
import Data.Numbers exposing (..)
import Data.Printer as Printer exposing (Printer, PrinterType)
import Dict exposing (Dict)
import Generate.Helper as Generate
import String.Extra as String


generate :
    String
    -> Dict String NumberingSystemData
    -> Dict String Symbols
    -> Dict String DecimalFormats
    -> Dict String ScientificFormats
    -> Dict String PercentFormats
    -> Dict String CurrencyFormats
    -> ( List Function, Dict String Printer )
generate module_ numberingSystems symbols decimalFormats scientificFormats percentFormats currencyFormats =
    let
        generateFormat numberSystem names format =
            ( generateNumberPrinter module_ numberingSystems numberSystem names format
            , generateNumberFormat names format
            )
    in
    [ decimalFormats
        |> Dict.map
            (\numberSystem decimalFormats ->
                generateFormat numberSystem
                    [ "decimal", numberSystem, "standard" ]
                    decimalFormats.standard
            )
        |> Dict.values
    , scientificFormats
        |> Dict.map
            (\numberSystem scientificFormats ->
                generateFormat numberSystem
                    [ "scientific", numberSystem, "standard" ]
                    scientificFormats.standard
            )
        |> Dict.values
    , percentFormats
        |> Dict.map
            (\numberSystem percentFormats ->
                generateFormat numberSystem
                    [ "percent", numberSystem, "standard" ]
                    percentFormats.standard
            )
        |> Dict.values
    , currencyFormats
        |> Dict.map
            (\numberSystem currencyFormats ->
                [ generateFormat numberSystem
                    [ "currency", numberSystem, "standard" ]
                    currencyFormats.standard
                , generateFormat numberSystem
                    [ "currency", numberSystem, "accounting" ]
                    currencyFormats.accounting
                ]
            )
        |> Dict.values
        |> List.concat
    ]
        |> List.concat
        |> List.foldr
            (\( ( printerFunction, ( printerName, printer ) ), formatFunction ) ( functions, printers ) ->
                ( printerFunction :: formatFunction :: functions
                , Dict.insert printerName printer printers
                )
            )
            ( [], Dict.empty )
        |> Tuple.mapFirst
            (\functions ->
                (symbols
                    |> Dict.map
                        (\numberSystem symbols ->
                            generateNumberSymbols numberSystem symbols
                        )
                    |> Dict.values
                )
                    ++ functions
            )


generateNumberPrinter :
    String
    -> Dict String NumberingSystemData
    -> String
    -> List String
    -> NumberFormat
    -> ( Function, ( String, Printer ) )
generateNumberPrinter module_ numberingSystems numberSystem names numberFormat =
    let
        name =
            names
                |> String.join "-"
                |> String.camelize

        digitSymbols =
            case Dict.get numberSystem numberingSystems of
                Just (Numeric { digits }) ->
                    digits

                _ ->
                    []
    in
    ( Exposed
        { name = name
        , imports =
            [ "import Printer.Number as Number"
            , "import Data.Numbers exposing (NumberFormat)"
            , "import Text exposing (FloatPrinter, s, floatPrinter)"
            ]
        , implementation =
            [ "{-| -}"
            , Generate.function
                { name = name
                , arguments = []
                , returnType = "FloatPrinter args msg"
                , body =
                    [ "floatPrinter"
                    , [ "(\\float ->"
                      , "s (Number.print "
                      , numberSystem ++ "NumberSymbols"
                      , [ "[ "
                        , digitSymbols
                            |> List.map (\char -> "'" ++ String.fromList [ char ] ++ "'")
                            |> String.join ", "
                        , " ]"
                        ]
                            |> String.concat
                      , name ++ "NumberFormat"
                      , "float))"
                      ]
                        |> String.join " "
                    , [ "(Number.floatInfo"
                      , name ++ "NumberFormat)"
                      ]
                        |> String.join " "
                    ]
                        |> String.join "\n"
                }
            ]
                |> String.join "\n"
        }
    , ( name
      , { module_ = module_
        , type_ = Printer.Float
        }
      )
    )


generateNumberFormat : List String -> NumberFormat -> Function
generateNumberFormat names numberFormat =
    let
        numberPattern pattern =
            [ ( "prefix"
              , pattern.prefix
                    |> Generate.string
              )
            , ( "suffix"
              , pattern.suffix
                    |> Generate.string
              )
            , ( "primaryGroupingSize"
              , pattern.primaryGroupingSize
                    |> toString
              )
            , ( "secondaryGroupingSize"
              , pattern.secondaryGroupingSize
                    |> toString
              )
            , ( "minimalIntegerCount"
              , pattern.minimalIntegerCount
                    |> toString
              )
            , ( "minimalFractionCount"
              , pattern.minimalFractionCount
                    |> toString
              )
            , ( "maximalFractionCount"
              , pattern.maximalFractionCount
                    |> toString
              )
            ]
                |> Generate.record

        name =
            names
                |> String.join "-"
                |> String.camelize
    in
    Internal
        { imports = []
        , implementation =
            [ name ++ "NumberFormat : NumberFormat\n"
            , name ++ "NumberFormat =\n"
            , [ ( "positivePattern", numberPattern numberFormat.positivePattern )
              , ( "negativePattern"
                , case numberFormat.negativePattern of
                    Just negativePattern ->
                        [ "Just ("
                        , numberPattern negativePattern
                        , ")"
                        ]
                            |> String.concat

                    Nothing ->
                        "Nothing"
                )
              ]
                |> Generate.record
                |> Generate.indent
            ]
                |> String.concat
        }


generateNumberSymbols : String -> Symbols -> Function
generateNumberSymbols numberSystem symbols =
    let
        name =
            numberSystem ++ "NumberSymbols"
    in
    Internal
        { imports =
            [ "import Data.Numbers exposing (Symbols)" ]
        , implementation =
            [ name ++ " : Symbols\n"
            , name ++ " =\n"
            , [ ( "decimal", symbols.decimal )
              , ( "group", symbols.group )
              , ( "list", symbols.list )
              , ( "percentSign", symbols.percentSign )
              , ( "plusSign", symbols.plusSign )
              , ( "minusSign", symbols.minusSign )
              , ( "exponential", symbols.exponential )
              , ( "superscriptingExponent", symbols.superscriptingExponent )
              , ( "perMille", symbols.perMille )
              , ( "infinity", symbols.infinity )
              , ( "nan", symbols.nan )
              , ( "timeSeparator", symbols.timeSeparator )
              ]
                |> List.map (Tuple.mapSecond Generate.string)
                |> Generate.record
                |> Generate.indent
            ]
                |> String.concat
        }
