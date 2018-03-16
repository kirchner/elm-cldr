module Generate.Number exposing (generate)

import Data.Numbers exposing (..)
import Dict exposing (Dict)
import Generate.Helper as Generate
import Misc exposing (Function)
import String.Extra as String


generate :
    Dict String Symbols
    -> Dict String DecimalFormats
    -> Dict String ScientificFormats
    -> Dict String PercentFormats
    -> Dict String CurrencyFormats
    -> List Function
generate symbols decimalFormats scientificFormats percentFormats currencyFormats =
    let
        generateFormat numberSystem names format =
            [ generateNumberPrinter numberSystem names format
            , generateNumberFormat names format
            ]
    in
    [ [ symbols
            |> Dict.map
                (\numberSystem symbols ->
                    generateNumberSymbols numberSystem symbols
                )
            |> Dict.values
      ]
    , decimalFormats
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
                    |> List.concat
            )
        |> Dict.values
    ]
        |> List.concat
        |> List.concat


generateNumberPrinter : String -> List String -> NumberFormat -> Function
generateNumberPrinter numberSystem names numberFormat =
    let
        name =
            names
                |> String.join "-"
                |> String.camelize
    in
    { export = Just name
    , imports =
        [ "import Printer.Number as Number"
        , "import Data.Numbers exposing (NumberFormat)"
        , "import Translation exposing (Printer, s, printer)"
        ]
    , implementation =
        [ "{-| -}"
        , Generate.function
            { name = name
            , arguments = []
            , returnType = "Printer Float args msg"
            , body =
                [ [ "printer"
                  , names
                        |> List.map Generate.string
                        |> Generate.listOneLine
                  , "<|"
                  ]
                    |> String.join " "
                , [ "\\float ->"
                  , [ "s (Number.print "
                    , numberSystem ++ "NumberSymbols"
                    , name ++ "NumberFormat"
                    , "float)"
                    ]
                        |> String.join " "
                  ]
                    |> String.join "\n"
                ]
                    |> String.join "\n"
            }
        ]
            |> String.join "\n"
    }


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
    { export = Nothing
    , imports = []
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
    { export = Nothing
    , imports =
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