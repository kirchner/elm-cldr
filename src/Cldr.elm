module Cldr exposing (..)

{-| This module exposes localization and internationalization data of
the [common locale data repository](https://cldr.unicode.org/)
(CLDR).
-}

import Char
import Dict exposing (Dict)
import Generate
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Localized
    exposing
        ( NumberFormat
        , NumberPattern
        , NumberSymbols
        )
import Parser exposing (..)
import String.Extra as String


cldr : Dict String String -> String -> String -> Cldr
cldr numberFormatsJsons cardinalsJson ordinalsJson =
    let
        numberFormats =
            numberFormatsJsons
                |> Dict.toList
                |> List.map
                    (\( code, json ) ->
                        case json |> Decode.decodeString (numberFormatsDecoder code) of
                            Ok numberFormats ->
                                ( code |> String.camelize, numberFormats )

                            Err error ->
                                Debug.crash (toString error)
                    )
                |> Dict.fromList
    in
    case
        ( cardinalsJson |> Decode.decodeString cardinalRulesDecoder
        , ordinalsJson |> Decode.decodeString ordinalRulesDecoder
        )
    of
        ( Ok allCardinalRules, Ok allOrdinalRules ) ->
            { locales =
                allCardinalRules
                    |> Dict.map
                        (\code cardinalRules ->
                            let
                                defaultNumberFormats =
                                    case Dict.get "en" numberFormats of
                                        Just enNumberFormats ->
                                            enNumberFormats

                                        Nothing ->
                                            Debug.crash "en number formats not available"

                                saveNumberFormats =
                                    Dict.get code numberFormats
                                        |> Maybe.withDefault defaultNumberFormats
                            in
                            case Dict.get code allOrdinalRules of
                                Just ordinalRules ->
                                    { numberFormats = saveNumberFormats
                                    , cardinalRules = cardinalRules
                                    , ordinalRules = Just ordinalRules
                                    }

                                Nothing ->
                                    { numberFormats = saveNumberFormats
                                    , cardinalRules = cardinalRules
                                    , ordinalRules = Nothing
                                    }
                        )
            }

        _ ->
            Debug.crash "could not parse CLDR data"


type alias Cldr =
    { locales : Dict String Locale
    }


type alias Locale =
    { numberFormats : NumberFormats
    , cardinalRules : PluralRules
    , ordinalRules : Maybe PluralRules
    }


numberFormatsDecoder : String -> Decoder NumberFormats
numberFormatsDecoder code =
    Decode.decode NumberFormats
        |> Decode.requiredAt
            [ "main"
            , code
            , "numbers"
            , "symbols-numberSystem-latn"
            ]
            numberSymbolsDecoder
        |> Decode.requiredAt
            [ "main"
            , code
            , "numbers"
            , "decimalFormats-numberSystem-latn"
            , "standard"
            ]
            numberFormatDecoder
        |> Decode.requiredAt
            [ "main"
            , code
            , "numbers"
            , "percentFormats-numberSystem-latn"
            , "standard"
            ]
            numberFormatDecoder
        |> Decode.requiredAt
            [ "main"
            , code
            , "numbers"
            , "scientificFormats-numberSystem-latn"
            , "standard"
            ]
            numberFormatDecoder


numberSymbolsDecoder : Decoder NumberSymbols
numberSymbolsDecoder =
    Decode.decode NumberSymbols
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


numberFormatDecoder : Decoder NumberFormat
numberFormatDecoder =
    Decode.string
        |> Decode.andThen
            (\format ->
                case numberFormat format of
                    Ok format ->
                        Decode.succeed format

                    Err error ->
                        Decode.fail error
            )


cardinalRulesDecoder : Decoder (Dict String PluralRules)
cardinalRulesDecoder =
    Decode.at [ "supplemental", "plurals-type-cardinal" ]
        (Decode.dict pluralRulesDecoder)


ordinalRulesDecoder : Decoder (Dict String PluralRules)
ordinalRulesDecoder =
    Decode.at [ "supplemental", "plurals-type-ordinal" ]
        (Decode.dict pluralRulesDecoder)



---- NUMBERS


type alias NumberFormats =
    { symbols : NumberSymbols
    , standard : NumberFormat
    , percent : NumberFormat
    , scientific : NumberFormat
    }


numberFormat : String -> Result String NumberFormat
numberFormat pattern =
    case pattern |> String.split ";" of
        positivePattern :: [] ->
            case numberPattern positivePattern of
                Ok positivePattern ->
                    { positivePattern = positivePattern
                    , negativePattern = Nothing
                    }
                        |> Ok

                Err error ->
                    Err ("bad positive pattern: " ++ error)

        positivePattern :: negativePattern :: [] ->
            case
                ( numberPattern positivePattern
                , numberPattern negativePattern
                )
            of
                ( Ok positivePattern, Ok negativePattern ) ->
                    { positivePattern = positivePattern
                    , negativePattern = Just negativePattern
                    }
                        |> Ok

                _ ->
                    Err "bad positive or negative pattern"

        _ ->
            Err "bad number format"


numberPattern : String -> Result String NumberPattern
numberPattern pattern =
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



---- PLURAL RULE


type alias PluralRules =
    { zero : Maybe Or
    , one : Maybe Or
    , two : Maybe Or
    , few : Maybe Or
    , many : Maybe Or
    }


pluralRulesDecoder : Decoder PluralRules
pluralRulesDecoder =
    Decode.decode PluralRules
        |> maybeAt "pluralRule-count-zero" orDecoder
        |> maybeAt "pluralRule-count-one" orDecoder
        |> maybeAt "pluralRule-count-two" orDecoder
        |> maybeAt "pluralRule-count-few" orDecoder
        |> maybeAt "pluralRule-count-many" orDecoder


orDecoder : Decoder Or
orDecoder =
    Decode.string
        |> Decode.andThen
            (\pluralRule ->
                case run orParser pluralRule of
                    Ok rule ->
                        Decode.succeed rule

                    Err _ ->
                        Decode.fail "not a proper plural rule"
            )


maybeAt fieldName decoder =
    Decode.custom <|
        Decode.oneOf
            [ Decode.field fieldName (decoder |> Decode.map Just)
            , Decode.succeed Nothing
            ]


type Or
    = Or (List And)


type And
    = And (List Relation)


type Relation
    = Equal Expression (List Range)
    | NotEqual Expression (List Range)


type Expression
    = Simple PluralOperand
    | Modulo PluralOperand Int


type PluralOperand
    = AbsoluteValue
    | FractionDigits WithTrailingZeros
    | IntegerDigitCount
    | FractionDigitCount WithTrailingZeros


type WithTrailingZeros
    = WithTrailingZeros
    | WithoutTrailingZeros


type Range
    = Single Int
    | Range Int Int



---- CODE GEN


generateLocaleModule : String -> Locale -> { filename : String, content : String }
generateLocaleModule code locale =
    let
        sanitizedLocaleCode =
            code
                |> String.camelize
                |> String.toSentenceCase
    in
    { filename = sanitizedLocaleCode ++ ".elm"
    , content =
        [ Generate.moduIe
            { name = "Localized." ++ sanitizedLocaleCode
            , exposed =
                [ "cardinalSelector"
                , "ordinalSelector"
                , "cardinal"
                , "ordinal"
                , "decimal"
                ]
            }
        , [ "import Internal.Localized exposing (..)"
          , "import Localized exposing (..)"
          ]
            |> String.join "\n"
        , generateNumberFormats locale.numberFormats
        , generatePlural "cardinal" locale.cardinalRules
        , case locale.ordinalRules of
            Just ordinalRules ->
                generatePlural "ordinal" ordinalRules

            Nothing ->
                "ordinal = cardinal"
        , generateSelector "cardinal" locale.cardinalRules
        , case locale.ordinalRules of
            Just ordinalRules ->
                generateSelector "ordinal" ordinalRules

            Nothing ->
                "ordinalSelector = cardinalSelector"
        ]
            |> String.join "\n\n"
    }


generateNumberFormats : NumberFormats -> String
generateNumberFormats numberFormats =
    [ generateNumberSymbols numberFormats.symbols
    , generateNumberFormat "standard" numberFormats.standard
    , generateNumberFormat "percent" numberFormats.percent
    , generateNumberFormat "scientific" numberFormats.scientific
    , generateDecimal
    ]
        |> String.join "\n\n"


generateNumberFormat : String -> NumberFormat -> String
generateNumberFormat kind numberFormat =
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
    in
    [ kind ++ "NumberFormat : NumberFormat\n"
    , kind ++ "NumberFormat =\n"
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


generateNumberSymbols : NumberSymbols -> String
generateNumberSymbols symbols =
    [ "numberSymbols : NumberSymbols\n"
    , "numberSymbols =\n"
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


generateDecimal : String
generateDecimal =
    Generate.function
        { name = "decimal"
        , arguments = [ ( "(args -> Float)", "accessor" ) ]
        , returnType = "Part args"
        , body =
            "customDecimal accessor numberSymbols standardNumberFormat"
        }


generatePlural : String -> PluralRules -> String
generatePlural kind pluralRules =
    let
        body =
            [ "customPlural accessor"
            , [ "(toString >> " ++ kind ++ "Selector)"
              , pluralCasesAssignment
              ]
                |> String.concat
                |> Generate.indent
            ]
                |> String.join "\n"

        pluralCasesAssignment =
            [ "{"
            , [ pluralCaseAssignment "zero" pluralRules.zero
              , pluralCaseAssignment "one" pluralRules.one
              , pluralCaseAssignment "two" pluralRules.two
              , pluralCaseAssignment "few" pluralRules.few
              , pluralCaseAssignment "many" pluralRules.many
              , "other = other"
              ]
                |> String.join "\n ,"
            , "\n}"
            ]
                |> String.concat

        pluralCaseAssignment name rule =
            if rule == Nothing then
                name ++ " = []"
            else
                name ++ " = " ++ name

        pluralCasesType =
            [ "{"
            , [ pluralCaseType "zero" pluralRules.zero
              , pluralCaseType "one" pluralRules.one
              , pluralCaseType "two" pluralRules.two
              , pluralCaseType "few" pluralRules.few
              , pluralCaseType "many" pluralRules.many
              , Just "other : List (Part args)"
              ]
                |> List.filterMap identity
                |> String.join "\n ,"
            , "\n}"
            ]
                |> String.concat

        pluralCaseType name maybeRule =
            case maybeRule of
                Just _ ->
                    [ name
                    , " : List (Part args)"
                    ]
                        |> String.concat
                        |> Just

                Nothing ->
                    Nothing

        pluralCasesNames =
            [ "{"
            , [ pluralCaseName "zero" pluralRules.zero
              , pluralCaseName "one" pluralRules.one
              , pluralCaseName "two" pluralRules.two
              , pluralCaseName "few" pluralRules.few
              , pluralCaseName "many" pluralRules.many
              , Just "other"
              ]
                |> List.filterMap identity
                |> String.join " ,"
            , "}"
            ]
                |> String.concat

        pluralCaseName name maybeRule =
            case maybeRule of
                Just _ ->
                    Just name

                Nothing ->
                    Nothing
    in
    Generate.function
        { name = kind
        , arguments =
            [ ( "(args -> Float)", "accessor" )
            , ( pluralCasesType, pluralCasesNames )
            ]
        , returnType = "Part args"
        , body = body
        }


generateSelector : String -> PluralRules -> String
generateSelector kind pluralRules =
    let
        conditions pluralRules =
            [ pluralRules.zero |> Maybe.map (generateCondition "Zero")
            , pluralRules.one |> Maybe.map (generateCondition "One")
            , pluralRules.two |> Maybe.map (generateCondition "Two")
            , pluralRules.few |> Maybe.map (generateCondition "Few")
            , pluralRules.many |> Maybe.map (generateCondition "Many")
            ]
                |> List.filterMap identity

        generateBody pluralRules =
            case conditions pluralRules of
                [] ->
                    "Other"

                _ ->
                    conditions pluralRules
                        |> Generate.ifThenElseChain "Other"

        generateCondition pluralType pluralRule =
            ( generatePluralRule pluralRule, pluralType )
    in
    Generate.function
        { name = kind ++ "Selector"
        , arguments = [ ( "String", "count" ) ]
        , returnType = "PluralCase"
        , body = generateBody pluralRules
        }


generatePluralRule : Or -> String
generatePluralRule (Or ands) =
    ands
        |> List.map generateAnd
        |> String.join "\n|| "


generateAnd : And -> String
generateAnd (And relations) =
    [ "(\n"
    , relations
        |> List.map generateRelation
        |> String.join "\n&& "
        |> Generate.indent
    , "\n)"
    ]
        |> String.concat


generateRelation : Relation -> String
generateRelation relation =
    case relation of
        Equal expression ranges ->
            ranges
                |> List.map
                    (\range ->
                        case range of
                            Single int ->
                                [ "("
                                , generateExpression expression
                                , " == "
                                , toString int
                                , ")"
                                ]
                                    |> String.concat

                            Range a b ->
                                [ "("
                                , [ "("
                                  , generateExpression expression
                                  , " < "
                                  , toString a
                                  , ")"
                                  ]
                                    |> String.concat
                                , " && "
                                , [ "("
                                  , generateExpression expression
                                  , " > "
                                  , toString b
                                  , ")"
                                  ]
                                    |> String.concat
                                , ")"
                                ]
                                    |> String.concat
                    )
                |> String.join " && "

        NotEqual expression ranges ->
            ranges
                |> List.map
                    (\range ->
                        case range of
                            Single int ->
                                [ "("
                                , generateExpression expression
                                , " /= "
                                , toString int
                                , ")"
                                ]
                                    |> String.concat

                            Range a b ->
                                [ "("
                                , [ "("
                                  , generateExpression expression
                                  , " > "
                                  , toString a
                                  , ")"
                                  ]
                                    |> String.concat
                                , " && "
                                , [ "("
                                  , generateExpression expression
                                  , " < "
                                  , toString b
                                  , ")"
                                  ]
                                    |> String.concat
                                , ")"
                                ]
                                    |> String.concat
                    )
                |> String.join " && "


generateExpression : Expression -> String
generateExpression expression =
    case expression of
        Simple pluralOperand ->
            generateOperand "." pluralOperand

        Modulo pluralOperand modulo ->
            [ generateOperand "." pluralOperand
            , " % "
            , toString modulo
            ]
                |> String.concat


generateOperand : String -> PluralOperand -> String
generateOperand decimal pluralOperand =
    [ "("
    , String.concat <|
        case pluralOperand of
            AbsoluteValue ->
                [ "absoluteValue "
                , "'"
                , decimal
                , "'"
                , " count"
                ]

            FractionDigits withTrailingZeros ->
                [ "fractionDigits "
                , "'"
                , decimal
                , "'"
                , " "
                , withTrailingZeros |> toString
                , " count"
                ]

            IntegerDigitCount ->
                [ "integerDigitCount "
                , "'"
                , decimal
                , "'"
                , " count"
                ]

            FractionDigitCount withTrailingZeros ->
                [ "fractionDigitCount "
                , "'"
                , decimal
                , "'"
                , " "
                , withTrailingZeros |> toString
                , " count"
                ]
    , ")"
    ]
        |> String.concat



---- PRINTER


printOr : Or -> String
printOr (Or or) =
    or
        |> List.map printAnd
        |> String.join " or "


printAnd : And -> String
printAnd (And and) =
    and
        |> List.map printRelation
        |> String.join " and "


printRelation : Relation -> String
printRelation relation =
    case relation of
        Equal expression ranges ->
            [ printExpression expression
            , " = "
            , ranges
                |> List.map printRange
                |> String.join ","
            ]
                |> String.concat

        NotEqual expression ranges ->
            [ printExpression expression
            , " != "
            , ranges
                |> List.map printRange
                |> String.join ","
            ]
                |> String.concat


printExpression : Expression -> String
printExpression expression =
    case expression of
        Simple pluralOperand ->
            printPluralOperand pluralOperand

        Modulo pluralOperand modulo ->
            [ printPluralOperand pluralOperand
            , " % "
            , toString modulo
            ]
                |> String.concat


printPluralOperand : PluralOperand -> String
printPluralOperand pluralOperand =
    case pluralOperand of
        AbsoluteValue ->
            "n"

        FractionDigits WithTrailingZeros ->
            "f"

        FractionDigits WithoutTrailingZeros ->
            "t"

        IntegerDigitCount ->
            "i"

        FractionDigitCount WithTrailingZeros ->
            "v"

        FractionDigitCount WithoutTrailingZeros ->
            "w"


printRange : Range -> String
printRange range =
    case range of
        Single int ->
            toString int

        Range a b ->
            [ toString a
            , ".."
            , toString b
            ]
                |> String.concat



---- PARSER


orParser : Parser Or
orParser =
    andParser
        |> andThen
            (\and ->
                oneOf
                    [ delayedCommit (spaces |. symbol "or")
                        (succeed (\(Or ands) -> Or (and :: ands))
                            |. spaces
                            |= orParser
                        )
                    , succeed (Or [ and ])
                    ]
            )


andParser : Parser And
andParser =
    relationParser
        |> andThen
            (\relation ->
                oneOf
                    [ delayedCommit (spaces |. symbol "and")
                        (succeed (\(And relations) -> And (relation :: relations))
                            |. spaces
                            |= andParser
                        )
                    , succeed (And [ relation ])
                    ]
            )


relationParser : Parser Relation
relationParser =
    succeed identity
        |= oneOf
            [ delayedCommitMap Equal
                (expressionParser
                    |. spaces
                    |. symbol "="
                )
                (succeed identity
                    |. spaces
                    |= rangesParser
                )
            , delayedCommitMap NotEqual
                (expressionParser
                    |. spaces
                    |. symbol "!="
                )
                (succeed identity
                    |. spaces
                    |= rangesParser
                )
            ]


expressionParser : Parser Expression
expressionParser =
    oneOf
        [ delayedCommitMap Modulo
            (pluralOperandParser
                |. spaces
                |. symbol "%"
            )
            (succeed identity
                |. spaces
                |= intParser
            )
        , succeed Simple
            |= pluralOperandParser
        ]


pluralOperandParser : Parser PluralOperand
pluralOperandParser =
    let
        parser ( operand, sym ) =
            succeed operand |. symbol sym
    in
    [ ( AbsoluteValue, "n" )
    , ( FractionDigits WithTrailingZeros, "f" )
    , ( FractionDigits WithoutTrailingZeros, "t" )
    , ( IntegerDigitCount, "i" )
    , ( FractionDigitCount WithTrailingZeros, "v" )
    , ( FractionDigitCount WithoutTrailingZeros, "w" )
    ]
        |> List.map parser
        |> oneOf


rangesParser : Parser (List Range)
rangesParser =
    rangeParser
        |> andThen
            (\range ->
                oneOf
                    [ delayedCommit (symbol ",")
                        (succeed (\ranges -> range :: ranges)
                            |= rangesParser
                        )
                    , succeed [ range ]
                    ]
            )


rangeParser : Parser Range
rangeParser =
    oneOf
        [ delayedCommitMap Range
            (intParser
                |. keyword ".."
            )
            intParser
        , succeed Single
            |= intParser
        ]



---- PARSER HELPER


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


intParser : Parser Int
intParser =
    oneOf
        [ succeed 0 |. symbol "0"
        , (source <|
            ignore (Exactly 1) (\c -> Char.isDigit c && c /= '0')
                |. ignore zeroOrMore Char.isDigit
          )
            |> andThen
                (\numbers ->
                    case String.toInt numbers of
                        Ok int ->
                            succeed int

                        Err error ->
                            fail error
                )
        ]