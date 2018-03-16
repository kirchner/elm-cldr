module Data.PluralRules
    exposing
        ( Expression
            ( Modulo
            , Simple
            )
        , PluralOperand
            ( AbsoluteValue
            , FractionDigitCount
            , FractionDigits
            , IntegerDigits
            )
        , PluralRules
        , Range
            ( Range
            , Single
            )
        , Relation
            ( And
            , Equal
            , NotEqual
            , Or
            )
        , WithTrailingZeros
            ( WithTrailingZeros
            , WithoutTrailingZeros
            )
        , decoder
        )

import Char
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Parser exposing (..)


type alias PluralRules =
    { zero : Maybe Relation
    , one : Maybe Relation
    , two : Maybe Relation
    , few : Maybe Relation
    , many : Maybe Relation
    }


type Relation
    = Equal Expression (List Range)
    | NotEqual Expression (List Range)
    | Or Relation Relation
    | And Relation Relation


type Expression
    = Simple PluralOperand
    | Modulo PluralOperand Int


type PluralOperand
    = AbsoluteValue
    | IntegerDigits
    | FractionDigitCount WithTrailingZeros
    | FractionDigits WithTrailingZeros


type WithTrailingZeros
    = WithTrailingZeros
    | WithoutTrailingZeros


type Range
    = Single Int
    | Range Int Int



---- DECODER


decoder =
    Decode.succeed PluralRules
        |> Decode.optional "pluralRule-count-zero" (Decode.map Just pluralRuleDecoder) Nothing
        |> Decode.optional "pluralRule-count-one" (Decode.map Just pluralRuleDecoder) Nothing
        |> Decode.optional "pluralRule-count-two" (Decode.map Just pluralRuleDecoder) Nothing
        |> Decode.optional "pluralRule-count-few" (Decode.map Just pluralRuleDecoder) Nothing
        |> Decode.optional "pluralRule-count-many" (Decode.map Just pluralRuleDecoder) Nothing


pluralRuleDecoder : Decoder Relation
pluralRuleDecoder =
    Decode.string
        |> Decode.andThen
            (\pluralRule ->
                case run orParser pluralRule of
                    Ok rule ->
                        Decode.succeed rule

                    Err error ->
                        Decode.fail ("not a proper plural rule: " ++ toString error)
            )



---- PARSER


orParser : Parser Relation
orParser =
    andParser
        |> andThen
            (\and ->
                oneOf
                    [ delayedCommit (spaces |. symbol "or")
                        (succeed (\or -> Or and or)
                            |. spaces
                            |= orParser
                        )
                    , succeed and
                    ]
            )


andParser : Parser Relation
andParser =
    relationParser
        |> andThen
            (\relation ->
                oneOf
                    [ delayedCommit (spaces |. symbol "and")
                        (succeed (\and -> And relation and)
                            |. spaces
                            |= andParser
                        )
                    , succeed relation
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
    , ( IntegerDigits, "i" )
    , ( FractionDigitCount WithTrailingZeros, "v" )
    , ( FractionDigitCount WithoutTrailingZeros, "w" )
    , ( FractionDigits WithTrailingZeros, "f" )
    , ( FractionDigits WithoutTrailingZeros, "t" )
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
