module Parsers exposing (..)

import Cldr exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Parser exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "parse"
        [ describe "Relation"
            [ fuzz relationFuzzer "random relation" <|
                \relation ->
                    relation
                        |> printRelation
                        |> run relationParser
                        |> Expect.equal (Ok relation)
            , test "Equal" <|
                \_ ->
                    "n = 1"
                        |> run relationParser
                        |> Expect.equal
                            (Ok
                                (Equal
                                    (Simple AbsoluteValue)
                                    [ Single 1 ]
                                )
                            )
            , test "Equal 0" <|
                \_ ->
                    "n = 0"
                        |> run relationParser
                        |> Expect.equal
                            (Ok
                                (Equal
                                    (Simple AbsoluteValue)
                                    [ Single 0 ]
                                )
                            )
            , test "Not Equal" <|
                \_ ->
                    "n % 5 != 42"
                        |> run relationParser
                        |> Expect.equal
                            (Ok
                                (NotEqual
                                    (Modulo AbsoluteValue 5)
                                    [ Single 42 ]
                                )
                            )
            ]
        , test "And" <|
            \_ ->
                "n % 10 = 0 and n != 100"
                    |> run andParser
                    |> Expect.equal
                        (Ok
                            (And
                                [ Equal
                                    (Modulo AbsoluteValue 10)
                                    [ Single 0 ]
                                , NotEqual
                                    (Simple AbsoluteValue)
                                    [ Single 100 ]
                                ]
                            )
                        )
        , test "Or" <|
            \_ ->
                "n % 5 != 2 or n = 1"
                    |> run orParser
                    |> Expect.equal
                        (Ok
                            (Or
                                [ And
                                    [ NotEqual
                                        (Modulo AbsoluteValue 5)
                                        [ Single 2 ]
                                    ]
                                , And
                                    [ Equal
                                        (Simple AbsoluteValue)
                                        [ Single 1 ]
                                    ]
                                ]
                            )
                        )
        , describe "Expression"
            [ test "Simple" <|
                \_ ->
                    "n"
                        |> run expressionParser
                        |> Expect.equal (Ok (Simple AbsoluteValue))
            , test "Modulo" <|
                \_ ->
                    "n % 10"
                        |> run expressionParser
                        |> Expect.equal (Ok (Modulo AbsoluteValue 10))
            , test "Modulo 0" <|
                \_ ->
                    "n % 0"
                        |> run expressionParser
                        |> Expect.equal (Ok (Modulo AbsoluteValue 0))
            ]
        , describe "PluralOperand"
            [ test "AbsoluteValue" <|
                \_ ->
                    "n"
                        |> run pluralOperandParser
                        |> Expect.equal (Ok AbsoluteValue)
            , test "IntegerDigitCount" <|
                \_ ->
                    "i"
                        |> run pluralOperandParser
                        |> Expect.equal (Ok IntegerDigitCount)
            ]
        , describe "Range"
            [ test "a range" <|
                \_ ->
                    "1..2"
                        |> run rangeParser
                        |> Expect.equal (Ok (Range 1 2))
            , test "a single range" <|
                \_ ->
                    "42"
                        |> run rangeParser
                        |> Expect.equal (Ok (Single 42))
            ]
        , describe "List Range"
            [ test "a list of ranges" <|
                \_ ->
                    "1,2,3,1..4"
                        |> run rangesParser
                        |> Expect.equal (Ok [ Single 1, Single 2, Single 3, Range 1 4 ])
            ]
        , describe "Int"
            [ test "simple int" <|
                \_ ->
                    "123"
                        |> run intParser
                        |> Expect.equal (Ok 123)
            , test "non digits follow" <|
                \_ ->
                    "123a"
                        |> run intParser
                        |> Expect.equal (Ok 123)
            , test "non digits follow and get parsed" <|
                \_ ->
                    "123a"
                        |> run
                            (succeed identity
                                |= intParser
                                |. keyword "a"
                            )
                        |> Expect.equal (Ok 123)
            , fuzz Fuzz.int "random int" <|
                \int ->
                    (int ^ 2)
                        |> toString
                        |> run intParser
                        |> Expect.equal (Ok (int ^ 2))
            ]
        , describe "NumerFormats"
            [ test "with integer part" <|
                \_ ->
                    "###0"
                        |> numberFormat
                        |> Expect.equal
                            (Ok
                                { positivePattern =
                                    { prefix = ""
                                    , suffix = ""
                                    , primaryGroupingSize = Nothing
                                    , secondaryGroupingSize = Nothing
                                    , integerPattern =
                                        [ HashPattern
                                        , HashPattern
                                        , HashPattern
                                        , ZeroPattern
                                        ]
                                    , fractionalPattern = []
                                    }
                                , negativePattern = Nothing
                                }
                            )
            , test "with integer part and primary group" <|
                \_ ->
                    "#,###0"
                        |> numberFormat
                        |> Expect.equal
                            (Ok
                                { positivePattern =
                                    { prefix = ""
                                    , suffix = ""
                                    , primaryGroupingSize = Just 4
                                    , secondaryGroupingSize = Nothing
                                    , integerPattern =
                                        [ HashPattern
                                        , HashPattern
                                        , HashPattern
                                        , HashPattern
                                        , ZeroPattern
                                        ]
                                    , fractionalPattern = []
                                    }
                                , negativePattern = Nothing
                                }
                            )
            , test "with integer part, primary group and secondary group" <|
                \_ ->
                    "#,#,###0"
                        |> numberFormat
                        |> Expect.equal
                            (Ok
                                { positivePattern =
                                    { prefix = ""
                                    , suffix = ""
                                    , primaryGroupingSize = Just 4
                                    , secondaryGroupingSize = Just 1
                                    , integerPattern =
                                        [ HashPattern
                                        , HashPattern
                                        , HashPattern
                                        , HashPattern
                                        , HashPattern
                                        , ZeroPattern
                                        ]
                                    , fractionalPattern = []
                                    }
                                , negativePattern = Nothing
                                }
                            )
            , test "with integer and fractional part" <|
                \_ ->
                    "#,##0.##"
                        |> numberFormat
                        |> Expect.equal
                            (Ok
                                { positivePattern =
                                    { prefix = ""
                                    , suffix = ""
                                    , primaryGroupingSize = Just 3
                                    , secondaryGroupingSize = Nothing
                                    , integerPattern =
                                        [ HashPattern
                                        , HashPattern
                                        , HashPattern
                                        , ZeroPattern
                                        ]
                                    , fractionalPattern =
                                        [ HashPattern
                                        , HashPattern
                                        ]
                                    }
                                , negativePattern = Nothing
                                }
                            )
            ]
        ]



---- FUZZER


orFuzzer : Fuzzer Or
orFuzzer =
    Fuzz.map2 (::) andFuzzer (Fuzz.list andFuzzer)
        |> Fuzz.map Or


andFuzzer : Fuzzer And
andFuzzer =
    Fuzz.map2 (::) relationFuzzer (Fuzz.list relationFuzzer)
        |> Fuzz.map And


relationFuzzer : Fuzzer Relation
relationFuzzer =
    Fuzz.oneOf
        [ Fuzz.map2 Equal expressionFuzzer rangesFuzzer
        , Fuzz.map2 NotEqual expressionFuzzer rangesFuzzer
        ]


expressionFuzzer : Fuzzer Expression
expressionFuzzer =
    Fuzz.oneOf
        [ pluralOperandFuzzer |> Fuzz.map Simple
        , Fuzz.map2 Modulo pluralOperandFuzzer positiveIntFuzzer
        ]


pluralOperandFuzzer : Fuzzer PluralOperand
pluralOperandFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant AbsoluteValue
        , Fuzz.constant (FractionDigits WithTrailingZeros)
        , Fuzz.constant (FractionDigits WithoutTrailingZeros)
        , Fuzz.constant IntegerDigitCount
        , Fuzz.constant (FractionDigitCount WithTrailingZeros)
        , Fuzz.constant (FractionDigitCount WithoutTrailingZeros)
        ]


rangesFuzzer : Fuzzer (List Range)
rangesFuzzer =
    Fuzz.map2 (::)
        rangeFuzzer
        (Fuzz.list rangeFuzzer)


rangeFuzzer : Fuzzer Range
rangeFuzzer =
    Fuzz.oneOf
        [ positiveIntFuzzer |> Fuzz.map Single
        , Fuzz.map2 Range positiveIntFuzzer positiveIntFuzzer
        ]


positiveIntFuzzer : Fuzzer Int
positiveIntFuzzer =
    Fuzz.int |> Fuzz.map abs
