module LocalizedTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Localized exposing (..)
import Localized.De as De
import Localized.En as En
import Test exposing (..)


suite : Test
suite =
    describe "print"
        [ test "a simple message" <|
            \_ ->
                simpleMessage
                    |> print {}
                    |> Expect.equal "Hello!"
        , test "a message with string argument" <|
            \_ ->
                stringMessage
                    |> print { name = "Alice" }
                    |> Expect.equal "Good morning, Alice!"
        , describe "a message with decimal argument"
            [ test "custom" <|
                \_ ->
                    let
                        numberFormat =
                            { positivePattern =
                                { prefix = ""
                                , suffix = ""
                                , primaryGroupingSize = Nothing
                                , secondaryGroupingSize = Nothing
                                , minimalIntegerCount = 1
                                , minimalFractionCount = 0
                                , maximalFractionCount = 0
                                }
                            , negativePattern = Nothing
                            }
                    in
                    [ customDecimal .num enNumberSymbols numberFormat ]
                        |> print { num = 1234 }
                        |> Expect.equal "1234"
            , test "with leading zeros" <|
                \_ ->
                    let
                        numberFormat =
                            { positivePattern =
                                { prefix = ""
                                , suffix = ""
                                , primaryGroupingSize = Nothing
                                , secondaryGroupingSize = Nothing
                                , minimalIntegerCount = 3
                                , minimalFractionCount = 0
                                , maximalFractionCount = 0
                                }
                            , negativePattern = Nothing
                            }
                    in
                    [ customDecimal .num enNumberSymbols numberFormat ]
                        |> print { num = 1 }
                        |> Expect.equal "001"
            , test "with fractional part" <|
                \_ ->
                    let
                        numberFormat =
                            { positivePattern =
                                { prefix = ""
                                , suffix = ""
                                , primaryGroupingSize = Nothing
                                , secondaryGroupingSize = Nothing
                                , minimalIntegerCount = 1
                                , minimalFractionCount = 0
                                , maximalFractionCount = 2
                                }
                            , negativePattern = Nothing
                            }
                    in
                    [ customDecimal .num enNumberSymbols numberFormat ]
                        |> print { num = 42.24 }
                        |> Expect.equal "42.24"
            , test "with fractional part and trailing zeros" <|
                \_ ->
                    let
                        numberFormat =
                            { positivePattern =
                                { prefix = ""
                                , suffix = ""
                                , primaryGroupingSize = Nothing
                                , secondaryGroupingSize = Nothing
                                , minimalIntegerCount = 1
                                , minimalFractionCount = 3
                                , maximalFractionCount = 5
                                }
                            , negativePattern = Nothing
                            }
                    in
                    [ customDecimal .num enNumberSymbols numberFormat ]
                        |> print { num = 42.24 }
                        |> Expect.equal "42.240"
            , test "with fractional part and zeros in before last digit" <|
                \_ ->
                    let
                        numberFormat =
                            { positivePattern =
                                { prefix = ""
                                , suffix = ""
                                , primaryGroupingSize = Nothing
                                , secondaryGroupingSize = Nothing
                                , minimalIntegerCount = 1
                                , minimalFractionCount = 0
                                , maximalFractionCount = 3
                                }
                            , negativePattern = Nothing
                            }
                    in
                    [ customDecimal .num enNumberSymbols numberFormat ]
                        |> print { num = 42.001 }
                        |> Expect.equal "42.001"
            , test "with fractional part and non-zero digit after maximalFractionCount" <|
                \_ ->
                    let
                        numberFormat =
                            { positivePattern =
                                { prefix = ""
                                , suffix = ""
                                , primaryGroupingSize = Nothing
                                , secondaryGroupingSize = Nothing
                                , minimalIntegerCount = 1
                                , minimalFractionCount = 0
                                , maximalFractionCount = 3
                                }
                            , negativePattern = Nothing
                            }
                    in
                    [ customDecimal .num enNumberSymbols numberFormat ]
                        |> print { num = 42.0001 }
                        |> Expect.equal "42"
            , test "with fractional part and non-zero digit after maximalFractionCount, but with non-zero minimalFractionCount" <|
                \_ ->
                    let
                        numberFormat =
                            { positivePattern =
                                { prefix = ""
                                , suffix = ""
                                , primaryGroupingSize = Nothing
                                , secondaryGroupingSize = Nothing
                                , minimalIntegerCount = 1
                                , minimalFractionCount = 1
                                , maximalFractionCount = 3
                                }
                            , negativePattern = Nothing
                            }
                    in
                    [ customDecimal .num enNumberSymbols numberFormat ]
                        |> print { num = 42.0001 }
                        |> Expect.equal "42.0"
            ]
        , describe "de locale"
            [ describe "a decimal message"
                [ test "height = 1234.12345" <|
                    \_ ->
                        deDecimalMessage
                            |> print { height = 1234.12345 }
                            |> Expect.equal "Aktuelle Höhe: 1234,123."
                ]
            ]
        , describe "en locale"
            [ describe "a decimal message"
                [ test "height = 1234.12345" <|
                    \_ ->
                        enDecimalMessage
                            |> print { height = 1234.12345 }
                            |> Expect.equal "Current height: 1234.123."
                ]
            , describe "a cardinal message"
                [ test "count = 1" <|
                    \_ ->
                        enCardinalMsg
                            |> print { count = 1 }
                            |> Expect.equal "There is one new message."
                , test "count = 10" <|
                    \_ ->
                        enCardinalMsg
                            |> print { count = 10 }
                            |> Expect.equal "There are 10 new messages."
                ]
            , describe "an ordinal message"
                [ test "count = 1" <|
                    \_ ->
                        enOrdinalMsg
                            |> print { count = 1 }
                            |> Expect.equal "This is the 1st message."
                , test "count = 2" <|
                    \_ ->
                        enOrdinalMsg
                            |> print { count = 2 }
                            |> Expect.equal "This is the 2nd message."
                , test "count = 3" <|
                    \_ ->
                        enOrdinalMsg
                            |> print { count = 3 }
                            |> Expect.equal "This is the 3rd message."
                , test "count = 7" <|
                    \_ ->
                        enOrdinalMsg
                            |> print { count = 7 }
                            |> Expect.equal "This is the 7th message."
                , test "count = 11" <|
                    \_ ->
                        enOrdinalMsg
                            |> print { count = 11 }
                            |> Expect.equal "This is the 11th message."
                , test "count = 42" <|
                    \_ ->
                        enOrdinalMsg
                            |> print { count = 42 }
                            |> Expect.equal "This is the 42nd message."
                ]
            ]
        ]



---- EXAMPLE MESSAGES


enNumberSymbols =
    { decimal = "."
    , group = ","
    , list = ";"
    , percentSign = "%"
    , plusSign = "+"
    , minusSign = "-"
    , exponential = "E"
    , superscriptingExponent = "×"
    , perMille = "‰"
    , infinity = "∞"
    , nan = "NaN"
    , timeSeparator = ":"
    }


simpleMessage =
    [ s "Hello!" ]


stringMessage =
    [ s "Good morning, "
    , string .name
    , s "!"
    ]


enDecimalMessage =
    [ s "Current height: "
    , En.decimal .height
    , s "."
    ]


deDecimalMessage =
    [ s "Aktuelle Höhe: "
    , De.decimal .height
    , s "."
    ]


enCardinalMsg =
    [ En.cardinal .count
        { one =
            [ s "There is one new message." ]
        , other =
            [ s "There are "
            , count
            , s " new messages."
            ]
        }
    ]


enOrdinalMsg =
    [ En.ordinal .count
        { one =
            [ s "This is the "
            , count
            , s "st message."
            ]
        , two =
            [ s "This is the "
            , count
            , s "nd message."
            ]
        , few =
            [ s "This is the "
            , count
            , s "rd message."
            ]
        , other =
            [ s "This is the "
            , count
            , s "th message."
            ]
        }
    ]
