module LocalizedTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Internal.PluralRules exposing (..)
import Localized exposing (..)
import Localized.De as De
import Localized.En as En
import Test exposing (..)


pluralTests : Test
pluralTests =
    describe "plural helpers"
        [ describe "absoluteValue"
            [ test "of 1" <|
                \_ ->
                    "1"
                        |> absoluteValue '.'
                        |> Expect.equal 1
            , test "of 1.5" <|
                \_ ->
                    "1.5"
                        |> absoluteValue '.'
                        |> Expect.equal 1.5
            , fuzz Fuzz.float "of random float" <|
                \float ->
                    float
                        |> toString
                        |> absoluteValue '.'
                        |> Expect.equal float
            ]
        , describe "integerDigits"
            [ test "of 4" <|
                \_ ->
                    "4"
                        |> integerDigits '.'
                        |> Expect.equal 4
            , test "of 4.2" <|
                \_ ->
                    "4.2"
                        |> integerDigits '.'
                        |> Expect.equal 4
            , test "of 123.123" <|
                \_ ->
                    "123.123"
                        |> integerDigits '.'
                        |> Expect.equal 123
            ]
        , describe "fractionDigits"
            [ describe "WithTrailingZeros"
                [ test "of 1.100" <|
                    \_ ->
                        "1.300"
                            |> fractionDigits '.' WithTrailingZeros
                            |> Expect.equal 300
                ]
            , describe "WithoutTrailingZeros"
                [ test "of 1.100" <|
                    \_ ->
                        "1.300"
                            |> fractionDigits '.' WithoutTrailingZeros
                            |> Expect.equal 3
                ]
            ]
        , describe "fractionDigitCount"
            [ describe "WithTrailingZeros"
                [ test "of 1.100" <|
                    \_ ->
                        "1.300"
                            |> fractionDigitCount '.' WithTrailingZeros
                            |> Expect.equal 3
                ]
            , describe "WithoutTrailingZeros"
                [ test "of 1.100" <|
                    \_ ->
                        "1.300"
                            |> fractionDigitCount '.' WithoutTrailingZeros
                            |> Expect.equal 1
                ]
            ]
        ]


suite : Test
suite =
    describe "print"
        [ test "a simple message" <|
            \_ ->
                simpleMessage
                    |> print
                    |> Expect.equal "Hello!"
        , test "a message with string argument" <|
            \_ ->
                stringMessage
                    |> printWith { name = "Alice" }
                    |> Expect.equal "Good morning, Alice!"
        , describe "a decimal message"
            [ test "height = 1234.12345" <|
                \_ ->
                    decimalMessage
                        |> printWith { height = 1234.12345 }
                        |> Expect.equal "Current height: 1234.12345."
            ]
        , describe "en locale"
            [ describe "a cardinal message"
                [ test "count = 1" <|
                    \_ ->
                        enCardinalMsg
                            |> printWith { count = 1 }
                            |> Expect.equal "There is one new message."
                , test "count = 10" <|
                    \_ ->
                        enCardinalMsg
                            |> printWith { count = 10 }
                            |> Expect.equal "There are 10 new messages."
                ]
            , describe "an ordinal message"
                [ test "count = 1" <|
                    \_ ->
                        enOrdinalMsg
                            |> printWith { count = 1 }
                            |> Expect.equal "This is the 1st message."
                , test "count = 2" <|
                    \_ ->
                        enOrdinalMsg
                            |> printWith { count = 2 }
                            |> Expect.equal "This is the 2nd message."
                , test "count = 3" <|
                    \_ ->
                        enOrdinalMsg
                            |> printWith { count = 3 }
                            |> Expect.equal "This is the 3rd message."
                , test "count = 7" <|
                    \_ ->
                        enOrdinalMsg
                            |> printWith { count = 7 }
                            |> Expect.equal "This is the 7th message."
                , test "count = 11" <|
                    \_ ->
                        enOrdinalMsg
                            |> printWith { count = 11 }
                            |> Expect.equal "This is the 11th message."
                , test "count = 42" <|
                    \_ ->
                        enOrdinalMsg
                            |> printWith { count = 42 }
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


decimalMessage =
    [ s "Current height: "
    , decimal .height (customNumberFormat toString)
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
