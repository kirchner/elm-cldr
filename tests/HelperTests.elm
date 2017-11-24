module HelperTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Helper exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Helper"
        [ describe "absoluteValue"
            [ test "trailing zeros" <|
                \_ ->
                    "42.00"
                        |> absoluteValue '.'
                        |> Expect.equal (Just 42)
            , fuzz Fuzz.int "integer" <|
                \int ->
                    int
                        |> toString
                        |> absoluteValue '.'
                        |> Expect.equal (Just (toFloat (abs int)))
            , fuzz Fuzz.float "float" <|
                \float ->
                    float
                        |> toString
                        |> absoluteValue '.'
                        |> Expect.equal (Just (abs float))
            ]
        , test "integerDigitCount" <|
            \_ ->
                "123.209"
                    |> integerDigitCount '.'
                    |> Expect.equal (Just 3)
        , describe "fractionDigits"
            [ test "WithTrailingZeros" <|
                \_ ->
                    "42.00100"
                        |> fractionDigits '.' WithTrailingZeros
                        |> Expect.equal (Just 100)
            , test "WithoutTrailingZeros" <|
                \_ ->
                    "42.00100"
                        |> fractionDigits '.' WithoutTrailingZeros
                        |> Expect.equal (Just 1)
            ]
        , describe "fractionDigitCount"
            [ test "WithTrailingZeros" <|
                \_ ->
                    "42.00100"
                        |> fractionDigitCount '.' WithTrailingZeros
                        |> Expect.equal (Just 5)
            , test "WithoutTrailingZeros" <|
                \_ ->
                    "42.00100"
                        |> fractionDigitCount '.' WithoutTrailingZeros
                        |> Expect.equal (Just 3)
            ]
        ]
