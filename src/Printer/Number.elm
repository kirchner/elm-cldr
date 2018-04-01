module Printer.Number
    exposing
        ( floatInfo
        , print
        )

import Data.Numbers exposing (NumberFormat, Symbols)
import Text exposing (FloatInfo)


floatInfo : NumberFormat -> Float -> FloatInfo
floatInfo numberFormat num =
    let
        withPattern pattern =
            { absoluteValue = abs num
            , integerDigits =
                num
                    |> floor
                    |> digits
            , fractionDigits =
                fractionalDigits
                    pattern.minimalFractionCount
                    pattern.maximalFractionCount
                    0
                    num
            }

        fractionalDigits minimal maximal printed num =
            if printed < maximal then
                let
                    nextDigit =
                        ((num * 10) |> floor) % 10
                in
                if printed < minimal then
                    nextDigit :: fractionalDigits minimal maximal (printed + 1) (num * 10)
                else if
                    (floor (num * toFloat (10 ^ (maximal - printed)))
                        % (10 ^ (maximal - printed))
                    )
                        /= 0
                then
                    nextDigit :: fractionalDigits minimal maximal (printed + 1) (num * 10)
                else
                    []
            else
                []
    in
    if num >= 0 then
        withPattern numberFormat.positivePattern
    else
        case numberFormat.negativePattern of
            Just negativePattern ->
                withPattern negativePattern

            Nothing ->
                withPattern numberFormat.positivePattern


print : Symbols -> NumberFormat -> Float -> String
print symbols numberFormat num =
    let
        printWithPattern pattern num =
            [ pattern.prefix
            , if num < 0 then
                symbols.minusSign
              else
                ""
            , integerPart pattern num
            , case fractionalPart pattern num of
                "" ->
                    ""

                part ->
                    [ symbols.decimal
                    , part
                    ]
                        |> String.concat
            , pattern.suffix
            ]
                |> String.concat

        integerPart numberPattern num =
            let
                integerDigits =
                    num
                        |> floor
                        |> digits

                leadingZeroCount =
                    (numberPattern.minimalIntegerCount - List.length integerDigits)
                        |> clamp 0 numberPattern.minimalIntegerCount
            in
            [ String.repeat leadingZeroCount "0"
            , integerDigits
                |> List.map toString
                |> String.concat
            ]
                |> String.concat

        fractionalPart numberPattern num =
            [ fractionalDigits
                numberPattern.minimalFractionCount
                numberPattern.maximalFractionCount
                0
                num
            ]
                |> String.concat
    in
    if num >= 0 then
        printWithPattern numberFormat.positivePattern num
    else
        case numberFormat.negativePattern of
            Just negativePattern ->
                printWithPattern negativePattern num

            Nothing ->
                printWithPattern numberFormat.positivePattern num


fractionalDigits : Int -> Int -> Int -> Float -> String
fractionalDigits minimal maximal printed num =
    if printed < maximal then
        let
            nextDigit =
                (((num * 10) |> floor) % 10) |> toString
        in
        if printed < minimal then
            [ nextDigit
            , fractionalDigits minimal maximal (printed + 1) (num * 10)
            ]
                |> String.concat
        else if
            (floor (num * toFloat (10 ^ (maximal - printed)))
                % (10 ^ (maximal - printed))
            )
                /= 0
        then
            [ nextDigit
            , fractionalDigits minimal maximal (printed + 1) (num * 10)
            ]
                |> String.concat
        else
            ""
    else
        ""


digits : Int -> List Int
digits num =
    digitsHelper [] num
        |> List.reverse


digitsHelper : List Int -> Int -> List Int
digitsHelper digits num =
    if num // 10 == 0 then
        num :: digits
    else
        (num % 10) :: digitsHelper digits (num // 10)
