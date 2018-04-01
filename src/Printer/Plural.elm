module Printer.Plural
    exposing
        ( fractionDigitCount
        , fractionDigits
        , integerDigits
        )

import Data.PluralRules exposing (WithTrailingZeros(..))
import Text exposing (FloatInfo)


integerDigits : FloatInfo -> Int
integerDigits floatInfo =
    floatInfo.integerDigits
        |> List.foldr
            (\digit int ->
                10 * int + digit
            )
            0


fractionDigits : WithTrailingZeros -> FloatInfo -> Int
fractionDigits withTrailingZeros floatInfo =
    case withTrailingZeros of
        WithTrailingZeros ->
            floatInfo.fractionDigits
                |> List.foldr
                    (\digit int ->
                        10 * int + digit
                    )
                    0

        WithoutTrailingZeros ->
            floatInfo.fractionDigits
                |> List.foldr
                    (\digit maybeInt ->
                        case maybeInt of
                            Just int ->
                                Just (10 * int + digit)

                            Nothing ->
                                if digit == 0 then
                                    Nothing
                                else
                                    Just digit
                    )
                    Nothing
                |> Maybe.withDefault 0


fractionDigitCount : WithTrailingZeros -> FloatInfo -> Int
fractionDigitCount withTrailingZeros floatInfo =
    case withTrailingZeros of
        WithTrailingZeros ->
            floatInfo.fractionDigits
                |> List.length

        WithoutTrailingZeros ->
            floatInfo.fractionDigits
                |> List.foldr
                    (\digit maybeFractionDigits ->
                        case maybeFractionDigits of
                            Just digits ->
                                Just (digit :: digits)

                            Nothing ->
                                if digit == 0 then
                                    Nothing
                                else
                                    Just [ digit ]
                    )
                    Nothing
                |> Maybe.withDefault []
                |> List.length
