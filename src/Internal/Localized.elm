module Internal.Localized exposing (..)

import Char


type PluralCase
    = Zero
    | One
    | Two
    | Few
    | Many
    | Other


type WithTrailingZeros
    = WithTrailingZeros
    | WithoutTrailingZeros



---- PLURAL SELECTOR HELPER


absoluteValue : Char -> String -> Int
absoluteValue decimal num =
    case
        num
            |> String.map
                (\c ->
                    if c == decimal then
                        '.'
                    else
                        c
                )
            |> String.toFloat
            |> Result.toMaybe
            |> Maybe.map (floor >> abs)
    of
        Just i ->
            i

        Nothing ->
            Debug.crash "absoluteValue failed"


integerDigitCount : Char -> String -> Int
integerDigitCount decimal num =
    case num |> String.split (String.fromChar decimal) of
        integerString :: _ ->
            if integerString |> String.all Char.isDigit then
                integerString
                    |> String.length
            else
                0

        _ ->
            0


fractionDigits : Char -> WithTrailingZeros -> String -> Int
fractionDigits decimal withTrailingZeros num =
    case withTrailingZeros of
        WithTrailingZeros ->
            case num |> String.split (String.fromChar decimal) of
                _ :: fractionString :: [] ->
                    case
                        fractionString
                            |> String.toInt
                            |> Result.toMaybe
                    of
                        Just i ->
                            i

                        Nothing ->
                            Debug.crash "fraction digits failed"

                _ ->
                    Debug.crash "fraction digits failed"

        WithoutTrailingZeros ->
            case num |> String.split (String.fromChar decimal) of
                _ :: fractionString :: [] ->
                    case
                        fractionString
                            |> chompTrailingZeros
                            |> String.toInt
                            |> Result.toMaybe
                    of
                        Just i ->
                            i

                        Nothing ->
                            Debug.crash "fraction digits failed"

                _ ->
                    Debug.crash "fraction digits failed"


fractionDigitCount : Char -> WithTrailingZeros -> String -> Int
fractionDigitCount decimal withTrailingZeros num =
    case withTrailingZeros of
        WithTrailingZeros ->
            case num |> String.split (String.fromChar decimal) of
                _ :: fractionString :: [] ->
                    if fractionString |> String.all Char.isDigit then
                        fractionString
                            |> String.length
                    else
                        0

                _ ->
                    0

        WithoutTrailingZeros ->
            case num |> String.split (String.fromChar decimal) of
                _ :: fractionString :: [] ->
                    if fractionString |> String.all Char.isDigit then
                        fractionString
                            |> chompTrailingZeros
                            |> String.length
                    else
                        0

                _ ->
                    0


chompTrailingZeros : String -> String
chompTrailingZeros string =
    let
        chompTrailingZero nextChar ( chomp, sum ) =
            if nextChar == '0' && chomp then
                ( True, sum )
            else
                ( False, nextChar :: sum )
    in
    string
        |> String.foldr chompTrailingZero ( True, [] )
        |> Tuple.second
        |> String.fromList
