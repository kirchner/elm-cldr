module Helper exposing (..)

{-| -}

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


{-|

    absoluteValue "." "132.200" == 123.2

-}
absoluteValue : Char -> String -> Maybe Float
absoluteValue decimal num =
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
        |> Maybe.map abs


integerDigitCount : Char -> String -> Maybe Int
integerDigitCount decimal num =
    case num |> String.split (String.fromChar decimal) of
        integerString :: _ ->
            if integerString |> String.all Char.isDigit then
                integerString
                    |> String.length
                    |> Just
            else
                Nothing

        _ ->
            Nothing


fractionDigits : Char -> WithTrailingZeros -> String -> Maybe Int
fractionDigits decimal withTrailingZeros num =
    case withTrailingZeros of
        WithTrailingZeros ->
            case num |> String.split (String.fromChar decimal) of
                _ :: fractionString :: [] ->
                    fractionString
                        |> String.toInt
                        |> Result.toMaybe

                _ ->
                    Nothing

        WithoutTrailingZeros ->
            case num |> String.split (String.fromChar decimal) of
                _ :: fractionString :: [] ->
                    fractionString
                        |> chompTrailingZeros
                        |> String.toInt
                        |> Result.toMaybe

                _ ->
                    Nothing


fractionDigitCount : Char -> WithTrailingZeros -> String -> Maybe Int
fractionDigitCount decimal withTrailingZeros num =
    case withTrailingZeros of
        WithTrailingZeros ->
            case num |> String.split (String.fromChar decimal) of
                _ :: fractionString :: [] ->
                    if fractionString |> String.all Char.isDigit then
                        fractionString
                            |> String.length
                            |> Just
                    else
                        Nothing

                _ ->
                    Nothing

        WithoutTrailingZeros ->
            case num |> String.split (String.fromChar decimal) of
                _ :: fractionString :: [] ->
                    if fractionString |> String.all Char.isDigit then
                        fractionString
                            |> chompTrailingZeros
                            |> String.length
                            |> Just
                    else
                        Nothing

                _ ->
                    Nothing


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
