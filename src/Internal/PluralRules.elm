module Internal.PluralRules
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
        , absoluteValue
        , fractionDigitCount
        , fractionDigits
        , integerDigits
        )

import Char


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


checkRelation : Char -> String -> Relation -> Bool
checkRelation decimal num relation =
    case relation of
        Equal expression ranges ->
            ranges |> List.all (checkExpression decimal num expression)

        NotEqual expression ranges ->
            ranges |> List.all (checkExpression decimal num expression >> not)

        Or relationA relationB ->
            checkRelation decimal num relationA
                || checkRelation decimal num relationB

        And relationA relationB ->
            checkRelation decimal num relationA
                && checkRelation decimal num relationB


checkExpression : Char -> String -> Expression -> Range -> Bool
checkExpression decimal num expression range =
    let
        value pluralOperand =
            case pluralOperand of
                AbsoluteValue ->
                    absoluteValue decimal num

                FractionDigits withTrailingZeros ->
                    fractionDigits decimal withTrailingZeros num
                        |> toFloat

                IntegerDigits ->
                    integerDigits decimal num
                        |> toFloat

                FractionDigitCount withTrailingZeros ->
                    fractionDigitCount decimal withTrailingZeros num
                        |> toFloat

        inRange float =
            case range of
                Single a ->
                    toFloat a == float

                Range a b ->
                    (toFloat a <= float) && (float <= toFloat b)
    in
    case expression of
        Simple pluralOperand ->
            pluralOperand
                |> value
                |> inRange

        Modulo pluralOperand modulo ->
            ((pluralOperand |> value |> floor) % modulo)
                |> toFloat
                |> inRange


absoluteValue : Char -> String -> Float
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
    of
        Just float ->
            float

        Nothing ->
            Debug.crash "absoluteValue failed"


integerDigits : Char -> String -> Int
integerDigits decimal num =
    case num |> String.split (String.fromChar decimal) of
        integerString :: _ ->
            if integerString |> String.all Char.isDigit then
                integerString
                    |> String.toInt
                    |> Result.toMaybe
                    |> Maybe.withDefault 0
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
