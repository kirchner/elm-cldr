module Localized exposing (..)

import Char


type Part args
    = Verbatim String
    | String (args -> String)
    | Float (args -> Float) (Float -> String)
    | Plural (args -> Float) (Float -> PluralCase) (AllPluralCases args)
    | DynamicPlural (args -> Float) PluralRules (AllPluralCases args)
    | Count


s : String -> Part args
s text =
    Verbatim text


string : (args -> String) -> Part args
string accessor =
    String accessor


count : Part args
count =
    Count


customPlural :
    (args -> Float)
    -> (Float -> PluralCase)
    -> AllPluralCases args
    -> Part args
customPlural accessor selector cases =
    Plural accessor selector cases


dynamicPlural :
    (args -> Float)
    -> PluralRules
    -> AllPluralCases args
    -> Part args
dynamicPlural accessor pluralRules pluralCases =
    DynamicPlural accessor pluralRules pluralCases



---- PRINT


print : List (Part {}) -> String
print parts =
    parts
        |> printWith {}


printWith : args -> List (Part args) -> String
printWith args parts =
    parts
        |> List.map (printPart Nothing args)
        |> String.concat


printPart : Maybe String -> args -> Part args -> String
printPart maybeCount args part =
    case part of
        Verbatim text ->
            text

        String accessor ->
            accessor args

        Float accessor printer ->
            args |> accessor |> printer

        Plural accessor selector cases ->
            let
                nextCount =
                    -- TODO: insert actual number printing
                    args |> accessor |> toString
            in
            String.concat <|
                List.map (printPart (Just nextCount) args) <|
                    case args |> accessor |> selector of
                        Zero ->
                            cases.zero

                        One ->
                            cases.one

                        Two ->
                            cases.two

                        Few ->
                            cases.few

                        Many ->
                            cases.many

                        Other ->
                            cases.other

        DynamicPlural accessor pluralRules pluralCases ->
            let
                nextCount =
                    -- TODO: insert actual number printing
                    args |> accessor |> toString
            in
            [ ( pluralRules.zero, pluralCases.zero )
            , ( pluralRules.one, pluralCases.one )
            , ( pluralRules.two, pluralCases.two )
            , ( pluralRules.few, pluralCases.few )
            , ( pluralRules.many, pluralCases.many )
            ]
                |> List.filterMap
                    (\( maybeRule, nextParts ) ->
                        case maybeRule |> Maybe.map (checkRelation '.' nextCount) of
                            Just True ->
                                Just nextParts

                            _ ->
                                Nothing
                    )
                |> List.head
                |> Maybe.withDefault pluralCases.other
                |> List.map (printPart (Just nextCount) args)
                |> String.concat

        Count ->
            case maybeCount of
                Just count ->
                    count

                Nothing ->
                    Debug.crash "no count given"



---- PLURALIZATION


type PluralCase
    = Zero
    | One
    | Two
    | Few
    | Many
    | Other


type alias AllPluralCases args =
    { zero : List (Part args)
    , one : List (Part args)
    , two : List (Part args)
    , few : List (Part args)
    , many : List (Part args)
    , other : List (Part args)
    }


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



---- NUMBER FORMATTING


type alias NumberSymbols =
    { decimal : String
    , group : String
    , list : String
    , percentSign : String
    , plusSign : String
    , minusSign : String
    , exponential : String
    , superscriptingExponent : String
    , perMille : String
    , infinity : String
    , nan : String
    , timeSeparator : String
    }


type alias NumberFormats =
    { symbols : NumberSymbols
    , standard : NumberFormat
    , percent : NumberFormat
    , scientific : NumberFormat
    }


type alias NumberFormat =
    { positivePattern : NumberPattern
    , negativePattern : Maybe NumberPattern
    }


type alias NumberPattern =
    { prefix : String
    , suffix : String
    , primaryGroupingSize : Maybe Int
    , secondaryGroupingSize : Maybe Int
    , minimalIntegerCount : Int
    , minimalFractionCount : Int
    , maximalFractionCount : Int
    }



---- SHARED PARTS


{-| -}
customDecimal : (args -> Float) -> NumberSymbols -> NumberFormat -> Part args
customDecimal accessor numberSymbols numberFormat =
    let
        printer num =
            if num >= 0 then
                printWithPattern numberFormat.positivePattern num
            else
                case numberFormat.negativePattern of
                    Just negativePattern ->
                        printWithPattern negativePattern num

                    Nothing ->
                        printWithPattern numberFormat.positivePattern num

        printWithPattern pattern num =
            [ pattern.prefix
            , if num < 0 then
                numberSymbols.minusSign
              else
                ""
            , integerPart pattern num
            , case fractionalPart pattern num of
                "" ->
                    ""

                part ->
                    [ numberSymbols.decimal
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
    Float accessor printer


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
