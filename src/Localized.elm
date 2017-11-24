module Localized
    exposing
        ( NumberFormat
        , NumberPattern
        , NumberSymbols
        , Part
        , count
        , customDecimal
        , customPlural
        , print
        , s
        , string
        )

import Internal.Localized exposing (..)


type Part args
    = Verbatim String
    | String (args -> String)
    | Float (args -> Float) (Float -> String)
    | Plural (args -> Float) (Float -> PluralCase) (AllPluralCases args)
    | Count


type alias AllPluralCases args =
    { zero : List (Part args)
    , one : List (Part args)
    , two : List (Part args)
    , few : List (Part args)
    , many : List (Part args)
    , other : List (Part args)
    }


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


s : String -> Part args
s text =
    Verbatim text


string : (args -> String) -> Part args
string accessor =
    String accessor


count : Part args
count =
    Count


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


customPlural :
    (args -> Float)
    -> (Float -> PluralCase)
    ->
        { zero : List (Part args)
        , one : List (Part args)
        , two : List (Part args)
        , few : List (Part args)
        , many : List (Part args)
        , other : List (Part args)
        }
    -> Part args
customPlural accessor selector cases =
    Plural accessor selector cases



---- PRINT


print : args -> List (Part args) -> String
print args parts =
    parts
        |> List.map (printPart args)
        |> String.concat


printWithCount : String -> args -> List (Part args) -> String
printWithCount count args parts =
    parts
        |> List.map (printPartWithCount count args)
        |> String.concat


printPart : args -> Part args -> String
printPart args part =
    case part of
        Verbatim text ->
            text

        String accessor ->
            accessor args

        Float accessor printer ->
            args |> accessor |> printer

        Plural accessor selector cases ->
            let
                count =
                    -- TODO: insert actual number printing
                    args |> accessor |> toString
            in
            case args |> accessor |> selector of
                Zero ->
                    cases.zero
                        |> printWithCount count args

                One ->
                    cases.one
                        |> printWithCount count args

                Two ->
                    cases.two
                        |> printWithCount count args

                Few ->
                    cases.few
                        |> printWithCount count args

                Many ->
                    cases.many
                        |> printWithCount count args

                Other ->
                    cases.other
                        |> printWithCount count args

        Count ->
            Debug.crash "no count given"


printPartWithCount : String -> args -> Part args -> String
printPartWithCount count args part =
    case part of
        Verbatim text ->
            text

        String accessor ->
            accessor args

        Float accessor printer ->
            args |> accessor |> printer

        Plural accessor selector cases ->
            case args |> accessor |> selector of
                Zero ->
                    cases.zero
                        |> print args

                One ->
                    cases.one
                        |> print args

                Two ->
                    cases.two
                        |> print args

                Few ->
                    cases.few
                        |> print args

                Many ->
                    cases.many
                        |> print args

                Other ->
                    cases.other
                        |> print args

        Count ->
            count