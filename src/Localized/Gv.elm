module Localized.Gv
    exposing
        ( cardinal
        , cardinalDynamic
        , decimal
        , ordinal
        , ordinalDynamic
        )

import Localized exposing (..)


numberSymbols : NumberSymbols
numberSymbols =
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


standardNumberFormat : NumberFormat
standardNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 3
        , secondaryGroupingSize = Nothing
        , minimalIntegerCount = 1
        , minimalFractionCount = 0
        , maximalFractionCount = 3
        }
    , negativePattern = Nothing
    }


percentNumberFormat : NumberFormat
percentNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 4
        , secondaryGroupingSize = Nothing
        , minimalIntegerCount = 1
        , minimalFractionCount = 0
        , maximalFractionCount = 0
        }
    , negativePattern = Nothing
    }


scientificNumberFormat : NumberFormat
scientificNumberFormat =
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


decimal :
    (args -> Float)
    -> Part args
decimal accessor =
    customDecimal accessor numberSymbols standardNumberFormat


cardinal :
    (args -> Float)
    ->
        { one : List (Part args)
        , two : List (Part args)
        , few : List (Part args)
        , many : List (Part args)
        , other : List (Part args)
        }
    -> Part args
cardinal accessor { one, two, few, many, other } =
    customPlural accessor
        (toString >> cardinalSelector)
        { zero = []
        , one = one
        , two = two
        , few = few
        , many = many
        , other = other
        }


cardinalDynamic :
    (args -> Float)
    ->
        { one : List (Part args)
        , two : List (Part args)
        , few : List (Part args)
        , many : List (Part args)
        , other : List (Part args)
        }
    -> Part args
cardinalDynamic accessor { one, two, few, many, other } =
    dynamicPlural accessor
        cardinalPluralRules
        { zero = []
        , one = one
        , two = two
        , few = few
        , many = many
        , other = other
        }


ordinal =
    cardinal


cardinalPluralRules : PluralRules
cardinalPluralRules =
    { zero = Nothing
    , one =
        Just
            (And
                (Equal
                    (Simple
                        (FractionDigitCount WithTrailingZeros)
                    )
                    [ Single 0 ]
                )
                (Equal
                    (Modulo IntegerDigits 10)
                    [ Single 1 ]
                )
            )
    , two =
        Just
            (And
                (Equal
                    (Simple
                        (FractionDigitCount WithTrailingZeros)
                    )
                    [ Single 0 ]
                )
                (Equal
                    (Modulo IntegerDigits 10)
                    [ Single 2 ]
                )
            )
    , few =
        Just
            (And
                (Equal
                    (Simple
                        (FractionDigitCount WithTrailingZeros)
                    )
                    [ Single 0 ]
                )
                (Equal
                    (Modulo IntegerDigits 100)
                    [ Single 0, Single 20, Single 40, Single 60, Single 80 ]
                )
            )
    , many =
        Just
            (NotEqual
                (Simple
                    (FractionDigitCount WithTrailingZeros)
                )
                [ Single 0 ]
            )
    }


cardinalSelector :
    String
    -> PluralCase
cardinalSelector count =
    if
        (fractionDigitCount '.' WithTrailingZeros count == 0)
            && (integerDigits '.' count % 10 == 1)
    then
        One
    else if
        (fractionDigitCount '.' WithTrailingZeros count == 0)
            && (integerDigits '.' count % 10 == 2)
    then
        Two
    else if
        (fractionDigitCount '.' WithTrailingZeros count == 0)
            && ((integerDigits '.' count % 100 == 0)
                    || (integerDigits '.' count % 100 == 20)
                    || (integerDigits '.' count % 100 == 40)
                    || (integerDigits '.' count % 100 == 60)
                    || (integerDigits '.' count % 100 == 80)
               )
    then
        Few
    else if fractionDigitCount '.' WithTrailingZeros count /= 0 then
        Many
    else
        Other


ordinalSelector =
    cardinalSelector
