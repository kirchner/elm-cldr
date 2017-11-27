module Localized.Iw
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
        , many : List (Part args)
        , other : List (Part args)
        }
    -> Part args
cardinal accessor { one, two, many, other } =
    customPlural accessor
        (toString >> cardinalSelector)
        { zero = []
        , one = one
        , two = two
        , few = []
        , many = many
        , other = other
        }


cardinalDynamic :
    (args -> Float)
    ->
        { one : List (Part args)
        , two : List (Part args)
        , many : List (Part args)
        , other : List (Part args)
        }
    -> Part args
cardinalDynamic accessor { one, two, many, other } =
    dynamicPlural accessor
        cardinalPluralRules
        { zero = []
        , one = one
        , two = two
        , few = []
        , many = many
        , other = other
        }


ordinal :
    (args -> Float)
    ->
        { other : List (Part args)
        }
    -> Part args
ordinal accessor { other } =
    customPlural accessor
        (toString >> ordinalSelector)
        { zero = []
        , one = []
        , two = []
        , few = []
        , many = []
        , other = other
        }


ordinalDynamic :
    (args -> Float)
    ->
        { other : List (Part args)
        }
    -> Part args
ordinalDynamic accessor { other } =
    dynamicPlural accessor
        ordinalPluralRules
        { zero = []
        , one = []
        , two = []
        , few = []
        , many = []
        , other = other
        }


cardinalPluralRules : PluralRules
cardinalPluralRules =
    { zero = Nothing
    , one =
        Just
            (And
                (Equal
                    (Simple IntegerDigits)
                    [ Single 1 ]
                )
                (Equal
                    (Simple
                        (FractionDigitCount WithTrailingZeros)
                    )
                    [ Single 0 ]
                )
            )
    , two =
        Just
            (And
                (Equal
                    (Simple IntegerDigits)
                    [ Single 2 ]
                )
                (Equal
                    (Simple
                        (FractionDigitCount WithTrailingZeros)
                    )
                    [ Single 0 ]
                )
            )
    , few = Nothing
    , many =
        Just
            (And
                (Equal
                    (Simple
                        (FractionDigitCount WithTrailingZeros)
                    )
                    [ Single 0 ]
                )
                (And
                    (NotEqual
                        (Simple AbsoluteValue)
                        [ Range 0 10 ]
                    )
                    (Equal
                        (Modulo AbsoluteValue 10)
                        [ Single 0 ]
                    )
                )
            )
    }


cardinalSelector :
    String
    -> PluralCase
cardinalSelector count =
    if
        (integerDigits '.' count == 1)
            && (fractionDigitCount '.' WithTrailingZeros count == 0)
    then
        One
    else if
        (integerDigits '.' count == 2)
            && (fractionDigitCount '.' WithTrailingZeros count == 0)
    then
        Two
    else if
        (fractionDigitCount '.' WithTrailingZeros count == 0)
            && (((absoluteValue '.' count > 0) && (absoluteValue '.' count < 10))
                    && (floor (absoluteValue '.' count) % 10 == 0)
               )
    then
        Many
    else
        Other


ordinalPluralRules : PluralRules
ordinalPluralRules =
    { zero = Nothing, one = Nothing, two = Nothing, few = Nothing, many = Nothing }


ordinalSelector :
    String
    -> PluralCase
ordinalSelector count =
    Other
