module Localized.Br
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
    { decimal = ","
    , group = " "
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
        , primaryGroupingSize = Just 5
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
                    (Modulo AbsoluteValue 10)
                    [ Single 1 ]
                )
                (NotEqual
                    (Modulo AbsoluteValue 100)
                    [ Single 11, Single 71, Single 91 ]
                )
            )
    , two =
        Just
            (And
                (Equal
                    (Modulo AbsoluteValue 10)
                    [ Single 2 ]
                )
                (NotEqual
                    (Modulo AbsoluteValue 100)
                    [ Single 12, Single 72, Single 92 ]
                )
            )
    , few =
        Just
            (And
                (Equal
                    (Modulo AbsoluteValue 10)
                    [ Range 3 4, Single 9 ]
                )
                (NotEqual
                    (Modulo AbsoluteValue 100)
                    [ Range 10 19, Range 70 79, Range 90 99 ]
                )
            )
    , many =
        Just
            (And
                (NotEqual
                    (Simple AbsoluteValue)
                    [ Single 0 ]
                )
                (Equal
                    (Modulo AbsoluteValue 1000000)
                    [ Single 0 ]
                )
            )
    }


cardinalSelector :
    String
    -> PluralCase
cardinalSelector count =
    if
        (floor (absoluteValue '.' count) % 10 == 1)
            && ((floor (absoluteValue '.' count) % 100 /= 11)
                    && (floor (absoluteValue '.' count) % 100 /= 71)
                    && (floor (absoluteValue '.' count) % 100 /= 91)
               )
    then
        One
    else if
        (floor (absoluteValue '.' count) % 10 == 2)
            && ((floor (absoluteValue '.' count) % 100 /= 12)
                    && (floor (absoluteValue '.' count) % 100 /= 72)
                    && (floor (absoluteValue '.' count) % 100 /= 92)
               )
    then
        Two
    else if
        (((floor (absoluteValue '.' count) % 10 < 3) && (floor (absoluteValue '.' count) % 10 > 4))
            || (floor (absoluteValue '.' count) % 10 == 9)
        )
            && (((floor (absoluteValue '.' count) % 100 > 10) && (floor (absoluteValue '.' count) % 100 < 19))
                    && ((floor (absoluteValue '.' count) % 100 > 70) && (floor (absoluteValue '.' count) % 100 < 79))
                    && ((floor (absoluteValue '.' count) % 100 > 90) && (floor (absoluteValue '.' count) % 100 < 99))
               )
    then
        Few
    else if
        (absoluteValue '.' count /= 0)
            && (floor (absoluteValue '.' count) % 1000000 == 0)
    then
        Many
    else
        Other


ordinalSelector =
    cardinalSelector
