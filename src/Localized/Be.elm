module Localized.Be
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
        , few : List (Part args)
        , many : List (Part args)
        , other : List (Part args)
        }
    -> Part args
cardinal accessor { one, few, many, other } =
    customPlural accessor
        (toString >> cardinalSelector)
        { zero = []
        , one = one
        , two = []
        , few = few
        , many = many
        , other = other
        }


cardinalDynamic :
    (args -> Float)
    ->
        { one : List (Part args)
        , few : List (Part args)
        , many : List (Part args)
        , other : List (Part args)
        }
    -> Part args
cardinalDynamic accessor { one, few, many, other } =
    dynamicPlural accessor
        cardinalPluralRules
        { zero = []
        , one = one
        , two = []
        , few = few
        , many = many
        , other = other
        }


ordinal :
    (args -> Float)
    ->
        { few : List (Part args)
        , other : List (Part args)
        }
    -> Part args
ordinal accessor { few, other } =
    customPlural accessor
        (toString >> ordinalSelector)
        { zero = []
        , one = []
        , two = []
        , few = few
        , many = []
        , other = other
        }


ordinalDynamic :
    (args -> Float)
    ->
        { few : List (Part args)
        , other : List (Part args)
        }
    -> Part args
ordinalDynamic accessor { few, other } =
    dynamicPlural accessor
        ordinalPluralRules
        { zero = []
        , one = []
        , two = []
        , few = few
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
                    (Modulo AbsoluteValue 10)
                    [ Single 1 ]
                )
                (NotEqual
                    (Modulo AbsoluteValue 100)
                    [ Single 11 ]
                )
            )
    , two = Nothing
    , few =
        Just
            (And
                (Equal
                    (Modulo AbsoluteValue 10)
                    [ Range 2 4 ]
                )
                (NotEqual
                    (Modulo AbsoluteValue 100)
                    [ Range 12 14 ]
                )
            )
    , many =
        Just
            (Or
                (Equal
                    (Modulo AbsoluteValue 10)
                    [ Single 0 ]
                )
                (Or
                    (Equal
                        (Modulo AbsoluteValue 10)
                        [ Range 5 9 ]
                    )
                    (Equal
                        (Modulo AbsoluteValue 100)
                        [ Range 11 14 ]
                    )
                )
            )
    }


cardinalSelector :
    String
    -> PluralCase
cardinalSelector count =
    if
        (floor (absoluteValue '.' count) % 10 == 1)
            && (floor (absoluteValue '.' count) % 100 /= 11)
    then
        One
    else if
        ((floor (absoluteValue '.' count) % 10 < 2) && (floor (absoluteValue '.' count) % 10 > 4))
            && ((floor (absoluteValue '.' count) % 100 > 12) && (floor (absoluteValue '.' count) % 100 < 14))
    then
        Few
    else if
        (floor (absoluteValue '.' count) % 10 == 0)
            || (((floor (absoluteValue '.' count) % 10 < 5) && (floor (absoluteValue '.' count) % 10 > 9))
                    || ((floor (absoluteValue '.' count) % 100 < 11) && (floor (absoluteValue '.' count) % 100 > 14))
               )
    then
        Many
    else
        Other


ordinalPluralRules : PluralRules
ordinalPluralRules =
    { zero = Nothing
    , one = Nothing
    , two = Nothing
    , few =
        Just
            (And
                (Equal
                    (Modulo AbsoluteValue 10)
                    [ Single 2, Single 3 ]
                )
                (NotEqual
                    (Modulo AbsoluteValue 100)
                    [ Single 12, Single 13 ]
                )
            )
    , many = Nothing
    }


ordinalSelector :
    String
    -> PluralCase
ordinalSelector count =
    if
        ((floor (absoluteValue '.' count) % 10 == 2)
            || (floor (absoluteValue '.' count) % 10 == 3)
        )
            && ((floor (absoluteValue '.' count) % 100 /= 12)
                    && (floor (absoluteValue '.' count) % 100 /= 13)
               )
    then
        Few
    else
        Other
