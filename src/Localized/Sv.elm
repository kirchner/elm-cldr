module Localized.Sv
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
    , minusSign = "−"
    , exponential = "×10^"
    , superscriptingExponent = "×"
    , perMille = "‰"
    , infinity = "∞"
    , nan = "¤¤¤"
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
        , other : List (Part args)
        }
    -> Part args
cardinal accessor { one, other } =
    customPlural accessor
        (toString >> cardinalSelector)
        { zero = []
        , one = one
        , two = []
        , few = []
        , many = []
        , other = other
        }


cardinalDynamic :
    (args -> Float)
    ->
        { one : List (Part args)
        , other : List (Part args)
        }
    -> Part args
cardinalDynamic accessor { one, other } =
    dynamicPlural accessor
        cardinalPluralRules
        { zero = []
        , one = one
        , two = []
        , few = []
        , many = []
        , other = other
        }


ordinal :
    (args -> Float)
    ->
        { one : List (Part args)
        , other : List (Part args)
        }
    -> Part args
ordinal accessor { one, other } =
    customPlural accessor
        (toString >> ordinalSelector)
        { zero = []
        , one = one
        , two = []
        , few = []
        , many = []
        , other = other
        }


ordinalDynamic :
    (args -> Float)
    ->
        { one : List (Part args)
        , other : List (Part args)
        }
    -> Part args
ordinalDynamic accessor { one, other } =
    dynamicPlural accessor
        ordinalPluralRules
        { zero = []
        , one = one
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
    , two = Nothing
    , few = Nothing
    , many = Nothing
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
    else
        Other


ordinalPluralRules : PluralRules
ordinalPluralRules =
    { zero = Nothing
    , one =
        Just
            (And
                (Equal
                    (Modulo AbsoluteValue 10)
                    [ Single 1, Single 2 ]
                )
                (NotEqual
                    (Modulo AbsoluteValue 100)
                    [ Single 11, Single 12 ]
                )
            )
    , two = Nothing
    , few = Nothing
    , many = Nothing
    }


ordinalSelector :
    String
    -> PluralCase
ordinalSelector count =
    if
        ((floor (absoluteValue '.' count) % 10 == 1)
            || (floor (absoluteValue '.' count) % 10 == 2)
        )
            && ((floor (absoluteValue '.' count) % 100 /= 11)
                    && (floor (absoluteValue '.' count) % 100 /= 12)
               )
    then
        One
    else
        Other
