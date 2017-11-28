module Localized.Mk
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
    , group = "."
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
    -> Part args msg
decimal accessor =
    customDecimal accessor numberSymbols standardNumberFormat


cardinal :
    (args -> Float)
    ->
        { one : List (Part args msg)
        , other : List (Part args msg)
        }
    -> Part args msg
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
        { one : List (Part args msg)
        , other : List (Part args msg)
        }
    -> Part args msg
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
        { one : List (Part args msg)
        , two : List (Part args msg)
        , many : List (Part args msg)
        , other : List (Part args msg)
        }
    -> Part args msg
ordinal accessor { one, two, many, other } =
    customPlural accessor
        (toString >> ordinalSelector)
        { zero = []
        , one = one
        , two = two
        , few = []
        , many = many
        , other = other
        }


ordinalDynamic :
    (args -> Float)
    ->
        { one : List (Part args msg)
        , two : List (Part args msg)
        , many : List (Part args msg)
        , other : List (Part args msg)
        }
    -> Part args msg
ordinalDynamic accessor { one, two, many, other } =
    dynamicPlural accessor
        ordinalPluralRules
        { zero = []
        , one = one
        , two = two
        , few = []
        , many = many
        , other = other
        }


cardinalPluralRules : PluralRules
cardinalPluralRules =
    { zero = Nothing
    , one =
        Just
            (Or
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
                (Equal
                    (Modulo
                        (FractionDigits WithTrailingZeros)
                        10
                    )
                    [ Single 1 ]
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
        ((fractionDigitCount '.' WithTrailingZeros count == 0)
            && (integerDigits '.' count % 10 == 1)
        )
            || (fractionDigits '.' WithTrailingZeros count % 10 == 1)
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
                    (Modulo IntegerDigits 10)
                    [ Single 1 ]
                )
                (NotEqual
                    (Modulo IntegerDigits 100)
                    [ Single 11 ]
                )
            )
    , two =
        Just
            (And
                (Equal
                    (Modulo IntegerDigits 10)
                    [ Single 2 ]
                )
                (NotEqual
                    (Modulo IntegerDigits 100)
                    [ Single 12 ]
                )
            )
    , few = Nothing
    , many =
        Just
            (And
                (Equal
                    (Modulo IntegerDigits 10)
                    [ Single 7, Single 8 ]
                )
                (NotEqual
                    (Modulo IntegerDigits 100)
                    [ Single 17, Single 18 ]
                )
            )
    }


ordinalSelector :
    String
    -> PluralCase
ordinalSelector count =
    if
        (integerDigits '.' count % 10 == 1)
            && (integerDigits '.' count % 100 /= 11)
    then
        One
    else if
        (integerDigits '.' count % 10 == 2)
            && (integerDigits '.' count % 100 /= 12)
    then
        Two
    else if
        ((integerDigits '.' count % 10 == 7)
            || (integerDigits '.' count % 10 == 8)
        )
            && ((integerDigits '.' count % 100 /= 17)
                    && (integerDigits '.' count % 100 /= 18)
               )
    then
        Many
    else
        Other
