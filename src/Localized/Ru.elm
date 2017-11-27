module Localized.Ru
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
    , nan = "не число"
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
                    (Simple
                        (FractionDigitCount WithTrailingZeros)
                    )
                    [ Single 0 ]
                )
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
            )
    , two = Nothing
    , few =
        Just
            (And
                (Equal
                    (Simple
                        (FractionDigitCount WithTrailingZeros)
                    )
                    [ Single 0 ]
                )
                (And
                    (Equal
                        (Modulo IntegerDigits 10)
                        [ Range 2 4 ]
                    )
                    (NotEqual
                        (Modulo IntegerDigits 100)
                        [ Range 12 14 ]
                    )
                )
            )
    , many =
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
                        [ Single 0 ]
                    )
                )
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
                            [ Range 5 9 ]
                        )
                    )
                    (And
                        (Equal
                            (Simple
                                (FractionDigitCount WithTrailingZeros)
                            )
                            [ Single 0 ]
                        )
                        (Equal
                            (Modulo IntegerDigits 100)
                            [ Range 11 14 ]
                        )
                    )
                )
            )
    }


cardinalSelector :
    String
    -> PluralCase
cardinalSelector count =
    if
        (fractionDigitCount '.' WithTrailingZeros count == 0)
            && ((integerDigits '.' count % 10 == 1)
                    && (integerDigits '.' count % 100 /= 11)
               )
    then
        One
    else if
        (fractionDigitCount '.' WithTrailingZeros count == 0)
            && (((integerDigits '.' count % 10 < 2) && (integerDigits '.' count % 10 > 4))
                    && ((integerDigits '.' count % 100 > 12) && (integerDigits '.' count % 100 < 14))
               )
    then
        Few
    else if
        ((fractionDigitCount '.' WithTrailingZeros count == 0)
            && (integerDigits '.' count % 10 == 0)
        )
            || (((fractionDigitCount '.' WithTrailingZeros count == 0)
                    && ((integerDigits '.' count % 10 < 5) && (integerDigits '.' count % 10 > 9))
                )
                    || ((fractionDigitCount '.' WithTrailingZeros count == 0)
                            && ((integerDigits '.' count % 100 < 11) && (integerDigits '.' count % 100 > 14))
                       )
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
