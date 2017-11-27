module Localized.Lv
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
    , nan = "NS"
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
        { zero : List (Part args)
        , one : List (Part args)
        , other : List (Part args)
        }
    -> Part args
cardinal accessor { zero, one, other } =
    customPlural accessor
        (toString >> cardinalSelector)
        { zero = zero
        , one = one
        , two = []
        , few = []
        , many = []
        , other = other
        }


cardinalDynamic :
    (args -> Float)
    ->
        { zero : List (Part args)
        , one : List (Part args)
        , other : List (Part args)
        }
    -> Part args
cardinalDynamic accessor { zero, one, other } =
    dynamicPlural accessor
        cardinalPluralRules
        { zero = zero
        , one = one
        , two = []
        , few = []
        , many = []
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
    { zero =
        Just
            (Or
                (Equal
                    (Modulo AbsoluteValue 10)
                    [ Single 0 ]
                )
                (Or
                    (Equal
                        (Modulo AbsoluteValue 100)
                        [ Range 11 19 ]
                    )
                    (And
                        (Equal
                            (Simple
                                (FractionDigitCount WithTrailingZeros)
                            )
                            [ Single 2 ]
                        )
                        (Equal
                            (Modulo
                                (FractionDigits WithTrailingZeros)
                                100
                            )
                            [ Range 11 19 ]
                        )
                    )
                )
            )
    , one =
        Just
            (Or
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
                (Or
                    (And
                        (Equal
                            (Simple
                                (FractionDigitCount WithTrailingZeros)
                            )
                            [ Single 2 ]
                        )
                        (And
                            (Equal
                                (Modulo
                                    (FractionDigits WithTrailingZeros)
                                    10
                                )
                                [ Single 1 ]
                            )
                            (NotEqual
                                (Modulo
                                    (FractionDigits WithTrailingZeros)
                                    100
                                )
                                [ Single 11 ]
                            )
                        )
                    )
                    (And
                        (NotEqual
                            (Simple
                                (FractionDigitCount WithTrailingZeros)
                            )
                            [ Single 2 ]
                        )
                        (Equal
                            (Modulo
                                (FractionDigits WithTrailingZeros)
                                10
                            )
                            [ Single 1 ]
                        )
                    )
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
        (floor (absoluteValue '.' count) % 10 == 0)
            || (((floor (absoluteValue '.' count) % 100 < 11) && (floor (absoluteValue '.' count) % 100 > 19))
                    || ((fractionDigitCount '.' WithTrailingZeros count == 2)
                            && ((fractionDigits '.' WithTrailingZeros count % 100 < 11) && (fractionDigits '.' WithTrailingZeros count % 100 > 19))
                       )
               )
    then
        Zero
    else if
        ((floor (absoluteValue '.' count) % 10 == 1)
            && (floor (absoluteValue '.' count) % 100 /= 11)
        )
            || (((fractionDigitCount '.' WithTrailingZeros count == 2)
                    && ((fractionDigits '.' WithTrailingZeros count % 10 == 1)
                            && (fractionDigits '.' WithTrailingZeros count % 100 /= 11)
                       )
                )
                    || ((fractionDigitCount '.' WithTrailingZeros count /= 2)
                            && (fractionDigits '.' WithTrailingZeros count % 10 == 1)
                       )
               )
    then
        One
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
