module Localized.Dsb
    exposing
        ( cardinal
        , ordinal
        )

{-|

@docs cardinal, ordinal

-}

import Internal.Numbers exposing (..)
import Internal.PluralRules exposing (..)
import Localized exposing (Part, PluralCase(..))


numberSymbols : NumberSymbols
numberSymbols =
    { decimal = ","
    , group = "."
    , list = ";"
    , percentSign = "%"
    , plusSign = "+"
    , minusSign = "-"
    , exponential = "E"
    , superscriptingExponent = "·"
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


{-| -}
cardinal :
    (args -> Float)
    ->
        { one : List (Part args msg)
        , two : List (Part args msg)
        , few : List (Part args msg)
        , other : List (Part args msg)
        }
    -> Part args msg
cardinal accessor { one, two, few, other } =
    Localized.customPlural accessor
        (Localized.customNumberFormat toString)
        cardinalSelector
        { zero = []
        , one = one
        , two = two
        , few = few
        , many = []
        , other = other
        }


{-| -}
ordinal :
    (args -> Float)
    ->
        { other : List (Part args msg)
        }
    -> Part args msg
ordinal accessor { other } =
    Localized.customPlural accessor
        (Localized.customNumberFormat toString)
        ordinalSelector
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
            (Or
                (And
                    (Equal
                        (Simple
                            (FractionDigitCount WithTrailingZeros)
                        )
                        [ Single 0 ]
                    )
                    (Equal
                        (Modulo IntegerDigits 100)
                        [ Single 1 ]
                    )
                )
                (Equal
                    (Modulo
                        (FractionDigits WithTrailingZeros)
                        100
                    )
                    [ Single 1 ]
                )
            )
    , two =
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
                        (Modulo IntegerDigits 100)
                        [ Single 2 ]
                    )
                )
                (Equal
                    (Modulo
                        (FractionDigits WithTrailingZeros)
                        100
                    )
                    [ Single 2 ]
                )
            )
    , few =
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
                        (Modulo IntegerDigits 100)
                        [ Range 3 4 ]
                    )
                )
                (Equal
                    (Modulo
                        (FractionDigits WithTrailingZeros)
                        100
                    )
                    [ Range 3 4 ]
                )
            )
    , many = Nothing
    }


cardinalSelector :
    String
    -> PluralCase
cardinalSelector count =
    if
        ((fractionDigitCount '.' WithTrailingZeros count == 0)
            && (integerDigits '.' count % 100 == 1)
        )
            || (fractionDigits '.' WithTrailingZeros count % 100 == 1)
    then
        One
    else if
        ((fractionDigitCount '.' WithTrailingZeros count == 0)
            && (integerDigits '.' count % 100 == 2)
        )
            || (fractionDigits '.' WithTrailingZeros count % 100 == 2)
    then
        Two
    else if
        ((fractionDigitCount '.' WithTrailingZeros count == 0)
            && ((integerDigits '.' count % 100 < 3) && (integerDigits '.' count % 100 > 4))
        )
            || ((fractionDigits '.' WithTrailingZeros count % 100 < 3) && (fractionDigits '.' WithTrailingZeros count % 100 > 4))
    then
        Few
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
