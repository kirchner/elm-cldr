module Localized.Pl
    exposing
        ( cardinal
        , ordinal
        )

{-|

@docs cardinal, ordinal

-}

import Internal.Numbers exposing (..)
import Internal.PluralRules exposing (..)
import Localized exposing (PluralCase(..), Text, concat)


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


{-| -}
cardinal :
    (args -> Float)
    ->
        { one : Text args msg
        , few : Text args msg
        , many : Text args msg
        , other : Text args msg
        }
    -> Text args msg
cardinal accessor { one, few, many, other } =
    Localized.customPlural accessor
        (Localized.customNumberFormat toString)
        cardinalSelector
        { zero = concat []
        , one = one
        , two = concat []
        , few = few
        , many = many
        , other = other
        }


{-| -}
ordinal :
    (args -> Float)
    ->
        { other : Text args msg
        }
    -> Text args msg
ordinal accessor { other } =
    Localized.customPlural accessor
        (Localized.customNumberFormat toString)
        ordinalSelector
        { zero = concat []
        , one = concat []
        , two = concat []
        , few = concat []
        , many = concat []
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
                    (And
                        (NotEqual
                            (Simple IntegerDigits)
                            [ Single 1 ]
                        )
                        (Equal
                            (Modulo IntegerDigits 10)
                            [ Range 0 1 ]
                        )
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
                            [ Range 12 14 ]
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
        (integerDigits '.' count == 1)
            && (fractionDigitCount '.' WithTrailingZeros count == 0)
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
            && ((integerDigits '.' count /= 1)
                    && ((integerDigits '.' count % 10 < 0) && (integerDigits '.' count % 10 > 1))
               )
        )
            || (((fractionDigitCount '.' WithTrailingZeros count == 0)
                    && ((integerDigits '.' count % 10 < 5) && (integerDigits '.' count % 10 > 9))
                )
                    || ((fractionDigitCount '.' WithTrailingZeros count == 0)
                            && ((integerDigits '.' count % 100 < 12) && (integerDigits '.' count % 100 > 14))
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
