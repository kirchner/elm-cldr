module Localized.Tl
    exposing
        ( cardinal
        , ordinal
        )

{-|

@docs cardinal, ordinal

-}

import Internal.Numbers exposing (..)
import Internal.PluralRules exposing (..)
import Localized exposing (PluralCase(..), Text)


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


{-| -}
cardinal :
    (args -> Float)
    ->
        { one : List (Text args msg)
        , other : List (Text args msg)
        }
    -> Text args msg
cardinal accessor { one, other } =
    Localized.customPlural accessor
        (Localized.customNumberFormat toString)
        cardinalSelector
        { zero = []
        , one = one
        , two = []
        , few = []
        , many = []
        , other = other
        }


{-| -}
ordinal :
    (args -> Float)
    ->
        { one : List (Text args msg)
        , other : List (Text args msg)
        }
    -> Text args msg
ordinal accessor { one, other } =
    Localized.customPlural accessor
        (Localized.customNumberFormat toString)
        ordinalSelector
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
            (Or
                (And
                    (Equal
                        (Simple
                            (FractionDigitCount WithTrailingZeros)
                        )
                        [ Single 0 ]
                    )
                    (Equal
                        (Simple IntegerDigits)
                        [ Single 1, Single 2, Single 3 ]
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
                        (NotEqual
                            (Modulo IntegerDigits 10)
                            [ Single 4, Single 6, Single 9 ]
                        )
                    )
                    (And
                        (NotEqual
                            (Simple
                                (FractionDigitCount WithTrailingZeros)
                            )
                            [ Single 0 ]
                        )
                        (NotEqual
                            (Modulo
                                (FractionDigits WithTrailingZeros)
                                10
                            )
                            [ Single 4, Single 6, Single 9 ]
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
        ((fractionDigitCount '.' WithTrailingZeros count == 0)
            && ((integerDigits '.' count == 1)
                    || (integerDigits '.' count == 2)
                    || (integerDigits '.' count == 3)
               )
        )
            || (((fractionDigitCount '.' WithTrailingZeros count == 0)
                    && ((integerDigits '.' count % 10 /= 4)
                            && (integerDigits '.' count % 10 /= 6)
                            && (integerDigits '.' count % 10 /= 9)
                       )
                )
                    || ((fractionDigitCount '.' WithTrailingZeros count /= 0)
                            && ((fractionDigits '.' WithTrailingZeros count % 10 /= 4)
                                    && (fractionDigits '.' WithTrailingZeros count % 10 /= 6)
                                    && (fractionDigits '.' WithTrailingZeros count % 10 /= 9)
                               )
                       )
               )
    then
        One
    else
        Other


ordinalPluralRules : PluralRules
ordinalPluralRules =
    { zero = Nothing
    , one =
        Just
            (Equal
                (Simple AbsoluteValue)
                [ Single 1 ]
            )
    , two = Nothing
    , few = Nothing
    , many = Nothing
    }


ordinalSelector :
    String
    -> PluralCase
ordinalSelector count =
    if absoluteValue '.' count == 1 then
        One
    else
        Other
