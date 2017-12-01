module Localized.Ro
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


{-| -}
cardinal :
    (args -> Float)
    ->
        { one : List (Part args msg)
        , few : List (Part args msg)
        , other : List (Part args msg)
        }
    -> Part args msg
cardinal accessor { one, few, other } =
    Localized.customPlural accessor
        (Localized.customNumberFormat toString)
        cardinalSelector
        { zero = []
        , one = one
        , two = []
        , few = few
        , many = []
        , other = other
        }


{-| -}
ordinal :
    (args -> Float)
    ->
        { one : List (Part args msg)
        , other : List (Part args msg)
        }
    -> Part args msg
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
            (Or
                (NotEqual
                    (Simple
                        (FractionDigitCount WithTrailingZeros)
                    )
                    [ Single 0 ]
                )
                (Or
                    (Equal
                        (Simple AbsoluteValue)
                        [ Single 0 ]
                    )
                    (And
                        (NotEqual
                            (Simple AbsoluteValue)
                            [ Single 1 ]
                        )
                        (Equal
                            (Modulo AbsoluteValue 100)
                            [ Range 1 19 ]
                        )
                    )
                )
            )
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
    else if
        (fractionDigitCount '.' WithTrailingZeros count /= 0)
            || ((absoluteValue '.' count == 0)
                    || ((absoluteValue '.' count /= 1)
                            && ((floor (absoluteValue '.' count) % 100 < 1) && (floor (absoluteValue '.' count) % 100 > 19))
                       )
               )
    then
        Few
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
