module Localized.Lt
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
    , group = " "
    , list = ";"
    , percentSign = "%"
    , plusSign = "+"
    , minusSign = "−"
    , exponential = "×10^"
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
        , many : List (Part args msg)
        , other : List (Part args msg)
        }
    -> Part args msg
cardinal accessor { one, few, many, other } =
    Localized.customPlural accessor
        (Localized.customNumberFormat toString)
        cardinalSelector
        { zero = []
        , one = one
        , two = []
        , few = few
        , many = many
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
            (And
                (Equal
                    (Modulo AbsoluteValue 10)
                    [ Single 1 ]
                )
                (NotEqual
                    (Modulo AbsoluteValue 100)
                    [ Range 11 19 ]
                )
            )
    , two = Nothing
    , few =
        Just
            (And
                (Equal
                    (Modulo AbsoluteValue 10)
                    [ Range 2 9 ]
                )
                (NotEqual
                    (Modulo AbsoluteValue 100)
                    [ Range 11 19 ]
                )
            )
    , many =
        Just
            (NotEqual
                (Simple
                    (FractionDigits WithTrailingZeros)
                )
                [ Single 0 ]
            )
    }


cardinalSelector :
    String
    -> PluralCase
cardinalSelector count =
    if
        (floor (absoluteValue '.' count) % 10 == 1)
            && ((floor (absoluteValue '.' count) % 100 > 11) && (floor (absoluteValue '.' count) % 100 < 19))
    then
        One
    else if
        ((floor (absoluteValue '.' count) % 10 < 2) && (floor (absoluteValue '.' count) % 10 > 9))
            && ((floor (absoluteValue '.' count) % 100 > 11) && (floor (absoluteValue '.' count) % 100 < 19))
    then
        Few
    else if fractionDigits '.' WithTrailingZeros count /= 0 then
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
