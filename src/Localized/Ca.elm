module Localized.Ca
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


{-| -}
cardinal :
    (args -> Float)
    ->
        { one : Text args msg
        , other : Text args msg
        }
    -> Text args msg
cardinal accessor { one, other } =
    Localized.customPlural accessor
        (Localized.customNumberFormat toString)
        cardinalSelector
        { zero = concat []
        , one = one
        , two = concat []
        , few = concat []
        , many = concat []
        , other = other
        }


{-| -}
ordinal :
    (args -> Float)
    ->
        { one : Text args msg
        , two : Text args msg
        , few : Text args msg
        , other : Text args msg
        }
    -> Text args msg
ordinal accessor { one, two, few, other } =
    Localized.customPlural accessor
        (Localized.customNumberFormat toString)
        ordinalSelector
        { zero = concat []
        , one = one
        , two = two
        , few = few
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
            (Equal
                (Simple AbsoluteValue)
                [ Single 1, Single 3 ]
            )
    , two =
        Just
            (Equal
                (Simple AbsoluteValue)
                [ Single 2 ]
            )
    , few =
        Just
            (Equal
                (Simple AbsoluteValue)
                [ Single 4 ]
            )
    , many = Nothing
    }


ordinalSelector :
    String
    -> PluralCase
ordinalSelector count =
    if
        (absoluteValue '.' count == 1)
            || (absoluteValue '.' count == 3)
    then
        One
    else if absoluteValue '.' count == 2 then
        Two
    else if absoluteValue '.' count == 4 then
        Few
    else
        Other
