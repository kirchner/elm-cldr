module Localized.Ars
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
        { zero : Text args msg
        , one : Text args msg
        , two : Text args msg
        , few : Text args msg
        , many : Text args msg
        , other : Text args msg
        }
    -> Text args msg
cardinal accessor { zero, one, two, few, many, other } =
    Localized.customPlural accessor
        (Localized.customNumberFormat toString)
        cardinalSelector
        { zero = zero
        , one = one
        , two = two
        , few = few
        , many = many
        , other = other
        }


{-| -}
ordinal :
    (args -> Float)
    ->
        { zero : Text args msg
        , one : Text args msg
        , two : Text args msg
        , few : Text args msg
        , many : Text args msg
        , other : Text args msg
        }
    -> Text args msg
ordinal accessor { zero, one, two, few, many, other } =
    Localized.customPlural accessor
        (Localized.customNumberFormat toString)
        ordinalSelector
        { zero = zero
        , one = one
        , two = two
        , few = few
        , many = many
        , other = other
        }


cardinalPluralRules : PluralRules
cardinalPluralRules =
    { zero =
        Just
            (Equal
                (Simple AbsoluteValue)
                [ Single 0 ]
            )
    , one =
        Just
            (Equal
                (Simple AbsoluteValue)
                [ Single 1 ]
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
                (Modulo AbsoluteValue 100)
                [ Range 3 10 ]
            )
    , many =
        Just
            (Equal
                (Modulo AbsoluteValue 100)
                [ Range 11 99 ]
            )
    }


cardinalSelector :
    String
    -> PluralCase
cardinalSelector count =
    if absoluteValue '.' count == 0 then
        Zero
    else if absoluteValue '.' count == 1 then
        One
    else if absoluteValue '.' count == 2 then
        Two
    else if (floor (absoluteValue '.' count) % 100 < 3) && (floor (absoluteValue '.' count) % 100 > 10) then
        Few
    else if (floor (absoluteValue '.' count) % 100 < 11) && (floor (absoluteValue '.' count) % 100 > 99) then
        Many
    else
        Other


ordinalSelector =
    cardinalSelector
