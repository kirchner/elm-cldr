module Localized.Gd
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
        { one : List (Part args msg)
        , two : List (Part args msg)
        , few : List (Part args msg)
        , other : List (Part args msg)
        }
    -> Part args msg
ordinal accessor { one, two, few, other } =
    Localized.customPlural accessor
        (Localized.customNumberFormat toString)
        ordinalSelector
        { zero = []
        , one = one
        , two = two
        , few = few
        , many = []
        , other = other
        }


cardinalPluralRules : PluralRules
cardinalPluralRules =
    { zero = Nothing
    , one =
        Just
            (Equal
                (Simple AbsoluteValue)
                [ Single 1, Single 11 ]
            )
    , two =
        Just
            (Equal
                (Simple AbsoluteValue)
                [ Single 2, Single 12 ]
            )
    , few =
        Just
            (Equal
                (Simple AbsoluteValue)
                [ Range 3 10, Range 13 19 ]
            )
    , many = Nothing
    }


cardinalSelector :
    String
    -> PluralCase
cardinalSelector count =
    if
        (absoluteValue '.' count == 1)
            || (absoluteValue '.' count == 11)
    then
        One
    else if
        (absoluteValue '.' count == 2)
            || (absoluteValue '.' count == 12)
    then
        Two
    else if
        ((absoluteValue '.' count < 3) && (absoluteValue '.' count > 10))
            || ((absoluteValue '.' count < 13) && (absoluteValue '.' count > 19))
    then
        Few
    else
        Other


ordinalSelector =
    cardinalSelector
