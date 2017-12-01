module Localized.Ar
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
    , percentSign = "\x200E%\x200E"
    , plusSign = "\x200E+"
    , minusSign = "\x200E-"
    , exponential = "E"
    , superscriptingExponent = "×"
    , perMille = "‰"
    , infinity = "∞"
    , nan = "ليس رقمًا"
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
        { zero : List (Part args msg)
        , one : List (Part args msg)
        , two : List (Part args msg)
        , few : List (Part args msg)
        , many : List (Part args msg)
        , other : List (Part args msg)
        }
    -> Part args msg
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


ordinalPluralRules : PluralRules
ordinalPluralRules =
    { zero = Nothing, one = Nothing, two = Nothing, few = Nothing, many = Nothing }


ordinalSelector :
    String
    -> PluralCase
ordinalSelector count =
    Other
