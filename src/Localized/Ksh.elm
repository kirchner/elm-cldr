module Localized.Ksh
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
    , nan = "¤¤¤"
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
        { zero : List (Text args msg)
        , one : List (Text args msg)
        , other : List (Text args msg)
        }
    -> Text args msg
cardinal accessor { zero, one, other } =
    Localized.customPlural accessor
        (Localized.customNumberFormat toString)
        cardinalSelector
        { zero = zero
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
        { zero : List (Text args msg)
        , one : List (Text args msg)
        , other : List (Text args msg)
        }
    -> Text args msg
ordinal accessor { zero, one, other } =
    Localized.customPlural accessor
        (Localized.customNumberFormat toString)
        ordinalSelector
        { zero = zero
        , one = one
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
    , two = Nothing
    , few = Nothing
    , many = Nothing
    }


cardinalSelector :
    String
    -> PluralCase
cardinalSelector count =
    if absoluteValue '.' count == 0 then
        Zero
    else if absoluteValue '.' count == 1 then
        One
    else
        Other


ordinalSelector =
    cardinalSelector
