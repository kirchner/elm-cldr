module Localized.Lag
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
        { zero : List (Part args msg)
        , one : List (Part args msg)
        , other : List (Part args msg)
        }
    -> Part args msg
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
        { zero : List (Part args msg)
        , one : List (Part args msg)
        , other : List (Part args msg)
        }
    -> Part args msg
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
            (And
                (Equal
                    (Simple IntegerDigits)
                    [ Single 0, Single 1 ]
                )
                (NotEqual
                    (Simple AbsoluteValue)
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
    if absoluteValue '.' count == 0 then
        Zero
    else if
        ((integerDigits '.' count == 0)
            || (integerDigits '.' count == 1)
        )
            && (absoluteValue '.' count /= 0)
    then
        One
    else
        Other


ordinalSelector =
    cardinalSelector