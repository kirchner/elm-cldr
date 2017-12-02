module Localized.Kk
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
    , minusSign = "-"
    , exponential = "E"
    , superscriptingExponent = "×"
    , perMille = "‰"
    , infinity = "∞"
    , nan = "сан емес"
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
        { many : List (Text args msg)
        , other : List (Text args msg)
        }
    -> Text args msg
ordinal accessor { many, other } =
    Localized.customPlural accessor
        (Localized.customNumberFormat toString)
        ordinalSelector
        { zero = []
        , one = []
        , two = []
        , few = []
        , many = many
        , other = other
        }


cardinalPluralRules : PluralRules
cardinalPluralRules =
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


cardinalSelector :
    String
    -> PluralCase
cardinalSelector count =
    if absoluteValue '.' count == 1 then
        One
    else
        Other


ordinalPluralRules : PluralRules
ordinalPluralRules =
    { zero = Nothing
    , one = Nothing
    , two = Nothing
    , few = Nothing
    , many =
        Just
            (Or
                (Equal
                    (Modulo AbsoluteValue 10)
                    [ Single 6 ]
                )
                (Or
                    (Equal
                        (Modulo AbsoluteValue 10)
                        [ Single 9 ]
                    )
                    (And
                        (Equal
                            (Modulo AbsoluteValue 10)
                            [ Single 0 ]
                        )
                        (NotEqual
                            (Simple AbsoluteValue)
                            [ Single 0 ]
                        )
                    )
                )
            )
    }


ordinalSelector :
    String
    -> PluralCase
ordinalSelector count =
    if
        (floor (absoluteValue '.' count) % 10 == 6)
            || ((floor (absoluteValue '.' count) % 10 == 9)
                    || ((floor (absoluteValue '.' count) % 10 == 0)
                            && (absoluteValue '.' count /= 0)
                       )
               )
    then
        Many
    else
        Other
