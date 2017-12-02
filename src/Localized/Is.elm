module Localized.Is
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
        { other : List (Text args msg)
        }
    -> Text args msg
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
            (Or
                (And
                    (Equal
                        (Simple
                            (FractionDigits WithoutTrailingZeros)
                        )
                        [ Single 0 ]
                    )
                    (And
                        (Equal
                            (Modulo IntegerDigits 10)
                            [ Single 1 ]
                        )
                        (NotEqual
                            (Modulo IntegerDigits 100)
                            [ Single 11 ]
                        )
                    )
                )
                (NotEqual
                    (Simple
                        (FractionDigits WithoutTrailingZeros)
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
        ((fractionDigits '.' WithoutTrailingZeros count == 0)
            && ((integerDigits '.' count % 10 == 1)
                    && (integerDigits '.' count % 100 /= 11)
               )
        )
            || (fractionDigits '.' WithoutTrailingZeros count /= 0)
    then
        One
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
