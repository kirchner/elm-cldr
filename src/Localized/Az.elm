module Localized.Az
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
        { one : List (Text args msg)
        , few : List (Text args msg)
        , many : List (Text args msg)
        , other : List (Text args msg)
        }
    -> Text args msg
ordinal accessor { one, few, many, other } =
    Localized.customPlural accessor
        (Localized.customNumberFormat toString)
        ordinalSelector
        { zero = []
        , one = one
        , two = []
        , few = few
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
    , one =
        Just
            (Or
                (Equal
                    (Modulo IntegerDigits 10)
                    [ Single 1, Single 2, Single 5, Single 7, Single 8 ]
                )
                (Equal
                    (Modulo IntegerDigits 100)
                    [ Single 20, Single 50, Single 70, Single 80 ]
                )
            )
    , two = Nothing
    , few =
        Just
            (Or
                (Equal
                    (Modulo IntegerDigits 10)
                    [ Single 3, Single 4 ]
                )
                (Equal
                    (Modulo IntegerDigits 1000)
                    [ Single 100, Single 200, Single 300, Single 400, Single 500, Single 600, Single 700, Single 800, Single 900 ]
                )
            )
    , many =
        Just
            (Or
                (Equal
                    (Simple IntegerDigits)
                    [ Single 0 ]
                )
                (Or
                    (Equal
                        (Modulo IntegerDigits 10)
                        [ Single 6 ]
                    )
                    (Equal
                        (Modulo IntegerDigits 100)
                        [ Single 40, Single 60, Single 90 ]
                    )
                )
            )
    }


ordinalSelector :
    String
    -> PluralCase
ordinalSelector count =
    if
        ((integerDigits '.' count % 10 == 1)
            || (integerDigits '.' count % 10 == 2)
            || (integerDigits '.' count % 10 == 5)
            || (integerDigits '.' count % 10 == 7)
            || (integerDigits '.' count % 10 == 8)
        )
            || ((integerDigits '.' count % 100 == 20)
                    || (integerDigits '.' count % 100 == 50)
                    || (integerDigits '.' count % 100 == 70)
                    || (integerDigits '.' count % 100 == 80)
               )
    then
        One
    else if
        ((integerDigits '.' count % 10 == 3)
            || (integerDigits '.' count % 10 == 4)
        )
            || ((integerDigits '.' count % 1000 == 100)
                    || (integerDigits '.' count % 1000 == 200)
                    || (integerDigits '.' count % 1000 == 300)
                    || (integerDigits '.' count % 1000 == 400)
                    || (integerDigits '.' count % 1000 == 500)
                    || (integerDigits '.' count % 1000 == 600)
                    || (integerDigits '.' count % 1000 == 700)
                    || (integerDigits '.' count % 1000 == 800)
                    || (integerDigits '.' count % 1000 == 900)
               )
    then
        Few
    else if
        (integerDigits '.' count == 0)
            || ((integerDigits '.' count % 10 == 6)
                    || ((integerDigits '.' count % 100 == 40)
                            || (integerDigits '.' count % 100 == 60)
                            || (integerDigits '.' count % 100 == 90)
                       )
               )
    then
        Many
    else
        Other
