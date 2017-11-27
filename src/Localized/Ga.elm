module Localized.Ga
    exposing
        ( cardinal
        , cardinalDynamic
        , decimal
        , ordinal
        , ordinalDynamic
        )

import Localized exposing (..)


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


decimal :
    (args -> Float)
    -> Part args
decimal accessor =
    customDecimal accessor numberSymbols standardNumberFormat


cardinal :
    (args -> Float)
    ->
        { one : List (Part args)
        , two : List (Part args)
        , few : List (Part args)
        , many : List (Part args)
        , other : List (Part args)
        }
    -> Part args
cardinal accessor { one, two, few, many, other } =
    customPlural accessor
        (toString >> cardinalSelector)
        { zero = []
        , one = one
        , two = two
        , few = few
        , many = many
        , other = other
        }


cardinalDynamic :
    (args -> Float)
    ->
        { one : List (Part args)
        , two : List (Part args)
        , few : List (Part args)
        , many : List (Part args)
        , other : List (Part args)
        }
    -> Part args
cardinalDynamic accessor { one, two, few, many, other } =
    dynamicPlural accessor
        cardinalPluralRules
        { zero = []
        , one = one
        , two = two
        , few = few
        , many = many
        , other = other
        }


ordinal :
    (args -> Float)
    ->
        { one : List (Part args)
        , other : List (Part args)
        }
    -> Part args
ordinal accessor { one, other } =
    customPlural accessor
        (toString >> ordinalSelector)
        { zero = []
        , one = one
        , two = []
        , few = []
        , many = []
        , other = other
        }


ordinalDynamic :
    (args -> Float)
    ->
        { one : List (Part args)
        , other : List (Part args)
        }
    -> Part args
ordinalDynamic accessor { one, other } =
    dynamicPlural accessor
        ordinalPluralRules
        { zero = []
        , one = one
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
                (Simple AbsoluteValue)
                [ Range 3 6 ]
            )
    , many =
        Just
            (Equal
                (Simple AbsoluteValue)
                [ Range 7 10 ]
            )
    }


cardinalSelector :
    String
    -> PluralCase
cardinalSelector count =
    if absoluteValue '.' count == 1 then
        One
    else if absoluteValue '.' count == 2 then
        Two
    else if (absoluteValue '.' count < 3) && (absoluteValue '.' count > 6) then
        Few
    else if (absoluteValue '.' count < 7) && (absoluteValue '.' count > 10) then
        Many
    else
        Other


ordinalPluralRules : PluralRules
ordinalPluralRules =
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


ordinalSelector :
    String
    -> PluralCase
ordinalSelector count =
    if absoluteValue '.' count == 1 then
        One
    else
        Other
