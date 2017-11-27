module Localized.Lo
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
    , nan = "ບໍ່\x200Bແມ່ນ\x200Bໂຕ\x200Bເລກ"
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
        , minimalIntegerCount = 0
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
        { other : List (Part args)
        }
    -> Part args
cardinal accessor { other } =
    customPlural accessor
        (toString >> cardinalSelector)
        { zero = []
        , one = []
        , two = []
        , few = []
        , many = []
        , other = other
        }


cardinalDynamic :
    (args -> Float)
    ->
        { other : List (Part args)
        }
    -> Part args
cardinalDynamic accessor { other } =
    dynamicPlural accessor
        cardinalPluralRules
        { zero = []
        , one = []
        , two = []
        , few = []
        , many = []
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
    { zero = Nothing, one = Nothing, two = Nothing, few = Nothing, many = Nothing }


cardinalSelector :
    String
    -> PluralCase
cardinalSelector count =
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
