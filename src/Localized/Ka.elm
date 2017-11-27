module Localized.Ka
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
    , group = " "
    , list = ";"
    , percentSign = "%"
    , plusSign = "+"
    , minusSign = "-"
    , exponential = "E"
    , superscriptingExponent = "×"
    , perMille = "‰"
    , infinity = "∞"
    , nan = "არ არის რიცხვი"
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
        , other : List (Part args)
        }
    -> Part args
cardinal accessor { one, other } =
    customPlural accessor
        (toString >> cardinalSelector)
        { zero = []
        , one = one
        , two = []
        , few = []
        , many = []
        , other = other
        }


cardinalDynamic :
    (args -> Float)
    ->
        { one : List (Part args)
        , other : List (Part args)
        }
    -> Part args
cardinalDynamic accessor { one, other } =
    dynamicPlural accessor
        cardinalPluralRules
        { zero = []
        , one = one
        , two = []
        , few = []
        , many = []
        , other = other
        }


ordinal :
    (args -> Float)
    ->
        { one : List (Part args)
        , many : List (Part args)
        , other : List (Part args)
        }
    -> Part args
ordinal accessor { one, many, other } =
    customPlural accessor
        (toString >> ordinalSelector)
        { zero = []
        , one = one
        , two = []
        , few = []
        , many = many
        , other = other
        }


ordinalDynamic :
    (args -> Float)
    ->
        { one : List (Part args)
        , many : List (Part args)
        , other : List (Part args)
        }
    -> Part args
ordinalDynamic accessor { one, many, other } =
    dynamicPlural accessor
        ordinalPluralRules
        { zero = []
        , one = one
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
    , one =
        Just
            (Equal
                (Simple IntegerDigits)
                [ Single 1 ]
            )
    , two = Nothing
    , few = Nothing
    , many =
        Just
            (Or
                (Equal
                    (Simple IntegerDigits)
                    [ Single 0 ]
                )
                (Equal
                    (Modulo IntegerDigits 100)
                    [ Range 2 20, Single 40, Single 60, Single 80 ]
                )
            )
    }


ordinalSelector :
    String
    -> PluralCase
ordinalSelector count =
    if integerDigits '.' count == 1 then
        One
    else if
        (integerDigits '.' count == 0)
            || (((integerDigits '.' count % 100 < 2) && (integerDigits '.' count % 100 > 20))
                    || (integerDigits '.' count % 100 == 40)
                    || (integerDigits '.' count % 100 == 60)
                    || (integerDigits '.' count % 100 == 80)
               )
    then
        Many
    else
        Other
