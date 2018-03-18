module Cldr.Or
    exposing
        ( cardinal
        , currencyLatnAccounting
        , currencyLatnStandard
        , currencyOryaAccounting
        , currencyOryaStandard
        , decimalLatnStandard
        , decimalOryaStandard
        , ordinal
        , percentLatnStandard
        , percentOryaStandard
        , quote
        , quoteAlternate
        , scientificLatnStandard
        , scientificOryaStandard
        , toCardinalForm
        , toOrdinalForm
        )

{-|

@docs quote, quoteAlternate, decimalLatnStandard, decimalOryaStandard, scientificLatnStandard, scientificOryaStandard, percentLatnStandard, percentOryaStandard, currencyLatnStandard, currencyLatnAccounting, currencyOryaStandard, currencyOryaAccounting, toCardinalForm, toOrdinalForm, cardinal, ordinal

-}

import Data.Numbers exposing (NumberFormat, Symbols)
import Data.PluralRules exposing (WithTrailingZeros(WithTrailingZeros, WithoutTrailingZeros))
import Printer.Number as Number
import Printer.Plural as Plural
import Translation exposing (PluralForm(Few, Many, One, Other, Two, Zero), Printer, Text, concat, plural, printer, s)


{-| -}
quote : Printer (Text args node) args node
quote =
    printer [ "quote" ] <|
        \text ->
            concat
                [ s "“"
                , text
                , s "”"
                ]


{-| -}
quoteAlternate : Printer (Text args node) args node
quoteAlternate =
    printer [ "quote", "alternate" ] <|
        \text ->
            concat
                [ s "‘"
                , text
                , s "’"
                ]


latnNumberSymbols : Symbols
latnNumberSymbols =
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


oryaNumberSymbols : Symbols
oryaNumberSymbols =
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


{-| -}
decimalLatnStandard : Printer Float args msg
decimalLatnStandard =
    printer [ "decimal", "latn", "standard" ] <|
        \float ->
            s (Number.print latnNumberSymbols decimalLatnStandardNumberFormat float)


decimalLatnStandardNumberFormat : NumberFormat
decimalLatnStandardNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 3
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 0
        , maximalFractionCount = 3
        }
    , negativePattern = Nothing
    }


{-| -}
decimalOryaStandard : Printer Float args msg
decimalOryaStandard =
    printer [ "decimal", "orya", "standard" ] <|
        \float ->
            s (Number.print oryaNumberSymbols decimalOryaStandardNumberFormat float)


decimalOryaStandardNumberFormat : NumberFormat
decimalOryaStandardNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 3
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 0
        , maximalFractionCount = 3
        }
    , negativePattern = Nothing
    }


{-| -}
scientificLatnStandard : Printer Float args msg
scientificLatnStandard =
    printer [ "scientific", "latn", "standard" ] <|
        \float ->
            s (Number.print latnNumberSymbols scientificLatnStandardNumberFormat float)


scientificLatnStandardNumberFormat : NumberFormat
scientificLatnStandardNumberFormat =
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
scientificOryaStandard : Printer Float args msg
scientificOryaStandard =
    printer [ "scientific", "orya", "standard" ] <|
        \float ->
            s (Number.print oryaNumberSymbols scientificOryaStandardNumberFormat float)


scientificOryaStandardNumberFormat : NumberFormat
scientificOryaStandardNumberFormat =
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
percentLatnStandard : Printer Float args msg
percentLatnStandard =
    printer [ "percent", "latn", "standard" ] <|
        \float ->
            s (Number.print latnNumberSymbols percentLatnStandardNumberFormat float)


percentLatnStandardNumberFormat : NumberFormat
percentLatnStandardNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 4
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 0
        , maximalFractionCount = 0
        }
    , negativePattern = Nothing
    }


{-| -}
percentOryaStandard : Printer Float args msg
percentOryaStandard =
    printer [ "percent", "orya", "standard" ] <|
        \float ->
            s (Number.print oryaNumberSymbols percentOryaStandardNumberFormat float)


percentOryaStandardNumberFormat : NumberFormat
percentOryaStandardNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 4
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 0
        , maximalFractionCount = 0
        }
    , negativePattern = Nothing
    }


{-| -}
currencyLatnStandard : Printer Float args msg
currencyLatnStandard =
    printer [ "currency", "latn", "standard" ] <|
        \float ->
            s (Number.print latnNumberSymbols currencyLatnStandardNumberFormat float)


currencyLatnStandardNumberFormat : NumberFormat
currencyLatnStandardNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 3
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 2
        , maximalFractionCount = 2
        }
    , negativePattern = Nothing
    }


{-| -}
currencyLatnAccounting : Printer Float args msg
currencyLatnAccounting =
    printer [ "currency", "latn", "accounting" ] <|
        \float ->
            s (Number.print latnNumberSymbols currencyLatnAccountingNumberFormat float)


currencyLatnAccountingNumberFormat : NumberFormat
currencyLatnAccountingNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 3
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 2
        , maximalFractionCount = 2
        }
    , negativePattern =
        Just
            { prefix = ""
            , suffix = ""
            , primaryGroupingSize = Just 3
            , secondaryGroupingSize = Just 2
            , minimalIntegerCount = 1
            , minimalFractionCount = 2
            , maximalFractionCount = 3
            }
    }


{-| -}
currencyOryaStandard : Printer Float args msg
currencyOryaStandard =
    printer [ "currency", "orya", "standard" ] <|
        \float ->
            s (Number.print oryaNumberSymbols currencyOryaStandardNumberFormat float)


currencyOryaStandardNumberFormat : NumberFormat
currencyOryaStandardNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 3
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 2
        , maximalFractionCount = 2
        }
    , negativePattern = Nothing
    }


{-| -}
currencyOryaAccounting : Printer Float args msg
currencyOryaAccounting =
    printer [ "currency", "orya", "accounting" ] <|
        \float ->
            s (Number.print oryaNumberSymbols currencyOryaAccountingNumberFormat float)


currencyOryaAccountingNumberFormat : NumberFormat
currencyOryaAccountingNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 3
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 2
        , maximalFractionCount = 2
        }
    , negativePattern =
        Just
            { prefix = ""
            , suffix = ""
            , primaryGroupingSize = Just 3
            , secondaryGroupingSize = Just 2
            , minimalIntegerCount = 1
            , minimalFractionCount = 2
            , maximalFractionCount = 3
            }
    }


{-| -}
toCardinalForm :
    Float
    -> String
    -> PluralForm
toCardinalForm _ count =
    if Plural.absoluteValue '.' count == 1 then
        One
    else
        Other


{-| -}
toOrdinalForm :
    Float
    -> String
    -> PluralForm
toOrdinalForm _ count =
    if
        (Plural.absoluteValue '.' count == 1)
            || (Plural.absoluteValue '.' count == 5)
            || ((Plural.absoluteValue '.' count < 7) && (Plural.absoluteValue '.' count > 9))
    then
        One
    else if
        (Plural.absoluteValue '.' count == 2)
            || (Plural.absoluteValue '.' count == 3)
    then
        Two
    else if Plural.absoluteValue '.' count == 4 then
        Few
    else if Plural.absoluteValue '.' count == 6 then
        Many
    else
        Other


{-| -}
cardinal :
    Printer Float args msg
    -> (args -> Float)
    -> String
    ->
        { one : Text args msg
        , other : Text args msg
        }
    -> Text args msg
cardinal printer accessor name { one, other } =
    plural printer toCardinalForm accessor name <|
        { zero = Nothing
        , one = Just one
        , two = Nothing
        , few = Nothing
        , many = Nothing
        , other = other
        }


{-| -}
ordinal :
    Printer Float args msg
    -> (args -> Float)
    -> String
    ->
        { one : Text args msg
        , two : Text args msg
        , few : Text args msg
        , many : Text args msg
        , other : Text args msg
        }
    -> Text args msg
ordinal printer accessor name { one, two, few, many, other } =
    plural printer toOrdinalForm accessor name <|
        { zero = Nothing
        , one = Just one
        , two = Just two
        , few = Just few
        , many = Just many
        , other = other
        }
