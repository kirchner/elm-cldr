module Cldr.Ar.IL
    exposing
        ( cardinal
        , currencyLatnAccounting
        , currencyLatnStandard
        , decimalArabStandard
        , decimalLatnStandard
        , ordinal
        , percentArabStandard
        , percentLatnStandard
        , quote
        , quoteAlternate
        , scientificArabStandard
        , scientificLatnStandard
        , toCardinalForm
        , toOrdinalForm
        )

{-|

@docs quote, quoteAlternate, decimalArabStandard, decimalLatnStandard, scientificArabStandard, scientificLatnStandard, percentArabStandard, percentLatnStandard, currencyLatnStandard, currencyLatnAccounting, cardinal, toCardinalForm, ordinal, toOrdinalForm

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
                [ s "”"
                , text
                , s "“"
                ]


{-| -}
quoteAlternate : Printer (Text args node) args node
quoteAlternate =
    printer [ "quote", "alternate" ] <|
        \text ->
            concat
                [ s "’"
                , text
                , s "‘"
                ]


arabNumberSymbols : Symbols
arabNumberSymbols =
    { decimal = "٫"
    , group = "٬"
    , list = "؛"
    , percentSign = "٪\x061C"
    , plusSign = "\x061C+"
    , minusSign = "\x061C-"
    , exponential = "اس"
    , superscriptingExponent = "×"
    , perMille = "؉"
    , infinity = "∞"
    , nan = "ليس رقم"
    , timeSeparator = ":"
    }


latnNumberSymbols : Symbols
latnNumberSymbols =
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


{-| -}
decimalArabStandard : Printer Float args msg
decimalArabStandard =
    printer [ "decimal", "arab", "standard" ] <|
        \float ->
            s (Number.print arabNumberSymbols decimalArabStandardNumberFormat float)


decimalArabStandardNumberFormat : NumberFormat
decimalArabStandardNumberFormat =
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
        , secondaryGroupingSize = Nothing
        , minimalIntegerCount = 1
        , minimalFractionCount = 0
        , maximalFractionCount = 3
        }
    , negativePattern = Nothing
    }


{-| -}
scientificArabStandard : Printer Float args msg
scientificArabStandard =
    printer [ "scientific", "arab", "standard" ] <|
        \float ->
            s (Number.print arabNumberSymbols scientificArabStandardNumberFormat float)


scientificArabStandardNumberFormat : NumberFormat
scientificArabStandardNumberFormat =
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
percentArabStandard : Printer Float args msg
percentArabStandard =
    printer [ "percent", "arab", "standard" ] <|
        \float ->
            s (Number.print arabNumberSymbols percentArabStandardNumberFormat float)


percentArabStandardNumberFormat : NumberFormat
percentArabStandardNumberFormat =
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
        , secondaryGroupingSize = Nothing
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
        , secondaryGroupingSize = Nothing
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
        , secondaryGroupingSize = Nothing
        , minimalIntegerCount = 1
        , minimalFractionCount = 2
        , maximalFractionCount = 2
        }
    , negativePattern =
        Just
            { prefix = ""
            , suffix = ""
            , primaryGroupingSize = Just 3
            , secondaryGroupingSize = Nothing
            , minimalIntegerCount = 1
            , minimalFractionCount = 2
            , maximalFractionCount = 3
            }
    }


{-| -}
cardinal :
    Printer Float args msg
    -> (args -> Float)
    -> String
    ->
        { zero : Text args msg
        , one : Text args msg
        , two : Text args msg
        , few : Text args msg
        , many : Text args msg
        , other : Text args msg
        }
    -> Text args msg
cardinal printer accessor name { zero, one, two, few, many, other } =
    plural printer toCardinalForm accessor name <|
        { zero = Just zero
        , one = Just one
        , two = Just two
        , few = Just few
        , many = Just many
        , other = other
        }


{-| -}
toCardinalForm :
    Float
    -> String
    -> PluralForm
toCardinalForm _ count =
    if Plural.absoluteValue '.' count == 0 then
        Zero
    else if Plural.absoluteValue '.' count == 1 then
        One
    else if Plural.absoluteValue '.' count == 2 then
        Two
    else if (floor (Plural.absoluteValue '.' count) % 100 < 3) && (floor (Plural.absoluteValue '.' count) % 100 > 10) then
        Few
    else if (floor (Plural.absoluteValue '.' count) % 100 < 11) && (floor (Plural.absoluteValue '.' count) % 100 > 99) then
        Many
    else
        Other


{-| -}
ordinal :
    Printer Float args msg
    -> (args -> Float)
    -> String
    ->
        { other : Text args msg
        }
    -> Text args msg
ordinal printer accessor name { other } =
    plural printer toOrdinalForm accessor name <|
        { zero = Nothing
        , one = Nothing
        , two = Nothing
        , few = Nothing
        , many = Nothing
        , other = other
        }


{-| -}
toOrdinalForm :
    Float
    -> String
    -> PluralForm
toOrdinalForm _ count =
    Other
