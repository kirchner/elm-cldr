module Cldr.Pa.Arab
    exposing
        ( cardinal
        , decimalArabextStandard
        , decimalLatnStandard
        , ordinal
        , percentArabextStandard
        , percentLatnStandard
        , quote
        , quoteAlternate
        , scientificArabextStandard
        , scientificLatnStandard
        , toCardinalForm
        , toOrdinalForm
        )

{-|

@docs quote, quoteAlternate, decimalArabextStandard, decimalLatnStandard, scientificArabextStandard, scientificLatnStandard, percentArabextStandard, percentLatnStandard, cardinal, toCardinalForm, ordinal, toOrdinalForm

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


arabextNumberSymbols : Symbols
arabextNumberSymbols =
    { decimal = "٫"
    , group = "٬"
    , list = "؛"
    , percentSign = "٪"
    , plusSign = "\x200E+\x200E"
    , minusSign = "\x200E-\x200E"
    , exponential = "×۱۰^"
    , superscriptingExponent = "×"
    , perMille = "؉"
    , infinity = "∞"
    , nan = "NaN"
    , timeSeparator = "٫"
    }


latnNumberSymbols : Symbols
latnNumberSymbols =
    { decimal = "."
    , group = ","
    , list = ";"
    , percentSign = "%"
    , plusSign = "\x200E+"
    , minusSign = "\x200E-"
    , exponential = "E"
    , superscriptingExponent = "×"
    , perMille = "‰"
    , infinity = "∞"
    , nan = "NaN"
    , timeSeparator = ":"
    }


{-| -}
decimalArabextStandard : Printer Float args msg
decimalArabextStandard =
    printer [ "decimal", "arabext", "standard" ] <|
        \float ->
            s (Number.print arabextNumberSymbols decimalArabextStandardNumberFormat float)


decimalArabextStandardNumberFormat : NumberFormat
decimalArabextStandardNumberFormat =
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
scientificArabextStandard : Printer Float args msg
scientificArabextStandard =
    printer [ "scientific", "arabext", "standard" ] <|
        \float ->
            s (Number.print arabextNumberSymbols scientificArabextStandardNumberFormat float)


scientificArabextStandardNumberFormat : NumberFormat
scientificArabextStandardNumberFormat =
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
percentArabextStandard : Printer Float args msg
percentArabextStandard =
    printer [ "percent", "arabext", "standard" ] <|
        \float ->
            s (Number.print arabextNumberSymbols percentArabextStandardNumberFormat float)


percentArabextStandardNumberFormat : NumberFormat
percentArabextStandardNumberFormat =
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
toCardinalForm :
    Float
    -> String
    -> PluralForm
toCardinalForm _ count =
    if (Plural.absoluteValue '.' count < 0) && (Plural.absoluteValue '.' count > 1) then
        One
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
