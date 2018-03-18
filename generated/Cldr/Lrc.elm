module Cldr.Lrc
    exposing
        ( decimalArabextStandard
        , decimalLatnStandard
        , percentArabextStandard
        , percentLatnStandard
        , quote
        , quoteAlternate
        , scientificArabextStandard
        , scientificLatnStandard
        )

{-|

@docs quote, quoteAlternate, decimalArabextStandard, decimalLatnStandard, scientificArabextStandard, scientificLatnStandard, percentArabextStandard, percentLatnStandard

-}

import Data.Numbers exposing (NumberFormat, Symbols)
import Printer.Number as Number
import Translation exposing (Printer, Text, concat, printer, s)


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
