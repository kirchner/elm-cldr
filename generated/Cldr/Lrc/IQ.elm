module Cldr.Lrc.IQ
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
import Text exposing (FloatPrinter, Printer, Static, Text, concat, floatPrinter, printer, s)


{-| -}
quote : Printer (Text Static args node) args node
quote =
    printer [ "quote" ] <|
        \text ->
            concat
                [ s "“"
                , text
                , s "”"
                ]


{-| -}
quoteAlternate : Printer (Text Static args node) args node
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
decimalArabextStandard : FloatPrinter args msg
decimalArabextStandard =
    floatPrinter [ "decimal", "arabext", "standard" ]
        (\float -> s (Number.print arabextNumberSymbols decimalArabextStandardNumberFormat float))
        (Number.floatInfo decimalArabextStandardNumberFormat)


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
decimalLatnStandard : FloatPrinter args msg
decimalLatnStandard =
    floatPrinter [ "decimal", "latn", "standard" ]
        (\float -> s (Number.print latnNumberSymbols decimalLatnStandardNumberFormat float))
        (Number.floatInfo decimalLatnStandardNumberFormat)


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
scientificArabextStandard : FloatPrinter args msg
scientificArabextStandard =
    floatPrinter [ "scientific", "arabext", "standard" ]
        (\float -> s (Number.print arabextNumberSymbols scientificArabextStandardNumberFormat float))
        (Number.floatInfo scientificArabextStandardNumberFormat)


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
scientificLatnStandard : FloatPrinter args msg
scientificLatnStandard =
    floatPrinter [ "scientific", "latn", "standard" ]
        (\float -> s (Number.print latnNumberSymbols scientificLatnStandardNumberFormat float))
        (Number.floatInfo scientificLatnStandardNumberFormat)


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
percentArabextStandard : FloatPrinter args msg
percentArabextStandard =
    floatPrinter [ "percent", "arabext", "standard" ]
        (\float -> s (Number.print arabextNumberSymbols percentArabextStandardNumberFormat float))
        (Number.floatInfo percentArabextStandardNumberFormat)


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
percentLatnStandard : FloatPrinter args msg
percentLatnStandard =
    floatPrinter [ "percent", "latn", "standard" ]
        (\float -> s (Number.print latnNumberSymbols percentLatnStandardNumberFormat float))
        (Number.floatInfo percentLatnStandardNumberFormat)


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
