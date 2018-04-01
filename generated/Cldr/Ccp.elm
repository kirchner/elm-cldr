module Cldr.Ccp
    exposing
        ( currencyCakmAccounting
        , currencyCakmStandard
        , currencyLatnAccounting
        , currencyLatnStandard
        , decimalCakmStandard
        , decimalLatnStandard
        , percentCakmStandard
        , percentLatnStandard
        , quote
        , quoteAlternate
        , scientificCakmStandard
        , scientificLatnStandard
        )

{-|

@docs quote, quoteAlternate, decimalCakmStandard, decimalLatnStandard, scientificCakmStandard, scientificLatnStandard, percentCakmStandard, percentLatnStandard, currencyCakmStandard, currencyCakmAccounting, currencyLatnStandard, currencyLatnAccounting

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


cakmNumberSymbols : Symbols
cakmNumberSymbols =
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
decimalCakmStandard : FloatPrinter args msg
decimalCakmStandard =
    floatPrinter [ "decimal", "cakm", "standard" ]
        (\float -> s (Number.print cakmNumberSymbols decimalCakmStandardNumberFormat float))
        (Number.floatInfo decimalCakmStandardNumberFormat)


decimalCakmStandardNumberFormat : NumberFormat
decimalCakmStandardNumberFormat =
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
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 0
        , maximalFractionCount = 3
        }
    , negativePattern = Nothing
    }


{-| -}
scientificCakmStandard : FloatPrinter args msg
scientificCakmStandard =
    floatPrinter [ "scientific", "cakm", "standard" ]
        (\float -> s (Number.print cakmNumberSymbols scientificCakmStandardNumberFormat float))
        (Number.floatInfo scientificCakmStandardNumberFormat)


scientificCakmStandardNumberFormat : NumberFormat
scientificCakmStandardNumberFormat =
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
percentCakmStandard : FloatPrinter args msg
percentCakmStandard =
    floatPrinter [ "percent", "cakm", "standard" ]
        (\float -> s (Number.print cakmNumberSymbols percentCakmStandardNumberFormat float))
        (Number.floatInfo percentCakmStandardNumberFormat)


percentCakmStandardNumberFormat : NumberFormat
percentCakmStandardNumberFormat =
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
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 0
        , maximalFractionCount = 0
        }
    , negativePattern = Nothing
    }


{-| -}
currencyCakmStandard : FloatPrinter args msg
currencyCakmStandard =
    floatPrinter [ "currency", "cakm", "standard" ]
        (\float -> s (Number.print cakmNumberSymbols currencyCakmStandardNumberFormat float))
        (Number.floatInfo currencyCakmStandardNumberFormat)


currencyCakmStandardNumberFormat : NumberFormat
currencyCakmStandardNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 3
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 2
        , maximalFractionCount = 3
        }
    , negativePattern = Nothing
    }


{-| -}
currencyCakmAccounting : FloatPrinter args msg
currencyCakmAccounting =
    floatPrinter [ "currency", "cakm", "accounting" ]
        (\float -> s (Number.print cakmNumberSymbols currencyCakmAccountingNumberFormat float))
        (Number.floatInfo currencyCakmAccountingNumberFormat)


currencyCakmAccountingNumberFormat : NumberFormat
currencyCakmAccountingNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 3
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 2
        , maximalFractionCount = 3
        }
    , negativePattern =
        Just
            { prefix = ""
            , suffix = ""
            , primaryGroupingSize = Just 3
            , secondaryGroupingSize = Just 2
            , minimalIntegerCount = 1
            , minimalFractionCount = 2
            , maximalFractionCount = 4
            }
    }


{-| -}
currencyLatnStandard : FloatPrinter args msg
currencyLatnStandard =
    floatPrinter [ "currency", "latn", "standard" ]
        (\float -> s (Number.print latnNumberSymbols currencyLatnStandardNumberFormat float))
        (Number.floatInfo currencyLatnStandardNumberFormat)


currencyLatnStandardNumberFormat : NumberFormat
currencyLatnStandardNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 3
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 2
        , maximalFractionCount = 3
        }
    , negativePattern = Nothing
    }


{-| -}
currencyLatnAccounting : FloatPrinter args msg
currencyLatnAccounting =
    floatPrinter [ "currency", "latn", "accounting" ]
        (\float -> s (Number.print latnNumberSymbols currencyLatnAccountingNumberFormat float))
        (Number.floatInfo currencyLatnAccountingNumberFormat)


currencyLatnAccountingNumberFormat : NumberFormat
currencyLatnAccountingNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 3
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 2
        , maximalFractionCount = 3
        }
    , negativePattern =
        Just
            { prefix = ""
            , suffix = ""
            , primaryGroupingSize = Just 3
            , secondaryGroupingSize = Just 2
            , minimalIntegerCount = 1
            , minimalFractionCount = 2
            , maximalFractionCount = 4
            }
    }
