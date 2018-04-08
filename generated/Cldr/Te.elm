module Cldr.Te
    exposing
        ( cardinal
        , currencyLatnAccounting
        , currencyLatnStandard
        , currencyTeluAccounting
        , currencyTeluStandard
        , decimalLatnStandard
        , decimalTeluStandard
        , ordinal
        , percentLatnStandard
        , percentTeluStandard
        , quote
        , quoteAlternate
        , scientificLatnStandard
        , scientificTeluStandard
        , toCardinalForm
        , toOrdinalForm
        )

{-|

@docs quote, quoteAlternate, decimalLatnStandard, decimalTeluStandard, scientificLatnStandard, scientificTeluStandard, percentLatnStandard, percentTeluStandard, currencyLatnStandard, currencyLatnAccounting, currencyTeluStandard, currencyTeluAccounting, toCardinalForm, toOrdinalForm, cardinal, ordinal

-}

import Data.Numbers exposing (NumberFormat, Symbols)
import Data.PluralRules exposing (WithTrailingZeros(WithTrailingZeros, WithoutTrailingZeros))
import Printer.Number as Number
import Printer.Plural as Plural
import Text exposing (FloatInfo, FloatPrinter, PluralForm(Few, Many, One, Other, Two, Zero), Printer, Static, Text, concat, floatPrinter, plural, printer, s)


{-| -}
quote : Printer (Text Static args node) args node
quote =
    printer <|
        \text ->
            concat
                [ s "“"
                , text
                , s "”"
                ]


{-| -}
quoteAlternate : Printer (Text Static args node) args node
quoteAlternate =
    printer <|
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


teluNumberSymbols : Symbols
teluNumberSymbols =
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
decimalLatnStandard : FloatPrinter args msg
decimalLatnStandard =
    floatPrinter
        (\float -> s (Number.print latnNumberSymbols [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ] decimalLatnStandardNumberFormat float))
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
decimalTeluStandard : FloatPrinter args msg
decimalTeluStandard =
    floatPrinter
        (\float -> s (Number.print teluNumberSymbols [ '౦', '౧', '౨', '౩', '౪', '౫', '౬', '౭', '౮', '౯' ] decimalTeluStandardNumberFormat float))
        (Number.floatInfo decimalTeluStandardNumberFormat)


decimalTeluStandardNumberFormat : NumberFormat
decimalTeluStandardNumberFormat =
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
scientificLatnStandard : FloatPrinter args msg
scientificLatnStandard =
    floatPrinter
        (\float -> s (Number.print latnNumberSymbols [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ] scientificLatnStandardNumberFormat float))
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
scientificTeluStandard : FloatPrinter args msg
scientificTeluStandard =
    floatPrinter
        (\float -> s (Number.print teluNumberSymbols [ '౦', '౧', '౨', '౩', '౪', '౫', '౬', '౭', '౮', '౯' ] scientificTeluStandardNumberFormat float))
        (Number.floatInfo scientificTeluStandardNumberFormat)


scientificTeluStandardNumberFormat : NumberFormat
scientificTeluStandardNumberFormat =
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
percentLatnStandard : FloatPrinter args msg
percentLatnStandard =
    floatPrinter
        (\float -> s (Number.print latnNumberSymbols [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ] percentLatnStandardNumberFormat float))
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


{-| -}
percentTeluStandard : FloatPrinter args msg
percentTeluStandard =
    floatPrinter
        (\float -> s (Number.print teluNumberSymbols [ '౦', '౧', '౨', '౩', '౪', '౫', '౬', '౭', '౮', '౯' ] percentTeluStandardNumberFormat float))
        (Number.floatInfo percentTeluStandardNumberFormat)


percentTeluStandardNumberFormat : NumberFormat
percentTeluStandardNumberFormat =
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
currencyLatnStandard : FloatPrinter args msg
currencyLatnStandard =
    floatPrinter
        (\float -> s (Number.print latnNumberSymbols [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ] currencyLatnStandardNumberFormat float))
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
        , maximalFractionCount = 2
        }
    , negativePattern = Nothing
    }


{-| -}
currencyLatnAccounting : FloatPrinter args msg
currencyLatnAccounting =
    floatPrinter
        (\float -> s (Number.print latnNumberSymbols [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ] currencyLatnAccountingNumberFormat float))
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
currencyTeluStandard : FloatPrinter args msg
currencyTeluStandard =
    floatPrinter
        (\float -> s (Number.print teluNumberSymbols [ '౦', '౧', '౨', '౩', '౪', '౫', '౬', '౭', '౮', '౯' ] currencyTeluStandardNumberFormat float))
        (Number.floatInfo currencyTeluStandardNumberFormat)


currencyTeluStandardNumberFormat : NumberFormat
currencyTeluStandardNumberFormat =
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
currencyTeluAccounting : FloatPrinter args msg
currencyTeluAccounting =
    floatPrinter
        (\float -> s (Number.print teluNumberSymbols [ '౦', '౧', '౨', '౩', '౪', '౫', '౬', '౭', '౮', '౯' ] currencyTeluAccountingNumberFormat float))
        (Number.floatInfo currencyTeluAccountingNumberFormat)


currencyTeluAccountingNumberFormat : NumberFormat
currencyTeluAccountingNumberFormat =
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
    -> FloatInfo
    -> PluralForm
toCardinalForm _ floatInfo =
    if floatInfo.absoluteValue == 1 then
        One
    else
        Other


{-| -}
toOrdinalForm :
    Float
    -> FloatInfo
    -> PluralForm
toOrdinalForm _ floatInfo =
    Other


{-| -}
cardinal :
    (args -> Float)
    -> FloatPrinter args msg
    -> List ( Float, Text Static args msg )
    ->
        { one : Text Static args msg
        , other : Text Static args msg
        }
    -> Text Static args msg
cardinal accessor printer otherTexts { one, other } =
    plural accessor
        printer
        toCardinalForm
        otherTexts
        { zero = Nothing
        , one = Just one
        , two = Nothing
        , few = Nothing
        , many = Nothing
        , other = other
        }


{-| -}
ordinal :
    (args -> Float)
    -> FloatPrinter args msg
    -> List ( Float, Text Static args msg )
    ->
        { other : Text Static args msg
        }
    -> Text Static args msg
ordinal accessor printer otherTexts { other } =
    plural accessor
        printer
        toOrdinalForm
        otherTexts
        { zero = Nothing
        , one = Nothing
        , two = Nothing
        , few = Nothing
        , many = Nothing
        , other = other
        }
