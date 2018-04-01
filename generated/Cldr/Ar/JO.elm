module Cldr.Ar.JO
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

@docs quote, quoteAlternate, decimalArabStandard, decimalLatnStandard, scientificArabStandard, scientificLatnStandard, percentArabStandard, percentLatnStandard, currencyLatnStandard, currencyLatnAccounting, toCardinalForm, toOrdinalForm, cardinal, ordinal

-}

import Data.Numbers exposing (NumberFormat, Symbols)
import Data.PluralRules exposing (WithTrailingZeros(WithTrailingZeros, WithoutTrailingZeros))
import Printer.Number as Number
import Printer.Plural as Plural
import Text exposing (FloatInfo, FloatPrinter, PluralForm(Few, Many, One, Other, Two, Zero), Printer, Static, Text, concat, floatPrinter, plural, printer, s)


{-| -}
quote : Printer (Text Static args node) args node
quote =
    printer [ "quote" ] <|
        \text ->
            concat
                [ s "”"
                , text
                , s "“"
                ]


{-| -}
quoteAlternate : Printer (Text Static args node) args node
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
decimalArabStandard : FloatPrinter args msg
decimalArabStandard =
    floatPrinter [ "decimal", "arab", "standard" ]
        (\float -> s (Number.print arabNumberSymbols decimalArabStandardNumberFormat float))
        (Number.floatInfo decimalArabStandardNumberFormat)


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
scientificArabStandard : FloatPrinter args msg
scientificArabStandard =
    floatPrinter [ "scientific", "arab", "standard" ]
        (\float -> s (Number.print arabNumberSymbols scientificArabStandardNumberFormat float))
        (Number.floatInfo scientificArabStandardNumberFormat)


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
percentArabStandard : FloatPrinter args msg
percentArabStandard =
    floatPrinter [ "percent", "arab", "standard" ]
        (\float -> s (Number.print arabNumberSymbols percentArabStandardNumberFormat float))
        (Number.floatInfo percentArabStandardNumberFormat)


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
        , secondaryGroupingSize = Nothing
        , minimalIntegerCount = 1
        , minimalFractionCount = 2
        , maximalFractionCount = 2
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
toCardinalForm :
    Float
    -> FloatInfo
    -> PluralForm
toCardinalForm _ floatInfo =
    if floatInfo.absoluteValue == 0 then
        Zero
    else if floatInfo.absoluteValue == 1 then
        One
    else if floatInfo.absoluteValue == 2 then
        Two
    else if (floor floatInfo.absoluteValue % 100 < 3) && (floor floatInfo.absoluteValue % 100 > 10) then
        Few
    else if (floor floatInfo.absoluteValue % 100 < 11) && (floor floatInfo.absoluteValue % 100 > 99) then
        Many
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
        { zero : Text Static args msg
        , one : Text Static args msg
        , two : Text Static args msg
        , few : Text Static args msg
        , many : Text Static args msg
        , other : Text Static args msg
        }
    -> Text Static args msg
cardinal accessor printer otherTexts { zero, one, two, few, many, other } =
    plural accessor
        printer
        toCardinalForm
        otherTexts
        { zero = Just zero
        , one = Just one
        , two = Just two
        , few = Just few
        , many = Just many
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
