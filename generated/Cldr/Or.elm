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
decimalOryaStandard : FloatPrinter args msg
decimalOryaStandard =
    floatPrinter
        (\float -> s (Number.print oryaNumberSymbols [ '୦', '୧', '୨', '୩', '୪', '୫', '୬', '୭', '୮', '୯' ] decimalOryaStandardNumberFormat float))
        (Number.floatInfo decimalOryaStandardNumberFormat)


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
scientificOryaStandard : FloatPrinter args msg
scientificOryaStandard =
    floatPrinter
        (\float -> s (Number.print oryaNumberSymbols [ '୦', '୧', '୨', '୩', '୪', '୫', '୬', '୭', '୮', '୯' ] scientificOryaStandardNumberFormat float))
        (Number.floatInfo scientificOryaStandardNumberFormat)


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
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 0
        , maximalFractionCount = 0
        }
    , negativePattern = Nothing
    }


{-| -}
percentOryaStandard : FloatPrinter args msg
percentOryaStandard =
    floatPrinter
        (\float -> s (Number.print oryaNumberSymbols [ '୦', '୧', '୨', '୩', '୪', '୫', '୬', '୭', '୮', '୯' ] percentOryaStandardNumberFormat float))
        (Number.floatInfo percentOryaStandardNumberFormat)


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
currencyOryaStandard : FloatPrinter args msg
currencyOryaStandard =
    floatPrinter
        (\float -> s (Number.print oryaNumberSymbols [ '୦', '୧', '୨', '୩', '୪', '୫', '୬', '୭', '୮', '୯' ] currencyOryaStandardNumberFormat float))
        (Number.floatInfo currencyOryaStandardNumberFormat)


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
currencyOryaAccounting : FloatPrinter args msg
currencyOryaAccounting =
    floatPrinter
        (\float -> s (Number.print oryaNumberSymbols [ '୦', '୧', '୨', '୩', '୪', '୫', '୬', '୭', '୮', '୯' ] currencyOryaAccountingNumberFormat float))
        (Number.floatInfo currencyOryaAccountingNumberFormat)


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
    if
        (floatInfo.absoluteValue == 1)
            || (floatInfo.absoluteValue == 5)
            || ((floatInfo.absoluteValue < 7) && (floatInfo.absoluteValue > 9))
    then
        One
    else if
        (floatInfo.absoluteValue == 2)
            || (floatInfo.absoluteValue == 3)
    then
        Two
    else if floatInfo.absoluteValue == 4 then
        Few
    else if floatInfo.absoluteValue == 6 then
        Many
    else
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
        { one : Text Static args msg
        , two : Text Static args msg
        , few : Text Static args msg
        , many : Text Static args msg
        , other : Text Static args msg
        }
    -> Text Static args msg
ordinal accessor printer otherTexts { one, two, few, many, other } =
    plural accessor
        printer
        toOrdinalForm
        otherTexts
        { zero = Nothing
        , one = Just one
        , two = Just two
        , few = Just few
        , many = Just many
        , other = other
        }
