module Cldr.Lo
    exposing
        ( cardinal
        , decimalLaooStandard
        , decimalLatnStandard
        , ordinal
        , percentLaooStandard
        , percentLatnStandard
        , quote
        , quoteAlternate
        , scientificLaooStandard
        , scientificLatnStandard
        , toCardinalForm
        , toOrdinalForm
        )

{-|

@docs quote, quoteAlternate, decimalLaooStandard, decimalLatnStandard, scientificLaooStandard, scientificLatnStandard, percentLaooStandard, percentLatnStandard, toCardinalForm, toOrdinalForm, cardinal, ordinal

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


laooNumberSymbols : Symbols
laooNumberSymbols =
    { decimal = ","
    , group = "."
    , list = ";"
    , percentSign = "%"
    , plusSign = "+"
    , minusSign = "-"
    , exponential = "E"
    , superscriptingExponent = "×"
    , perMille = "‰"
    , infinity = "∞"
    , nan = "ບໍ່\x200Bແມ່ນ\x200Bໂຕ\x200Bເລກ"
    , timeSeparator = ":"
    }


latnNumberSymbols : Symbols
latnNumberSymbols =
    { decimal = ","
    , group = "."
    , list = ";"
    , percentSign = "%"
    , plusSign = "+"
    , minusSign = "-"
    , exponential = "E"
    , superscriptingExponent = "×"
    , perMille = "‰"
    , infinity = "∞"
    , nan = "ບໍ່\x200Bແມ່ນ\x200Bໂຕ\x200Bເລກ"
    , timeSeparator = ":"
    }


{-| -}
decimalLaooStandard : FloatPrinter args msg
decimalLaooStandard =
    floatPrinter
        (\float -> s (Number.print laooNumberSymbols [ '໐', '໑', '໒', '໓', '໔', '໕', '໖', '໗', '໘', '໙' ] decimalLaooStandardNumberFormat float))
        (Number.floatInfo decimalLaooStandardNumberFormat)


decimalLaooStandardNumberFormat : NumberFormat
decimalLaooStandardNumberFormat =
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
    floatPrinter
        (\float -> s (Number.print latnNumberSymbols [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ] decimalLatnStandardNumberFormat float))
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
scientificLaooStandard : FloatPrinter args msg
scientificLaooStandard =
    floatPrinter
        (\float -> s (Number.print laooNumberSymbols [ '໐', '໑', '໒', '໓', '໔', '໕', '໖', '໗', '໘', '໙' ] scientificLaooStandardNumberFormat float))
        (Number.floatInfo scientificLaooStandardNumberFormat)


scientificLaooStandardNumberFormat : NumberFormat
scientificLaooStandardNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Nothing
        , secondaryGroupingSize = Nothing
        , minimalIntegerCount = 0
        , minimalFractionCount = 0
        , maximalFractionCount = 0
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
        , minimalIntegerCount = 0
        , minimalFractionCount = 0
        , maximalFractionCount = 0
        }
    , negativePattern = Nothing
    }


{-| -}
percentLaooStandard : FloatPrinter args msg
percentLaooStandard =
    floatPrinter
        (\float -> s (Number.print laooNumberSymbols [ '໐', '໑', '໒', '໓', '໔', '໕', '໖', '໗', '໘', '໙' ] percentLaooStandardNumberFormat float))
        (Number.floatInfo percentLaooStandardNumberFormat)


percentLaooStandardNumberFormat : NumberFormat
percentLaooStandardNumberFormat =
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
toCardinalForm :
    Float
    -> FloatInfo
    -> PluralForm
toCardinalForm _ floatInfo =
    Other


{-| -}
toOrdinalForm :
    Float
    -> FloatInfo
    -> PluralForm
toOrdinalForm _ floatInfo =
    if floatInfo.absoluteValue == 1 then
        One
    else
        Other


{-| -}
cardinal :
    (args -> Float)
    -> FloatPrinter args msg
    -> List ( Float, Text Static args msg )
    ->
        { other : Text Static args msg
        }
    -> Text Static args msg
cardinal accessor printer otherTexts { other } =
    plural accessor
        printer
        toCardinalForm
        otherTexts
        { zero = Nothing
        , one = Nothing
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
        , other : Text Static args msg
        }
    -> Text Static args msg
ordinal accessor printer otherTexts { one, other } =
    plural accessor
        printer
        toOrdinalForm
        otherTexts
        { zero = Nothing
        , one = Just one
        , two = Nothing
        , few = Nothing
        , many = Nothing
        , other = other
        }
