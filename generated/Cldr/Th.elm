module Cldr.Th
    exposing
        ( cardinal
        , decimalLatnStandard
        , decimalThaiStandard
        , ordinal
        , percentLatnStandard
        , percentThaiStandard
        , quote
        , quoteAlternate
        , scientificLatnStandard
        , scientificThaiStandard
        , toCardinalForm
        , toOrdinalForm
        )

{-|

@docs quote, quoteAlternate, decimalLatnStandard, decimalThaiStandard, scientificLatnStandard, scientificThaiStandard, percentLatnStandard, percentThaiStandard, toCardinalForm, toOrdinalForm, cardinal, ordinal

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


thaiNumberSymbols : Symbols
thaiNumberSymbols =
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
decimalThaiStandard : FloatPrinter args msg
decimalThaiStandard =
    floatPrinter [ "decimal", "thai", "standard" ]
        (\float -> s (Number.print thaiNumberSymbols decimalThaiStandardNumberFormat float))
        (Number.floatInfo decimalThaiStandardNumberFormat)


decimalThaiStandardNumberFormat : NumberFormat
decimalThaiStandardNumberFormat =
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
scientificThaiStandard : FloatPrinter args msg
scientificThaiStandard =
    floatPrinter [ "scientific", "thai", "standard" ]
        (\float -> s (Number.print thaiNumberSymbols scientificThaiStandardNumberFormat float))
        (Number.floatInfo scientificThaiStandardNumberFormat)


scientificThaiStandardNumberFormat : NumberFormat
scientificThaiStandardNumberFormat =
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
percentThaiStandard : FloatPrinter args msg
percentThaiStandard =
    floatPrinter [ "percent", "thai", "standard" ]
        (\float -> s (Number.print thaiNumberSymbols percentThaiStandardNumberFormat float))
        (Number.floatInfo percentThaiStandardNumberFormat)


percentThaiStandardNumberFormat : NumberFormat
percentThaiStandardNumberFormat =
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
