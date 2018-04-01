module Cldr.Brx
    exposing
        ( cardinal
        , decimalDevaStandard
        , decimalLatnStandard
        , ordinal
        , percentDevaStandard
        , percentLatnStandard
        , quote
        , quoteAlternate
        , scientificDevaStandard
        , scientificLatnStandard
        , toCardinalForm
        , toOrdinalForm
        )

{-|

@docs quote, quoteAlternate, decimalDevaStandard, decimalLatnStandard, scientificDevaStandard, scientificLatnStandard, percentDevaStandard, percentLatnStandard, toCardinalForm, toOrdinalForm, cardinal, ordinal

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


devaNumberSymbols : Symbols
devaNumberSymbols =
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
decimalDevaStandard : FloatPrinter args msg
decimalDevaStandard =
    floatPrinter [ "decimal", "deva", "standard" ]
        (\float -> s (Number.print devaNumberSymbols decimalDevaStandardNumberFormat float))
        (Number.floatInfo decimalDevaStandardNumberFormat)


decimalDevaStandardNumberFormat : NumberFormat
decimalDevaStandardNumberFormat =
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
scientificDevaStandard : FloatPrinter args msg
scientificDevaStandard =
    floatPrinter [ "scientific", "deva", "standard" ]
        (\float -> s (Number.print devaNumberSymbols scientificDevaStandardNumberFormat float))
        (Number.floatInfo scientificDevaStandardNumberFormat)


scientificDevaStandardNumberFormat : NumberFormat
scientificDevaStandardNumberFormat =
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
percentDevaStandard : FloatPrinter args msg
percentDevaStandard =
    floatPrinter [ "percent", "deva", "standard" ]
        (\float -> s (Number.print devaNumberSymbols percentDevaStandardNumberFormat float))
        (Number.floatInfo percentDevaStandardNumberFormat)


percentDevaStandardNumberFormat : NumberFormat
percentDevaStandardNumberFormat =
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
