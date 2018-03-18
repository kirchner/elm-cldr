module Cldr.Mk
    exposing
        ( cardinal
        , currencyLatnAccounting
        , currencyLatnStandard
        , decimalLatnStandard
        , ordinal
        , percentLatnStandard
        , quote
        , quoteAlternate
        , scientificLatnStandard
        , toCardinalForm
        , toOrdinalForm
        )

{-|

@docs quote, quoteAlternate, decimalLatnStandard, scientificLatnStandard, percentLatnStandard, currencyLatnStandard, currencyLatnAccounting, toCardinalForm, toOrdinalForm, cardinal, ordinal

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
                [ s "„"
                , text
                , s "“"
                ]


{-| -}
quoteAlternate : Printer (Text args node) args node
quoteAlternate =
    printer [ "quote", "alternate" ] <|
        \text ->
            concat
                [ s "‚"
                , text
                , s "‘"
                ]


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
    , nan = "NaN"
    , timeSeparator = ":"
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
currencyLatnStandard : Printer Float args msg
currencyLatnStandard =
    printer [ "currency", "latn", "standard" ] <|
        \float ->
            s (Number.print latnNumberSymbols currencyLatnStandardNumberFormat float)


currencyLatnStandardNumberFormat : NumberFormat
currencyLatnStandardNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 3
        , secondaryGroupingSize = Nothing
        , minimalIntegerCount = 1
        , minimalFractionCount = 2
        , maximalFractionCount = 4
        }
    , negativePattern = Nothing
    }


{-| -}
currencyLatnAccounting : Printer Float args msg
currencyLatnAccounting =
    printer [ "currency", "latn", "accounting" ] <|
        \float ->
            s (Number.print latnNumberSymbols currencyLatnAccountingNumberFormat float)


currencyLatnAccountingNumberFormat : NumberFormat
currencyLatnAccountingNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 3
        , secondaryGroupingSize = Nothing
        , minimalIntegerCount = 1
        , minimalFractionCount = 2
        , maximalFractionCount = 4
        }
    , negativePattern = Nothing
    }


{-| -}
toCardinalForm :
    Float
    -> String
    -> PluralForm
toCardinalForm _ count =
    if
        ((Plural.fractionDigitCount '.' WithTrailingZeros count == 0)
            && (Plural.integerDigits '.' count % 10 == 1)
        )
            || (Plural.fractionDigits '.' WithTrailingZeros count % 10 == 1)
    then
        One
    else
        Other


{-| -}
toOrdinalForm :
    Float
    -> String
    -> PluralForm
toOrdinalForm _ count =
    if
        (Plural.integerDigits '.' count % 10 == 1)
            && (Plural.integerDigits '.' count % 100 /= 11)
    then
        One
    else if
        (Plural.integerDigits '.' count % 10 == 2)
            && (Plural.integerDigits '.' count % 100 /= 12)
    then
        Two
    else if
        ((Plural.integerDigits '.' count % 10 == 7)
            || (Plural.integerDigits '.' count % 10 == 8)
        )
            && ((Plural.integerDigits '.' count % 100 /= 17)
                    && (Plural.integerDigits '.' count % 100 /= 18)
               )
    then
        Many
    else
        Other


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
ordinal :
    Printer Float args msg
    -> (args -> Float)
    -> String
    ->
        { one : Text args msg
        , two : Text args msg
        , many : Text args msg
        , other : Text args msg
        }
    -> Text args msg
ordinal printer accessor name { one, two, many, other } =
    plural printer toOrdinalForm accessor name <|
        { zero = Nothing
        , one = Just one
        , two = Just two
        , few = Nothing
        , many = Just many
        , other = other
        }
