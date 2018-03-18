module Cldr.Az.Cyrl
    exposing
        ( cardinal
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

@docs quote, quoteAlternate, decimalLatnStandard, scientificLatnStandard, percentLatnStandard, cardinal, toCardinalForm, ordinal, toOrdinalForm

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
                [ s "«"
                , text
                , s "»"
                ]


{-| -}
quoteAlternate : Printer (Text args node) args node
quoteAlternate =
    printer [ "quote", "alternate" ] <|
        \text ->
            concat
                [ s "‹"
                , text
                , s "›"
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
toCardinalForm :
    Float
    -> String
    -> PluralForm
toCardinalForm _ count =
    if Plural.absoluteValue '.' count == 1 then
        One
    else
        Other


{-| -}
ordinal :
    Printer Float args msg
    -> (args -> Float)
    -> String
    ->
        { one : Text args msg
        , few : Text args msg
        , many : Text args msg
        , other : Text args msg
        }
    -> Text args msg
ordinal printer accessor name { one, few, many, other } =
    plural printer toOrdinalForm accessor name <|
        { zero = Nothing
        , one = Just one
        , two = Nothing
        , few = Just few
        , many = Just many
        , other = other
        }


{-| -}
toOrdinalForm :
    Float
    -> String
    -> PluralForm
toOrdinalForm _ count =
    if
        ((Plural.integerDigits '.' count % 10 == 1)
            || (Plural.integerDigits '.' count % 10 == 2)
            || (Plural.integerDigits '.' count % 10 == 5)
            || (Plural.integerDigits '.' count % 10 == 7)
            || (Plural.integerDigits '.' count % 10 == 8)
        )
            || ((Plural.integerDigits '.' count % 100 == 20)
                    || (Plural.integerDigits '.' count % 100 == 50)
                    || (Plural.integerDigits '.' count % 100 == 70)
                    || (Plural.integerDigits '.' count % 100 == 80)
               )
    then
        One
    else if
        ((Plural.integerDigits '.' count % 10 == 3)
            || (Plural.integerDigits '.' count % 10 == 4)
        )
            || ((Plural.integerDigits '.' count % 1000 == 100)
                    || (Plural.integerDigits '.' count % 1000 == 200)
                    || (Plural.integerDigits '.' count % 1000 == 300)
                    || (Plural.integerDigits '.' count % 1000 == 400)
                    || (Plural.integerDigits '.' count % 1000 == 500)
                    || (Plural.integerDigits '.' count % 1000 == 600)
                    || (Plural.integerDigits '.' count % 1000 == 700)
                    || (Plural.integerDigits '.' count % 1000 == 800)
                    || (Plural.integerDigits '.' count % 1000 == 900)
               )
    then
        Few
    else if
        (Plural.integerDigits '.' count == 0)
            || ((Plural.integerDigits '.' count % 10 == 6)
                    || ((Plural.integerDigits '.' count % 100 == 40)
                            || (Plural.integerDigits '.' count % 100 == 60)
                            || (Plural.integerDigits '.' count % 100 == 90)
                       )
               )
    then
        Many
    else
        Other
