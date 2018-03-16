module Cldr.Gv
    exposing
        ( cardinal
        , decimalLatnStandard
        , ordinal
        , percentLatnStandard
        , scientificLatnStandard
        , toCardinalForm
        , toOrdinalForm
        )

{-|

@docs cardinal, toCardinalForm, ordinal, toOrdinalForm, decimalLatnStandard, scientificLatnStandard, percentLatnStandard

-}

import Data.Numbers exposing (NumberFormat, Symbols)
import Data.PluralRules exposing (WithTrailingZeros(WithTrailingZeros, WithoutTrailingZeros))
import Printer.Number as Number
import Printer.Plural as Plural
import Translation exposing (PluralForm(Few, Many, One, Other, Two, Zero), Printer, Text, plural, printer, s)


{-| -}
cardinal :
    Printer Float args msg
    -> (args -> Float)
    -> String
    ->
        { one : Text args msg
        , two : Text args msg
        , few : Text args msg
        , many : Text args msg
        , other : Text args msg
        }
    -> Text args msg
cardinal printer accessor name { one, two, few, many, other } =
    plural printer toCardinalForm accessor name <|
        { zero = Nothing
        , one = Just one
        , two = Just two
        , few = Just few
        , many = Just many
        , other = other
        }


{-| -}
toCardinalForm :
    Float
    -> String
    -> PluralForm
toCardinalForm _ count =
    if
        (Plural.fractionDigitCount '.' WithTrailingZeros count == 0)
            && (Plural.integerDigits '.' count % 10 == 1)
    then
        One
    else if
        (Plural.fractionDigitCount '.' WithTrailingZeros count == 0)
            && (Plural.integerDigits '.' count % 10 == 2)
    then
        Two
    else if
        (Plural.fractionDigitCount '.' WithTrailingZeros count == 0)
            && ((Plural.integerDigits '.' count % 100 == 0)
                    || (Plural.integerDigits '.' count % 100 == 20)
                    || (Plural.integerDigits '.' count % 100 == 40)
                    || (Plural.integerDigits '.' count % 100 == 60)
                    || (Plural.integerDigits '.' count % 100 == 80)
               )
    then
        Few
    else if Plural.fractionDigitCount '.' WithTrailingZeros count /= 0 then
        Many
    else
        Other


{-| -}
ordinal :
    Printer Float args msg
    -> (args -> Float)
    -> String
    ->
        { one : Text args msg
        , two : Text args msg
        , few : Text args msg
        , many : Text args msg
        , other : Text args msg
        }
    -> Text args msg
ordinal printer accessor name { one, two, few, many, other } =
    plural printer toOrdinalForm accessor name <|
        { zero = Nothing
        , one = Just one
        , two = Just two
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
        (Plural.fractionDigitCount '.' WithTrailingZeros count == 0)
            && (Plural.integerDigits '.' count % 10 == 1)
    then
        One
    else if
        (Plural.fractionDigitCount '.' WithTrailingZeros count == 0)
            && (Plural.integerDigits '.' count % 10 == 2)
    then
        Two
    else if
        (Plural.fractionDigitCount '.' WithTrailingZeros count == 0)
            && ((Plural.integerDigits '.' count % 100 == 0)
                    || (Plural.integerDigits '.' count % 100 == 20)
                    || (Plural.integerDigits '.' count % 100 == 40)
                    || (Plural.integerDigits '.' count % 100 == 60)
                    || (Plural.integerDigits '.' count % 100 == 80)
               )
    then
        Few
    else if Plural.fractionDigitCount '.' WithTrailingZeros count /= 0 then
        Many
    else
        Other


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
