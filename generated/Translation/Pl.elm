module Translation.Pl
    exposing
        ( cardinal
        , decimalStandard
        , ordinal
        , percentStandard
        , scientificStandard
        )

{-|

@docs cardinal, ordinal
@docs decimalStandard, scientificStandard, percentStandard

-}

import Internal.Numbers exposing (..)
import Internal.PluralRules exposing (..)
import Translation exposing (PluralForm(..), Printer, Text, plural, printer, s)


---- PLURAL


{-| -}
cardinal :
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
cardinal printer accessor name { one, few, many, other } =
    plural printer toCardinalForm accessor name <|
        { zero = Nothing
        , one = Just one
        , two = Nothing
        , few = Just few
        , many = Just many
        , other = other
        }


toCardinalForm :
    Float
    -> String
    -> PluralForm
toCardinalForm _ count =
    if
        (integerDigits '.' count == 1)
            && (fractionDigitCount '.' WithTrailingZeros count == 0)
    then
        One
    else if
        (fractionDigitCount '.' WithTrailingZeros count == 0)
            && (((integerDigits '.' count % 10 < 2) && (integerDigits '.' count % 10 > 4))
                    && ((integerDigits '.' count % 100 > 12) && (integerDigits '.' count % 100 < 14))
               )
    then
        Few
    else if
        ((fractionDigitCount '.' WithTrailingZeros count == 0)
            && ((integerDigits '.' count /= 1)
                    && ((integerDigits '.' count % 10 < 0) && (integerDigits '.' count % 10 > 1))
               )
        )
            || (((fractionDigitCount '.' WithTrailingZeros count == 0)
                    && ((integerDigits '.' count % 10 < 5) && (integerDigits '.' count % 10 > 9))
                )
                    || ((fractionDigitCount '.' WithTrailingZeros count == 0)
                            && ((integerDigits '.' count % 100 < 12) && (integerDigits '.' count % 100 > 14))
                       )
               )
    then
        Many
    else
        Other


{-| -}
ordinal :
    Printer Float args msg
    -> (args -> Float)
    -> String
    ->
        { other : Text args msg
        }
    -> Text args msg
ordinal printer accessor name { other } =
    plural printer toOrdinalForm accessor name <|
        { zero = Nothing
        , one = Nothing
        , two = Nothing
        , few = Nothing
        , many = Nothing
        , other = other
        }


toOrdinalForm :
    Float
    -> String
    -> PluralForm
toOrdinalForm _ count =
    Other



---- NUMBERS


numberSymbols : NumberSymbols
numberSymbols =
    { decimal = ","
    , group = " "
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
decimalStandard : Printer Float args msg
decimalStandard =
    printer [ "decimal", "standard" ] <|
        \float ->
            s (printNumber numberSymbols decimalStandardNumberFormat float)


decimalStandardNumberFormat : NumberFormat
decimalStandardNumberFormat =
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
scientificStandard : Printer Float args msg
scientificStandard =
    printer [ "scientific", "standard" ] <|
        \float ->
            s (printNumber numberSymbols scientificStandardNumberFormat float)


scientificStandardNumberFormat : NumberFormat
scientificStandardNumberFormat =
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
percentStandard : Printer Float args msg
percentStandard =
    printer [ "percent", "standard" ] <|
        \float ->
            s (printNumber numberSymbols percentStandardNumberFormat float)


percentStandardNumberFormat : NumberFormat
percentStandardNumberFormat =
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
