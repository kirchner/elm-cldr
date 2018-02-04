module Translation.Cy
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
        { zero : Text args msg
        , one : Text args msg
        , two : Text args msg
        , few : Text args msg
        , many : Text args msg
        , other : Text args msg
        }
    -> Text args msg
cardinal printer accessor name { zero, one, two, few, many, other } =
    plural printer toCardinalForm accessor name <|
        { zero = Just zero
        , one = Just one
        , two = Just two
        , few = Just few
        , many = Just many
        , other = other
        }


toCardinalForm :
    Float
    -> String
    -> PluralForm
toCardinalForm _ count =
    if absoluteValue '.' count == 0 then
        Zero
    else if absoluteValue '.' count == 1 then
        One
    else if absoluteValue '.' count == 2 then
        Two
    else if absoluteValue '.' count == 3 then
        Few
    else if absoluteValue '.' count == 6 then
        Many
    else
        Other


{-| -}
ordinal :
    Printer Float args msg
    -> (args -> Float)
    -> String
    ->
        { zero : Text args msg
        , one : Text args msg
        , two : Text args msg
        , few : Text args msg
        , many : Text args msg
        , other : Text args msg
        }
    -> Text args msg
ordinal printer accessor name { zero, one, two, few, many, other } =
    plural printer toOrdinalForm accessor name <|
        { zero = Just zero
        , one = Just one
        , two = Just two
        , few = Just few
        , many = Just many
        , other = other
        }


toOrdinalForm :
    Float
    -> String
    -> PluralForm
toOrdinalForm _ count =
    if
        (absoluteValue '.' count == 0)
            || (absoluteValue '.' count == 7)
            || (absoluteValue '.' count == 8)
            || (absoluteValue '.' count == 9)
    then
        Zero
    else if absoluteValue '.' count == 1 then
        One
    else if absoluteValue '.' count == 2 then
        Two
    else if
        (absoluteValue '.' count == 3)
            || (absoluteValue '.' count == 4)
    then
        Few
    else if
        (absoluteValue '.' count == 5)
            || (absoluteValue '.' count == 6)
    then
        Many
    else
        Other



---- NUMBERS


numberSymbols : NumberSymbols
numberSymbols =
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
