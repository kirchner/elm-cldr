module Cldr.Bn
    exposing
        ( cardinal
        , currencyBengAccounting
        , currencyBengStandard
        , currencyLatnAccounting
        , currencyLatnStandard
        , decimalBengStandard
        , decimalLatnStandard
        , ordinal
        , percentBengStandard
        , percentLatnStandard
        , scientificBengStandard
        , scientificLatnStandard
        , toCardinalForm
        , toOrdinalForm
        )

{-|

@docs cardinal, toCardinalForm, ordinal, toOrdinalForm, decimalBengStandard, decimalLatnStandard, scientificBengStandard, scientificLatnStandard, percentBengStandard, percentLatnStandard, currencyBengStandard, currencyBengAccounting, currencyLatnStandard, currencyLatnAccounting

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
    if
        (Plural.integerDigits '.' count == 0)
            || (Plural.absoluteValue '.' count == 1)
    then
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
        (Plural.absoluteValue '.' count == 1)
            || (Plural.absoluteValue '.' count == 5)
            || (Plural.absoluteValue '.' count == 7)
            || (Plural.absoluteValue '.' count == 8)
            || (Plural.absoluteValue '.' count == 9)
            || (Plural.absoluteValue '.' count == 10)
    then
        One
    else if
        (Plural.absoluteValue '.' count == 2)
            || (Plural.absoluteValue '.' count == 3)
    then
        Two
    else if Plural.absoluteValue '.' count == 4 then
        Few
    else if Plural.absoluteValue '.' count == 6 then
        Many
    else
        Other


bengNumberSymbols : Symbols
bengNumberSymbols =
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
decimalBengStandard : Printer Float args msg
decimalBengStandard =
    printer [ "decimal", "beng", "standard" ] <|
        \float ->
            s (Number.print bengNumberSymbols decimalBengStandardNumberFormat float)


decimalBengStandardNumberFormat : NumberFormat
decimalBengStandardNumberFormat =
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
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 0
        , maximalFractionCount = 3
        }
    , negativePattern = Nothing
    }


{-| -}
scientificBengStandard : Printer Float args msg
scientificBengStandard =
    printer [ "scientific", "beng", "standard" ] <|
        \float ->
            s (Number.print bengNumberSymbols scientificBengStandardNumberFormat float)


scientificBengStandardNumberFormat : NumberFormat
scientificBengStandardNumberFormat =
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
percentBengStandard : Printer Float args msg
percentBengStandard =
    printer [ "percent", "beng", "standard" ] <|
        \float ->
            s (Number.print bengNumberSymbols percentBengStandardNumberFormat float)


percentBengStandardNumberFormat : NumberFormat
percentBengStandardNumberFormat =
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
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 0
        , maximalFractionCount = 0
        }
    , negativePattern = Nothing
    }


{-| -}
currencyBengStandard : Printer Float args msg
currencyBengStandard =
    printer [ "currency", "beng", "standard" ] <|
        \float ->
            s (Number.print bengNumberSymbols currencyBengStandardNumberFormat float)


currencyBengStandardNumberFormat : NumberFormat
currencyBengStandardNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 3
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 2
        , maximalFractionCount = 3
        }
    , negativePattern = Nothing
    }


{-| -}
currencyBengAccounting : Printer Float args msg
currencyBengAccounting =
    printer [ "currency", "beng", "accounting" ] <|
        \float ->
            s (Number.print bengNumberSymbols currencyBengAccountingNumberFormat float)


currencyBengAccountingNumberFormat : NumberFormat
currencyBengAccountingNumberFormat =
    { positivePattern =
        { prefix = ""
        , suffix = ""
        , primaryGroupingSize = Just 3
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 2
        , maximalFractionCount = 3
        }
    , negativePattern =
        Just
            { prefix = ""
            , suffix = ""
            , primaryGroupingSize = Just 3
            , secondaryGroupingSize = Just 2
            , minimalIntegerCount = 1
            , minimalFractionCount = 2
            , maximalFractionCount = 4
            }
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
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 2
        , maximalFractionCount = 3
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
        , secondaryGroupingSize = Just 2
        , minimalIntegerCount = 1
        , minimalFractionCount = 2
        , maximalFractionCount = 3
        }
    , negativePattern =
        Just
            { prefix = ""
            , suffix = ""
            , primaryGroupingSize = Just 3
            , secondaryGroupingSize = Just 2
            , minimalIntegerCount = 1
            , minimalFractionCount = 2
            , maximalFractionCount = 4
            }
    }
