module Internal.Supplemental exposing (Supplemental, decode)


type alias Supplemental =
    { cardinal : Dict String PluralRules
    , ordinal : Dict String PluralRules
    }
