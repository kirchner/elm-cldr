module Misc exposing (Function)


type alias Function =
    { export : Maybe String
    , imports : List String
    , implementation : String
    }
