module Data.Function
    exposing
        ( Function
            ( Exposed
            , Internal
            )
        , exposedName
        , implementation
        , imports
        )


type Function
    = Internal
        { imports : List String
        , implementation : String
        }
    | Exposed
        { name : String
        , imports : List String
        , implementation : String
        }


exposedName : Function -> Maybe String
exposedName function =
    case function of
        Exposed { name } ->
            Just name

        _ ->
            Nothing


imports : Function -> List String
imports function =
    case function of
        Internal { imports } ->
            imports

        Exposed { imports } ->
            imports


implementation : Function -> String
implementation function =
    case function of
        Internal { implementation } ->
            implementation

        Exposed { implementation } ->
            implementation
