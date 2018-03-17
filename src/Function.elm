module Function
    exposing
        ( Entry
        , EntryType
            ( Plural
            , PrinterFloat
            )
        , Function
            ( Exposed
            , Internal
            )
        , encodeEntryType
        , exposedEntry
        , exposedName
        , implementation
        , imports
        )

import Json.Encode as Encode exposing (Value)


type Function
    = Internal
        { imports : List String
        , implementation : String
        }
    | Exposed
        { name : String
        , entry : Maybe Entry
        , imports : List String
        , implementation : String
        }


type alias Entry =
    { names : List String
    , type_ : EntryType
    }


type EntryType
    = PrinterFloat
    | Plural


encodeEntryType : EntryType -> Value
encodeEntryType entryType =
    Encode.string <|
        case entryType of
            PrinterFloat ->
                "float"

            Plural ->
                "plural"


exposedName : Function -> Maybe String
exposedName function =
    case function of
        Exposed { name } ->
            Just name

        _ ->
            Nothing


exposedEntry : Function -> Maybe Entry
exposedEntry function =
    case function of
        Exposed { entry } ->
            entry

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
