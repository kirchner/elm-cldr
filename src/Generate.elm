module Generate exposing (run)

import Data exposing (Data)
import Data.Numbers exposing (Numbers)
import Data.PluralRules exposing (PluralRules)
import Dict exposing (Dict)
import Generate.Helper as Generate
import Generate.Number as Number
import Generate.Plural as Plural
import String.Extra as String


type alias File =
    { directory : List String
    , name : String
    , content : String
    }


run : Data -> List File
run data =
    let
        pluralRules =
            Dict.merge
                (\localeCode cardinal ->
                    Dict.insert localeCode
                        { cardinal = Just cardinal
                        , ordinal = Nothing
                        }
                )
                (\localeCode cardinal ordinal ->
                    Dict.insert localeCode
                        { cardinal = Just cardinal
                        , ordinal = Just ordinal
                        }
                )
                (\localeCode ordinal ->
                    Dict.insert localeCode
                        { cardinal = Nothing
                        , ordinal = Just ordinal
                        }
                )
                data.cardinals
                data.ordinals
                Dict.empty
    in
    data.main
        |> Dict.map
            (\localeCode localeData ->
                case localeCode of
                    [] ->
                        Debug.crash "invalid locale code"

                    mainCode :: rest ->
                        generateLocaleModule localeCode
                            (Dict.get mainCode pluralRules)
                            localeData.numbers
            )
        |> Dict.values


generateLocaleModule :
    List String
    ->
        Maybe
            { cardinal : Maybe PluralRules
            , ordinal : Maybe PluralRules
            }
    -> Numbers
    -> File
generateLocaleModule localeCode pluralRules numbers =
    let
        sanitizedLocaleCode =
            localeCode
                |> List.map (String.camelize >> String.toSentenceCase)

        ( name, directory ) =
            ( sanitizedLocaleCode
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ""
            , sanitizedLocaleCode
                |> List.reverse
                |> List.drop 1
                |> List.reverse
            )

        functions =
            [ pluralFunctions
            , Just numberFunctions
            ]
                |> List.filterMap identity
                |> List.concat

        pluralFunctions =
            Maybe.map2 Plural.generate
                (Maybe.map .cardinal pluralRules)
                (Maybe.map .ordinal pluralRules)

        numberFunctions =
            Number.generate
                numbers.symbols
                numbers.decimalFormats
                numbers.scientificFormats
                numbers.percentFormats
                numbers.currencyFormats
    in
    { directory = [ "generated", "Cldr" ] ++ directory
    , name = name
    , content =
        [ Generate.module_
            { name = String.join "." ("Cldr" :: directory ++ [ name ])
            , exposed = List.filterMap .export functions
            }
        , [ "{-|"
          , [ "@docs"
            , functions
                |> List.filterMap .export
                |> String.join ", "
            ]
                |> String.join " "
          , "-}"
          ]
            |> String.join "\n"
        , functions
            |> List.map .imports
            |> List.concat
            |> String.join "\n"
        , functions
            |> List.map .implementation
            |> String.join "\n\n"
        ]
            |> String.join "\n\n"
    }
