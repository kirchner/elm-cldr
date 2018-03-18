module Generate exposing (run)

import Data exposing (Data)
import Data.Delimiters exposing (Delimiters)
import Data.Function as Function
import Data.Numbers exposing (Numbers)
import Data.PluralRules exposing (PluralRules)
import Data.Printer as Printer
import Dict exposing (Dict)
import Generate.Delimiter as Delimiter
import Generate.Helper as Generate
import Generate.Number as Number
import Generate.Plural as Plural
import Json.Encode as Encode exposing (Value)
import String.Extra as String


type alias File =
    { directory : List String
    , name : String
    , content : String
    }


run : Data -> List ( File, ( String, Value ) )
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
                            localeData.delimiters
                            localeData.numbers
                            |> Tuple.mapSecond
                                (\value ->
                                    ( String.join "-" localeCode
                                    , value
                                    )
                                )
            )
        |> Dict.values


generateLocaleModule :
    List String
    ->
        Maybe
            { cardinal : Maybe PluralRules
            , ordinal : Maybe PluralRules
            }
    -> Delimiters
    -> Numbers
    -> ( File, Value )
generateLocaleModule localeCode pluralRules delimiters numbers =
    let
        sanitizedLocaleCode =
            localeCode
                |> List.map (String.camelize >> String.toSentenceCase)

        moduleName =
            String.join "." ("Cldr" :: sanitizedLocaleCode)

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
            [ Just delimiterFunctions
            , Just numberFunctions
            , pluralFunctions
            ]
                |> List.filterMap identity
                |> List.concat

        printers =
            [ delimiterPrinters
            , numberPrinters
            ]
                |> List.concat

        ( delimiterFunctions, delimiterPrinters ) =
            Delimiter.generate moduleName delimiters

        ( numberFunctions, numberPrinters ) =
            Number.generate moduleName
                numbers.symbols
                numbers.decimalFormats
                numbers.scientificFormats
                numbers.percentFormats
                numbers.currencyFormats

        pluralFunctions =
            Maybe.map2 Plural.generate
                (Maybe.map .cardinal pluralRules)
                (Maybe.map .ordinal pluralRules)
    in
    ( { directory = [ "generated", "Cldr" ] ++ directory
      , name = name
      , content =
            [ Generate.module_
                { name = String.join "." ("Cldr" :: directory ++ [ name ])
                , exposed = List.filterMap Function.exposedName functions
                }
            , [ "{-|"
              , [ "@docs"
                , functions
                    |> List.filterMap Function.exposedName
                    |> String.join ", "
                ]
                    |> String.join " "
              , "-}"
              ]
                |> String.join "\n"
            , functions
                |> List.map Function.imports
                |> List.concat
                |> String.join "\n"
            , functions
                |> List.map Function.implementation
                |> String.join "\n\n"
            ]
                |> String.join "\n\n"
      }
    , printers
        |> List.map Printer.encode
        |> Encode.list
    )
