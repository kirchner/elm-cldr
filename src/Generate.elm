module Generate exposing (run)

import Data exposing (Data)
import Data.Numbers exposing (Numbers)
import Data.PluralRules exposing (PluralRules)
import Dict exposing (Dict)
import Function exposing (Entry)
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
    -> Numbers
    -> ( File, Value )
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
    , functions
        |> List.filterMap
            (\function ->
                Maybe.map2 (,)
                    (Function.exposedEntry function)
                    (Function.exposedName function)
            )
        |> toValue ("Cldr." ++ String.join "." sanitizedLocaleCode)
        |> Encode.object
    )


toValue : String -> List ( Entry, String ) -> List ( String, Value )
toValue moduleName namesList =
    namesList
        |> List.foldl
            (\( { names, type_ }, name ) dict ->
                case names of
                    [] ->
                        dict

                    firstName :: otherNames ->
                        dict
                            |> Dict.update firstName
                                (\maybeEntry ->
                                    case maybeEntry of
                                        Nothing ->
                                            Just <|
                                                case otherNames of
                                                    [] ->
                                                        { single = Just ( name, type_ )
                                                        , multiples = []
                                                        }

                                                    _ ->
                                                        { single = Nothing
                                                        , multiples =
                                                            [ ( { names = otherNames
                                                                , type_ = type_
                                                                }
                                                              , name
                                                              )
                                                            ]
                                                        }

                                        Just entry ->
                                            Just <|
                                                case otherNames of
                                                    [] ->
                                                        { entry | single = Just ( name, type_ ) }

                                                    _ ->
                                                        { entry
                                                            | multiples =
                                                                ( { names = otherNames
                                                                  , type_ = type_
                                                                  }
                                                                , name
                                                                )
                                                                    :: entry.multiples
                                                        }
                                )
            )
            Dict.empty
        |> Dict.foldl
            (\firstName entry collected ->
                ( firstName
                , [ entry.single
                        |> Maybe.map
                            (\( name, type_ ) ->
                                [ ( "_function"
                                  , Encode.string name
                                  )
                                , ( "_type"
                                  , Function.encodeEntryType type_
                                  )
                                , ( "_module"
                                  , Encode.string moduleName
                                  )
                                ]
                            )
                  , Just (toValue moduleName entry.multiples)
                  ]
                    |> List.filterMap identity
                    |> List.concat
                    |> Encode.object
                )
                    :: collected
            )
            []
