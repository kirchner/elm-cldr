port module Main exposing (main)

import Data
import Generate
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Platform


port reportError : Value -> Cmd msg


port writeModule : Value -> Cmd msg


main : Program Value {} msg
main =
    Platform.programWithFlags
        { init = init
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


init : Value -> ( {}, Cmd msg )
init value =
    ( {}
    , case
        value
            |> Decode.decodeValue Data.rawDataDecoder
            |> Result.andThen Data.decode
      of
        Ok data ->
            let
                ( modules, functionJson ) =
                    Generate.run data
                        |> List.foldl
                            (\( file, ( localeCode, value ) ) ( cmds, values ) ->
                                ( ([ ( "directory"
                                     , file.directory
                                        |> List.map Encode.string
                                        |> Encode.list
                                     )
                                   , ( "name", Encode.string (file.name ++ ".elm") )
                                   , ( "content", Encode.string file.content )
                                   ]
                                    |> Encode.object
                                  )
                                    :: cmds
                                , ( localeCode, value ) :: values
                                )
                            )
                            ( [], [] )
            in
            [ modules
                |> List.map writeModule
                |> Cmd.batch
            , [ ( "directory"
                , Encode.list []
                )
              , ( "name"
                , Encode.string "generated.json"
                )
              , ( "content"
                , functionJson
                    |> Encode.object
                    |> Encode.encode 4
                    |> Encode.string
                )
              ]
                |> Encode.object
                |> writeModule
            ]
                |> Cmd.batch

        Err error ->
            [ ( "error"
              , error
                    |> Encode.string
              )
            ]
                |> Encode.object
                |> reportError
    )
