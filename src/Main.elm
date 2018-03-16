module Main exposing (main)

import Data
import Generate
import Json.Decode as Decode exposing (Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Platform
import Ports


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
            Generate.run data
                |> List.map
                    (\file ->
                        [ ( "directory"
                          , file.directory
                                |> List.map Encode.string
                                |> Encode.list
                          )
                        , ( "name", Encode.string file.name )
                        , ( "content", Encode.string file.content )
                        ]
                            |> Encode.object
                            |> Ports.writeModule
                    )
                |> Cmd.batch

        Err error ->
            [ ( "error"
              , error
                    |> Encode.string
              )
            ]
                |> Encode.object
                |> Ports.reportError
    )
