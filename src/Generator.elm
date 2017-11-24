port module Generator exposing (main)

import Cldr exposing (..)
import Dict
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Platform exposing (programWithFlags)


port exportResult : Value -> Cmd msg


type alias Flags =
    { numberFormatsJsons : List ( String, String )
    , cardinalsJson : String
    , ordinalsJson : String
    }


main : Program Flags {} Never
main =
    programWithFlags
        { init = init
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


init : Flags -> ( {}, Cmd Never )
init flags =
    ( {}
    , exportCldr
        (cldr
            (flags.numberFormatsJsons |> Dict.fromList)
            flags.cardinalsJson
            flags.ordinalsJson
        )
    )



---- EXPORT


exportCldr : Cldr -> Cmd msg
exportCldr cldr =
    let
        modules =
            cldr.locales
                |> Dict.map generateLocaleModule
                |> Dict.values
                |> List.map
                    (\{ filename, content } ->
                        [ ( "filename", filename |> Encode.string )
                        , ( "content", content |> Encode.string )
                        ]
                            |> Encode.object
                    )
                |> Encode.list
    in
    [ ( "modules"
      , modules
      )
    ]
        |> Encode.object
        |> exportResult
