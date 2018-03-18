module Data.Printer
    exposing
        ( Printer
        , PrinterType
            ( Date
            , Delimited
            , Float
            , List
            , StaticList
            , Time
            )
        , decoder
        , encode
        )

{-|

@docs Printer, PrinterType

@docs decoder, encode

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)


{-| -}
type alias Printer =
    { name : String
    , module_ : String
    , type_ : PrinterType
    , icuNames : List String
    }


{-| -}
type PrinterType
    = Delimited
    | StaticList
    | List
    | Float
    | Date
    | Time



---- DECODER


{-| -}
decoder : Decoder Printer
decoder =
    Decode.succeed Printer
        |> Decode.required "name" Decode.string
        |> Decode.required "module" Decode.string
        |> Decode.required "type" printerTypeDecoder
        |> Decode.required "icuNames" (Decode.list Decode.string)


printerTypeDecoder : Decoder PrinterType
printerTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\rawPrinterType ->
                case rawPrinterType of
                    "delimited" ->
                        Decode.succeed Delimited

                    "staticList" ->
                        Decode.succeed StaticList

                    "list" ->
                        Decode.succeed List

                    "float" ->
                        Decode.succeed Float

                    "date" ->
                        Decode.succeed Date

                    "time" ->
                        Decode.succeed Time

                    _ ->
                        Decode.fail ("'" ++ rawPrinterType ++ "' is not a valid printer type")
            )



---- ENCODE


{-| -}
encode : Printer -> Value
encode printer =
    [ ( "name", Encode.string printer.name )
    , ( "module", Encode.string printer.module_ )
    , ( "type", encodeType printer.type_ )
    , ( "icuNames"
      , printer.icuNames
            |> List.map Encode.string
            |> Encode.list
      )
    ]
        |> Encode.object


encodeType : PrinterType -> Value
encodeType printerType =
    case printerType of
        Delimited ->
            Encode.string "delimited"

        StaticList ->
            Encode.string "staticList"

        List ->
            Encode.string "list"

        Float ->
            Encode.string "float"

        Date ->
            Encode.string "date"

        Time ->
            Encode.string "time"
