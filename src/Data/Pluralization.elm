module Data.Pluralization
    exposing
        ( Pluralization
        , decoder
        , encode
        )

{-|

@docs Pluralization

@docs decoder, encode

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)


{-| -}
type alias Pluralization =
    { module_ : String
    , pluralForms : List String
    }



---- DECODER


{-| -}
decoder : Decoder Pluralization
decoder =
    Decode.succeed Pluralization
        |> Decode.required "module" Decode.string
        |> Decode.required "pluralForms" (Decode.list Decode.string)



---- ENCODE


{-| -}
encode : Pluralization -> Value
encode pluralization =
    [ ( "module", Encode.string pluralization.module_ )
    , ( "pluralForms"
      , pluralization.pluralForms
            |> List.map Encode.string
            |> Encode.list
      )
    ]
        |> Encode.object
