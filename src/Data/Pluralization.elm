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
import Text exposing (PluralForm(Few, Many, One, Other, Two, Zero))


{-| -}
type alias Pluralization =
    { module_ : String
    , pluralForms : List PluralForm
    }



---- DECODER


{-| -}
decoder : Decoder Pluralization
decoder =
    Decode.succeed Pluralization
        |> Decode.required "module" Decode.string
        |> Decode.required "pluralForms" (Decode.list pluralFormDecoder)


pluralFormDecoder : Decoder PluralForm
pluralFormDecoder =
    Decode.string
        |> Decode.andThen
            (\rawPluralForm ->
                case rawPluralForm of
                    "zero" ->
                        Decode.succeed Zero

                    "one" ->
                        Decode.succeed One

                    "two" ->
                        Decode.succeed Two

                    "few" ->
                        Decode.succeed Few

                    "many" ->
                        Decode.succeed Many

                    "other" ->
                        Decode.succeed Other

                    _ ->
                        Decode.fail <|
                            "'"
                                ++ rawPluralForm
                                ++ "' is not a valid plural form"
            )



---- ENCODE


{-| -}
encode : Pluralization -> Value
encode pluralization =
    [ ( "module", Encode.string pluralization.module_ )
    , ( "pluralForms"
      , pluralization.pluralForms
            |> List.map encodePluralForm
            |> Encode.list
      )
    ]
        |> Encode.object


encodePluralForm : PluralForm -> Value
encodePluralForm pluralForm =
    Encode.string <|
        case pluralForm of
            Zero ->
                "zero"

            One ->
                "one"

            Two ->
                "two"

            Few ->
                "few"

            Many ->
                "many"

            Other ->
                "other"
