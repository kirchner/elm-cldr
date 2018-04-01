module Generate.Delimiter exposing (generate)

import Data.Delimiters exposing (Delimiters)
import Data.Function exposing (Function(Exposed, Internal))
import Data.Printer as Printer exposing (Printer, PrinterType)
import Dict exposing (Dict)
import Generate.Helper as Generate


generate : String -> Delimiters -> ( List Function, Dict String Printer )
generate module_ delimiters =
    [ generateQuote module_
        "quote"
        [ "quote" ]
        delimiters.quotationStart
        delimiters.quotationEnd
    , generateQuote module_
        "quoteAlternate"
        [ "quote", "alternate" ]
        delimiters.alternateQuotationStart
        delimiters.alternateQuotationEnd
    ]
        |> List.foldr
            (\( function, ( name, printer ) ) ( functions, printers ) ->
                ( function :: functions
                , Dict.insert name printer printers
                )
            )
            ( [], Dict.empty )


generateQuote :
    String
    -> String
    -> List String
    -> String
    -> String
    -> ( Function, ( String, Printer ) )
generateQuote module_ name icuNames quotationStart quotationEnd =
    ( Exposed
        { name = name
        , imports =
            [ "import Text exposing (Printer, Text, Static, concat, s, printer)" ]
        , implementation =
            [ "{-| -}"
            , Generate.function
                { name = name
                , arguments = []
                , returnType = "Printer (Text Static args node) args node"
                , body =
                    [ "printer <|"
                    , [ "\\text ->"
                      , [ "concat"
                        , [ "s " ++ Generate.string quotationStart
                          , "text"
                          , "s " ++ Generate.string quotationEnd
                          ]
                            |> Generate.list
                            |> Generate.indent
                        ]
                            |> String.join "\n"
                            |> Generate.indent
                      ]
                        |> String.join "\n"
                    ]
                        |> String.join "\n"
                }
            ]
                |> String.join "\n"
        }
    , ( name
      , { module_ = module_
        , type_ = Printer.Delimited
        }
      )
    )
