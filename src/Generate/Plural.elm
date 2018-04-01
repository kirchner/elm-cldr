module Generate.Plural exposing (generate)

import Data.Function exposing (Function(Exposed, Internal))
import Data.PluralRules exposing (..)
import Data.Pluralization exposing (Pluralization)
import Generate.Helper as Generate
import String.Extra as String


generate :
    String
    -> Maybe PluralRules
    -> Maybe PluralRules
    -> ( List Function, List Pluralization )
generate module_ maybeCardinal maybeOrdinal =
    let
        or second first =
            case second of
                Just a ->
                    Just a

                Nothing ->
                    first
    in
    Maybe.map2
        (\ordinal cardinal ->
            [ generatePlural module_ "cardinal" cardinal
            , generatePlural module_ "ordinal" ordinal
            ]
                |> List.foldr
                    (\( function, pluralization ) ( functions, pluralizations ) ->
                        ( function :: functions
                        , pluralization :: pluralizations
                        )
                    )
                    ( [], [] )
                |> Tuple.mapFirst
                    (\functions ->
                        [ generateSelector "cardinal" cardinal
                        , generateSelector "ordinal" ordinal
                        ]
                            ++ functions
                    )
        )
        (maybeCardinal |> or maybeOrdinal)
        (maybeOrdinal |> or maybeCardinal)
        |> Maybe.withDefault ( [], [] )


generatePlural : String -> String -> PluralRules -> ( Function, Pluralization )
generatePlural module_ kind pluralRules =
    let
        body =
            [ [ "plural"
              , "accessor"
              , "printer"
              , "to" ++ String.toSentenceCase kind ++ "Form"
              , "otherTexts"
              ]
                |> String.join " "
            , pluralCasesAssignment
                |> Generate.indent
            ]
                |> String.join "\n"

        pluralCasesAssignment =
            [ "{"
            , [ pluralCaseAssignment "zero" pluralRules.zero
              , pluralCaseAssignment "one" pluralRules.one
              , pluralCaseAssignment "two" pluralRules.two
              , pluralCaseAssignment "few" pluralRules.few
              , pluralCaseAssignment "many" pluralRules.many
              , "other = other"
              ]
                |> String.join "\n ,"
            , "\n}"
            ]
                |> String.concat

        pluralCaseAssignment name rule =
            if rule == Nothing then
                name ++ " = Nothing"
            else
                name ++ " = Just " ++ name

        pluralCasesType =
            [ "{"
            , [ pluralCaseType "zero" pluralRules.zero
              , pluralCaseType "one" pluralRules.one
              , pluralCaseType "two" pluralRules.two
              , pluralCaseType "few" pluralRules.few
              , pluralCaseType "many" pluralRules.many
              , Just "other : Text Static args msg"
              ]
                |> List.filterMap identity
                |> String.join "\n ,"
            , "\n}"
            ]
                |> String.concat

        pluralCaseType name maybeRule =
            case maybeRule of
                Just _ ->
                    [ name
                    , " : Text Static args msg"
                    ]
                        |> String.concat
                        |> Just

                Nothing ->
                    Nothing

        pluralCasesNames =
            [ "{"
            , [ pluralCaseName "zero" pluralRules.zero
              , pluralCaseName "one" pluralRules.one
              , pluralCaseName "two" pluralRules.two
              , pluralCaseName "few" pluralRules.few
              , pluralCaseName "many" pluralRules.many
              , Just "other"
              ]
                |> List.filterMap identity
                |> String.join " ,"
            , "}"
            ]
                |> String.concat

        pluralCaseName name maybeRule =
            case maybeRule of
                Just _ ->
                    Just name

                Nothing ->
                    Nothing
    in
    ( Exposed
        { name = kind
        , imports =
            [ "import Text exposing (FloatPrinter, Text, Static, plural)"
            ]
        , implementation =
            [ "{-| -}"
            , Generate.function
                { name = kind
                , arguments =
                    [ ( "(args -> Float)", "accessor" )
                    , ( "FloatPrinter args msg", "printer" )
                    , ( "List ( Float, Text Static args msg )", "otherTexts" )
                    , ( pluralCasesType, pluralCasesNames )
                    ]
                , returnType = "Text Static args msg"
                , body = body
                }
            ]
                |> String.join "\n\n"
        }
    , { name = kind
      , module_ = module_
      , pluralForms =
            [ pluralCaseName "zero" pluralRules.zero
            , pluralCaseName "one" pluralRules.one
            , pluralCaseName "two" pluralRules.two
            , pluralCaseName "few" pluralRules.few
            , pluralCaseName "many" pluralRules.many
            , Just "other"
            ]
                |> List.filterMap identity
      , icuNames = [ kind ]
      }
    )


generateSelector : String -> PluralRules -> Function
generateSelector kind pluralRules =
    let
        body =
            case conditions pluralRules of
                [] ->
                    "Other"

                _ ->
                    conditions pluralRules
                        |> Generate.ifThenElseChain "Other"

        conditions pluralRules =
            [ pluralRules.zero |> Maybe.map (generateCondition "Zero")
            , pluralRules.one |> Maybe.map (generateCondition "One")
            , pluralRules.two |> Maybe.map (generateCondition "Two")
            , pluralRules.few |> Maybe.map (generateCondition "Few")
            , pluralRules.many |> Maybe.map (generateCondition "Many")
            ]
                |> List.filterMap identity

        generateCondition pluralType pluralRule =
            ( generateRelation pluralRule, pluralType )

        name =
            "to" ++ String.toSentenceCase kind ++ "Form"
    in
    Exposed
        { name = name
        , imports =
            [ "import Text exposing (FloatInfo, PluralForm(Zero, One, Two, Few, Many, Other))"
            , "import Printer.Plural as Plural"
            , "import Data.PluralRules exposing (WithTrailingZeros(WithTrailingZeros, WithoutTrailingZeros))"
            ]
        , implementation =
            [ "{-| -}"
            , Generate.function
                { name = name
                , arguments =
                    [ ( "Float", "_" )
                    , ( "FloatInfo", "floatInfo" )
                    ]
                , returnType = "PluralForm"
                , body = body
                }
            ]
                |> String.join "\n"
        }


generateRelation : Relation -> String
generateRelation relation =
    case relation of
        Equal expression ranges ->
            [ "("
            , ranges
                |> List.map
                    (\range ->
                        case range of
                            Single int ->
                                [ "("
                                , generateExpression expression
                                , " == "
                                , toString int
                                , ")"
                                ]
                                    |> String.concat

                            Range a b ->
                                [ "("
                                , [ "("
                                  , generateExpression expression
                                  , " < "
                                  , toString a
                                  , ")"
                                  ]
                                    |> String.concat
                                , " && "
                                , [ "("
                                  , generateExpression expression
                                  , " > "
                                  , toString b
                                  , ")"
                                  ]
                                    |> String.concat
                                , ")"
                                ]
                                    |> String.concat
                    )
                |> String.join "\n|| "
            , "\n)"
            ]
                |> String.concat

        NotEqual expression ranges ->
            [ "("
            , ranges
                |> List.map
                    (\range ->
                        case range of
                            Single int ->
                                [ "("
                                , generateExpression expression
                                , " /= "
                                , toString int
                                , ")"
                                ]
                                    |> String.concat

                            Range a b ->
                                [ "("
                                , [ "("
                                  , generateExpression expression
                                  , " > "
                                  , toString a
                                  , ")"
                                  ]
                                    |> String.concat
                                , " && "
                                , [ "("
                                  , generateExpression expression
                                  , " < "
                                  , toString b
                                  , ")"
                                  ]
                                    |> String.concat
                                , ")"
                                ]
                                    |> String.concat
                    )
                |> String.join "\n&& "
            , "\n)"
            ]
                |> String.concat

        And relationA relationB ->
            [ "("
            , generateRelation relationA
            , "\n && "
            , generateRelation relationB
            , ")"
            ]
                |> String.concat

        Or relationA relationB ->
            [ "("
            , generateRelation relationA
            , "\n || "
            , generateRelation relationB
            , ")"
            ]
                |> String.concat


generateExpression : Expression -> String
generateExpression expression =
    case expression of
        Simple pluralOperand ->
            generateOperand False "." pluralOperand

        Modulo pluralOperand modulo ->
            [ "("
            , generateOperand True "." pluralOperand
            , ")"
            , " % "
            , toString modulo
            ]
                |> String.concat


generateOperand : Bool -> String -> PluralOperand -> String
generateOperand round decimal pluralOperand =
    [ "("
    , case pluralOperand of
        AbsoluteValue ->
            if round then
                "floor (floatInfo.absoluteValue)"
            else
                "floatInfo.absoluteValue"

        FractionDigits withTrailingZeros ->
            String.join " "
                [ "Plural.fractionDigits"
                , withTrailingZeros |> toString
                , "floatInfo"
                ]

        IntegerDigits ->
            String.join " "
                [ "Plural.integerDigits", "floatInfo" ]

        FractionDigitCount withTrailingZeros ->
            String.join " "
                [ "Plural.fractionDigitCount"
                , withTrailingZeros |> toString
                , "floatInfo"
                ]
    , ")"
    ]
        |> String.concat
