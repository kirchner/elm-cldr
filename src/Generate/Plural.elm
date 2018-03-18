module Generate.Plural exposing (generate)

import Data.Function exposing (Function(Exposed, Internal))
import Data.PluralRules exposing (..)
import Generate.Helper as Generate
import String.Extra as String


generate : Maybe PluralRules -> Maybe PluralRules -> List Function
generate maybeCardinal maybeOrdinal =
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
            [ generatePlural "cardinal" cardinal
            , generateSelector "cardinal" cardinal
            , generatePlural "ordinal" ordinal
            , generateSelector "ordinal" ordinal
            ]
        )
        (maybeCardinal |> or maybeOrdinal)
        (maybeOrdinal |> or maybeCardinal)
        |> Maybe.withDefault []


generatePlural : String -> PluralRules -> Function
generatePlural kind pluralRules =
    let
        body =
            [ [ "plural"
              , "printer"
              , "to" ++ String.toSentenceCase kind ++ "Form"
              , "accessor"
              , "name"
              , "<|"
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
              , Just "other : Text args msg"
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
                    , " : Text args msg"
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
    Exposed
        { name = kind
        , imports =
            [ "import Translation exposing (Printer, Text, plural)"
            ]
        , implementation =
            [ "{-| -}"
            , Generate.function
                { name = kind
                , arguments =
                    [ ( "Printer Float args msg", "printer" )
                    , ( "(args -> Float)", "accessor" )
                    , ( "String", "name" )
                    , ( pluralCasesType, pluralCasesNames )
                    ]
                , returnType = "Text args msg"
                , body = body
                }
            ]
                |> String.join "\n\n"
        }


generateSelector : String -> PluralRules -> Function
generateSelector kind pluralRules =
    let
        generateBody pluralRules =
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
            [ "import Translation exposing (PluralForm(Zero, One, Two, Few, Many, Other))"
            , "import Printer.Plural as Plural"
            , "import Data.PluralRules exposing (WithTrailingZeros(WithTrailingZeros, WithoutTrailingZeros))"
            ]
        , implementation =
            [ "{-| -}"
            , Generate.function
                { name = name
                , arguments =
                    [ ( "Float", "_" )
                    , ( "String", "count" )
                    ]
                , returnType = "PluralForm"
                , body = generateBody pluralRules
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
    , String.concat <|
        case pluralOperand of
            AbsoluteValue ->
                if round then
                    [ "floor (Plural.absoluteValue "
                    , "'"
                    , decimal
                    , "'"
                    , " count)"
                    ]
                else
                    [ "Plural.absoluteValue "
                    , "'"
                    , decimal
                    , "'"
                    , " count"
                    ]

            FractionDigits withTrailingZeros ->
                [ "Plural.fractionDigits "
                , "'"
                , decimal
                , "'"
                , " "
                , withTrailingZeros |> toString
                , " count"
                ]

            IntegerDigits ->
                [ "Plural.integerDigits "
                , "'"
                , decimal
                , "'"
                , " count"
                ]

            FractionDigitCount withTrailingZeros ->
                [ "Plural.fractionDigitCount "
                , "'"
                , decimal
                , "'"
                , " "
                , withTrailingZeros |> toString
                , " count"
                ]
    , ")"
    ]
        |> String.concat
