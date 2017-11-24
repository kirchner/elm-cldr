module Generate
    exposing
        ( caseOf
        , function
        , ifThenElseChain
        , indent
        , list
        , moduIe
        , record
        , recordPatternMatch
        , recordType
        , string
        , tvpe
        )

import String.Extra as String


moduIe :
    { name : String
    , exposed : List String
    }
    -> String
moduIe { name, exposed } =
    [ "module "
    , name
    , " exposing\n"
    , [ "( "
      , exposed
            |> String.join "\n, "
      , "\n)"
      ]
        |> String.concat
        |> indent
    ]
        |> String.concat


function :
    { name : String
    , arguments : List ( String, String )
    , returnType : String
    , body : String
    }
    -> String
function { name, arguments, returnType, body } =
    let
        argumentNames =
            arguments |> List.map Tuple.first

        argumentTypes =
            arguments |> List.map Tuple.second
    in
    [ name
    , " : \n"
    , [ argumentNames
      , [ returnType ]
      ]
        |> List.concat
        |> String.join "\n-> "
        |> indent
    , "\n"
    , (name :: argumentTypes)
        |> String.join " "
    , " =\n"
    , body |> indent
    ]
        |> String.concat


string : String -> String
string text =
    [ "\""
    , text
    , "\""
    ]
        |> String.concat


list : List String -> String
list elements =
    [ "[ "
    , elements
        |> String.join "\n, "
    , "\n]\n"
    ]
        |> String.concat


caseOf :
    { expression : String
    , cases : List ( String, String )
    }
    -> String
caseOf { expression, cases } =
    [ "case "
    , expression
    , " of\n"
    , cases
        |> List.map
            (\( value, expression ) ->
                [ value
                , " ->\n"
                , expression |> indent
                ]
                    |> String.concat
            )
        |> List.intersperse "\n\n"
        |> String.concat
        |> indent
    ]
        |> String.concat


tvpe :
    { name : String
    , constructors : List String
    }
    -> String
tvpe { name, constructors } =
    [ "type "
    , name
    , "\n"
    , [ "= "
      , constructors |> String.join "\n| "
      ]
        |> String.concat
        |> indent
    ]
        |> String.concat


record : List ( String, String ) -> String
record fields =
    let
        field ( key, value ) =
            [ key
            , " = "
            , value
            ]
                |> String.concat
    in
    [ "{ "
    , fields
        |> List.map field
        |> String.join "\n, "
    , "\n}"
    ]
        |> String.concat


recordType : List ( String, String ) -> String
recordType fields =
    let
        field ( key, tvpe ) =
            [ key
            , " : "
            , tvpe
            ]
                |> String.concat
    in
    [ "{ "
    , fields
        |> List.map field
        |> String.join "\n, "
    , "\n}"
    ]
        |> String.concat


recordPatternMatch : List String -> String
recordPatternMatch keys =
    [ "{ "
    , keys
        |> String.join ", "
    , " }"
    ]
        |> String.concat


ifThenElseChain : String -> List ( String, String ) -> String
ifThenElseChain elseCondition ifConditions =
    let
        generateIfCondition ( condition, code ) =
            [ "if\n"
            , condition
                |> indent
            , "\nthen\n"
            , code
                |> indent
            , "\n"
            ]
                |> String.concat
    in
    [ ifConditions
        |> List.map generateIfCondition
        |> String.join "else "
    , "else\n"
    , elseCondition
        |> indent
    ]
        |> String.concat


indent : String -> String
indent text =
    text
        |> String.split "\n"
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"
