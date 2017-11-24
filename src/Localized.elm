module Localized
    exposing
        ( Part
        , count
        , customPlural
        , print
        , s
        , string
        )

import Internal.Localized exposing (..)


type Part args
    = Verbatim String
    | String (args -> String)
    | Plural (args -> Float) (Float -> PluralCase) (AllPluralCases args)
    | Count


type alias AllPluralCases args =
    { zero : List (Part args)
    , one : List (Part args)
    , two : List (Part args)
    , few : List (Part args)
    , many : List (Part args)
    , other : List (Part args)
    }



---- SHARED PARTS


s : String -> Part args
s text =
    Verbatim text


string : (args -> String) -> Part args
string accessor =
    String accessor


count : Part args
count =
    Count


customPlural :
    (args -> Float)
    -> (Float -> PluralCase)
    ->
        { zero : List (Part args)
        , one : List (Part args)
        , two : List (Part args)
        , few : List (Part args)
        , many : List (Part args)
        , other : List (Part args)
        }
    -> Part args
customPlural accessor selector cases =
    Plural accessor selector cases



---- PRINT


print : args -> List (Part args) -> String
print args parts =
    parts
        |> List.map (printPart args)
        |> String.concat


printWithCount : String -> args -> List (Part args) -> String
printWithCount count args parts =
    parts
        |> List.map (printPartWithCount count args)
        |> String.concat


printPart : args -> Part args -> String
printPart args part =
    case part of
        Verbatim text ->
            text

        String accessor ->
            accessor args

        Plural accessor selector cases ->
            let
                count =
                    -- TODO: insert actual number printing
                    args |> accessor |> toString
            in
            case args |> accessor |> selector of
                Zero ->
                    cases.zero
                        |> printWithCount count args

                One ->
                    cases.one
                        |> printWithCount count args

                Two ->
                    cases.two
                        |> printWithCount count args

                Few ->
                    cases.few
                        |> printWithCount count args

                Many ->
                    cases.many
                        |> printWithCount count args

                Other ->
                    cases.other
                        |> printWithCount count args

        Count ->
            Debug.crash "no count given"


printPartWithCount : String -> args -> Part args -> String
printPartWithCount count args part =
    case part of
        Verbatim text ->
            text

        String accessor ->
            accessor args

        Plural accessor selector cases ->
            case args |> accessor |> selector of
                Zero ->
                    cases.zero
                        |> print args

                One ->
                    cases.one
                        |> print args

                Two ->
                    cases.two
                        |> print args

                Few ->
                    cases.few
                        |> print args

                Many ->
                    cases.many
                        |> print args

                Other ->
                    cases.other
                        |> print args

        Count ->
            count
