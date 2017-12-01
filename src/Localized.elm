module Localized
    exposing
        ( AllPluralCases
        , NumberFormat
        , Part
        , PluralCase
            ( Few
            , Many
            , One
            , Other
            , Two
            , Zero
            )
        , count
        , customNumberFormat
        , customPlural
        , decimal
        , node
        , nodes
        , print
        , printWith
        , s
        , select
        , string
        )

{-| Create localized texts in a type safe way.

@docs Part


# Printing

@docs printWith, print, nodes


# Creating Parts


## Basic Parts

@docs s, string, node


## Number Formatting

@docs decimal, NumberFormat, customNumberFormat


## Pluralization and Selects

Every locale module like for example `Localized.En` exports
the pluralization parts `cardinal` and `ordinal`. You use these like
this

    emailInfo : List (Part { ord : Float } msg)
    emailInfo =
        [ Localized.En.ordinal .ord
            Localized.En.decimalStandard
            { one =
                [ s "This is the "
                , count
                , "st message."
                ]
            , two =
                [ s "This is the "
                , count
                , "nd message."
                ]
            , few =
                [ s "This is the "
                , count
                , "rd message."
                ]
            , other =
                [ s "This is the "
                , count
                , "th message."
                ]
            }
        ]

@docs count, customPlural, PluralCase, AllPluralCases

@docs select

-}

import Char
import Internal.Numbers
import Internal.PluralRules
import VirtualDom exposing (Node)


{-| Opaque building block for texts.
-}
type Part args msg
    = Verbatim String
    | NodePart (args -> (List (Node msg) -> Node msg)) (List (Part args msg))
    | String (args -> String)
    | Decimal (args -> Float) NumberFormat
    | Plural (args -> Float) NumberFormat (String -> PluralCase) (AllPluralCases args msg)
    | Count


{-| Create a part which returns the given `String`.

    greeting : List (Part {} msg)
    greeting =
        [ s "Hello!" ]

Then `"print greeting"` is equal to `"Hello!"`.

-}
s : String -> Part args msg
s text =
    Verbatim text


{-| Create a placeholder which eventually gets replaced by a `String`.

    personalGreeting : List (Part { name : String } msg)
    personalGreeting =
        [ s "Hello, "
        , string .name
        , s "!"
        ]

Then `printWith { name = "Alice" } personalGreeting` is equal to
`"Hello, Alice!"`.

-}
string : (args -> String) -> Part args msg
string accessor =
    String accessor


{-| Create a placeholder which gets replaced by a number which is
formatted according to the provided format.

    heightInfo : List (Part { height : Float } msg)
    heightInfo =
        [ s "Current height: "
        , decimal .height Localized.En.decimalStandard
        , s "."
        ]

Then `printWith { height = 3.1533 } heightInfo` is equal to `"Current
height: 3.153."`.

-}
decimal : (args -> Float) -> NumberFormat -> Part args msg
decimal accessor numberFormat =
    Decimal accessor numberFormat


{-| Opaque type representing a number formatting rule. Every locale
module provides functions to create formats which are specified in the
CLDR.
-}
type NumberFormat
    = CustomNumberFormat (Float -> String)


{-| Create your own number format.
-}
customNumberFormat : (Float -> String) -> NumberFormat
customNumberFormat printer =
    CustomNumberFormat printer


{-| Create a part which eventually gets replaced by a dom node.

    documentationInfo : List (Part { link : List (Html msg) -> Html msg } msg)
    documentationInfo =
        [ s "Take a look at our "
        , node .link
            [ s "documentation" ]
        , "."
        ]

    view : String -> Html msg
    view url =
        Html.div []
            (nodes documentationInfo
                { link = Html.a [ Html.Attributes.href url ] }
            )

Then `view` is equivalent to

    view : String -> Html msg
    view url =
        Html.div []
            [ Html.text "Take a look at our "
            , Html.a [ Html.Attributes.href url ]
                [ Html.text "documentation" ]
            , Html.text "."
            ]

-}
node :
    (args -> (List (Node msg) -> Node msg))
    -> List (Part args msg)
    -> Part args msg
node accessor nextParts =
    NodePart accessor nextParts


{-| Create a custom pluralization part. Use this function if you want
to provide your own pluralization rules.

**Note**: You usually want to use the `plural` function from the one of
the modules like `Localized.En`, according to the language the text
will be in.

-}
customPlural :
    (args -> Float)
    -> NumberFormat
    -> (String -> PluralCase)
    -> AllPluralCases args msg
    -> Part args msg
customPlural accessor numberFormat selector cases =
    Plural accessor numberFormat selector cases


{-| Create a part which gets replaced by the formatted number argument
of a plural part.

    emailInfo : List (Part { count : Float } msg)
    emailInfo =
        [ Localized.En.cardinal .count
            Localized.En.decimalStandard
            { one = [ s "You have one new email" ]
            , other =
                [ s "You have "
                , count
                , s " new emails"
                ]
            }
        ]

-}
count : Part args msg
count =
    Count


{-| This type represents the different plural cases which are used in
the [CLDR](http://cldr.unicode.org).
-}
type PluralCase
    = Zero
    | One
    | Two
    | Few
    | Many
    | Other


{-| -}
type alias AllPluralCases args msg =
    { zero : List (Part args msg)
    , one : List (Part args msg)
    , two : List (Part args msg)
    , few : List (Part args msg)
    , many : List (Part args msg)
    , other : List (Part args msg)
    }


{-| Create a part which selects between different versions using the
provided function.

    partyInfo : List (Part { gender : Gender } msg)
    partyInfo =
        [ select .gender <|
            \gender ->
                case gender of
                    Female ->
                        [ s "She gives a party." ]

                    Male ->
                        [ s "He gives a party." ]

                    Other ->
                        [ s "They give a party." ]
        ]

    type Gender
        = Female
        | Male
        | Other

-}
select : (args -> a) -> (a -> List (Part args msg)) -> Part args msg
select accessor selector =
    Debug.crash "TODO"



---- PRINT


{-| Use this function to turn a list of parts into a `String`.

    greeting : String
    greeting =
        printWith { name = "Alice" }
            [ s "Good morning, "
            , string .name
            , s "!"
            ]

Then `greeting` is equal to `"Good morning, Alice!"`.

-}
printWith : args -> List (Part args msg) -> String
printWith args parts =
    let
        printPart maybeCount part =
            case part of
                Verbatim text ->
                    text

                NodePart accessor nextParts ->
                    nextParts
                        |> List.map (printPart maybeCount)
                        |> String.concat

                String accessor ->
                    accessor args

                Decimal accessor (CustomNumberFormat printer) ->
                    args |> accessor |> printer

                Plural accessor (CustomNumberFormat printer) selector cases ->
                    let
                        nextCount =
                            args |> accessor |> printer
                    in
                    String.concat <|
                        List.map (printPart (Just nextCount)) <|
                            case nextCount |> selector of
                                Zero ->
                                    cases.zero

                                One ->
                                    cases.one

                                Two ->
                                    cases.two

                                Few ->
                                    cases.few

                                Many ->
                                    cases.many

                                Other ->
                                    cases.other

                Count ->
                    case maybeCount of
                        Just count ->
                            count

                        Nothing ->
                            Debug.crash "no count given"
    in
    parts
        |> List.map (printPart Nothing)
        |> String.concat


{-| Use this function if your list of parts does not need arguments.
This is basically `print parts = printWith {} parts`.
-}
print : List (Part {} msg) -> String
print parts =
    printWith {} parts


{-| Use this function to turn a list of parts into a list of dom nodes.
You want to do this if one of the parts is a `node`.

    documentationInfo : List (Part { link : List (Html msg) -> Html msg } msg)
    documentationInfo =
        [ s "Take a look at our "
        , node .link
            [ s "documentation" ]
        , "."
        ]

    view : String -> Html msg
    view url =
        Html.div [] <|
            nodes { link = Html.a [ Html.Attributes.href url ] }
                documentationInfo

Then `view` is equivalent to

    view : String -> Html msg
    view url =
        Html.div []
            [ Html.text "Take a look at our "
            , Html.a [ Html.Attributes.href url ]
                [ Html.text "documentation" ]
            , Html.text "."
            ]

-}
nodes : args -> List (Part args msg) -> List (Node msg)
nodes args parts =
    let
        toNodes : Maybe String -> Part args msg -> List (Node msg)
        toNodes maybeCount part =
            case part of
                Verbatim text ->
                    [ VirtualDom.text text ]

                NodePart accessor nextParts ->
                    [ nextParts
                        |> List.map (toNodes maybeCount)
                        |> List.concat
                        |> accessor args
                    ]

                String accessor ->
                    [ args
                        |> accessor
                        |> VirtualDom.text
                    ]

                Decimal accessor (CustomNumberFormat printer) ->
                    [ args
                        |> accessor
                        |> printer
                        |> VirtualDom.text
                    ]

                Plural accessor (CustomNumberFormat printer) selector cases ->
                    let
                        nextCount =
                            args |> accessor |> printer
                    in
                    (case nextCount |> selector of
                        Zero ->
                            cases.zero

                        One ->
                            cases.one

                        Two ->
                            cases.two

                        Few ->
                            cases.few

                        Many ->
                            cases.many

                        Other ->
                            cases.other
                    )
                        |> List.map (toNodes (Just nextCount))
                        |> List.concat

                Count ->
                    case maybeCount of
                        Just count ->
                            [ VirtualDom.text count ]

                        Nothing ->
                            Debug.crash "no count given"
    in
    parts
        |> List.map (toNodes Nothing)
        |> List.concat
