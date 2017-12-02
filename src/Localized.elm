module Localized
    exposing
        ( AllPluralCases
        , NumberFormat
        , Text
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

@docs Text


# Printing

@docs printWith, print, nodes


# Creating Texts


## Basic Texts

@docs s, string, node


## Number Formatting

@docs decimal, NumberFormat, customNumberFormat


## Pluralization and Selects

Every locale module like for example `Localized.En` exports the
pluralization texts `cardinal` and `ordinal`. You use these like this

    emailInfo : List (Text { ord : Float } msg)
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
type Text args msg
    = Verbatim String
    | NodeText (args -> (List (Node msg) -> Node msg)) (List (Text args msg))
    | String (args -> String)
    | Decimal (args -> Float) NumberFormat
    | Plural (args -> Float) NumberFormat (String -> PluralCase) (AllPluralCases args msg)
    | Count


{-| Create a text returns the given `String`.

    greeting : List (Text {} msg)
    greeting =
        [ s "Hello!" ]

Then `"print greeting"` is equal to `"Hello!"`.

-}
s : String -> Text args msg
s text =
    Verbatim text


{-| Create a placeholder which eventually gets replaced by a `String`.

    personalGreeting : List (Text { name : String } msg)
    personalGreeting =
        [ s "Hello, "
        , string .name
        , s "!"
        ]

Then `printWith { name = "Alice" } personalGreeting` is equal to
`"Hello, Alice!"`.

-}
string : (args -> String) -> Text args msg
string accessor =
    String accessor


{-| Create a placeholder which gets replaced by a number which is
formatted according to the provided format.

    heightInfo : List (Text { height : Float } msg)
    heightInfo =
        [ s "Current height: "
        , decimal .height Localized.En.decimalStandard
        , s "."
        ]

Then `printWith { height = 3.1533 } heightInfo` is equal to `"Current
height: 3.153."`.

-}
decimal : (args -> Float) -> NumberFormat -> Text args msg
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


{-| Create a text which eventually gets replaced by a dom node.

    documentationInfo : List (Text { link : List (Html msg) -> Html msg } msg)
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
    -> List (Text args msg)
    -> Text args msg
node accessor nextTexts =
    NodeText accessor nextTexts


{-| Create a custom pluralized text. Use this function if you want
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
    -> Text args msg
customPlural accessor numberFormat selector cases =
    Plural accessor numberFormat selector cases


{-| Create a text which gets replaced by the formatted number argument
of a pluralized text.

    emailInfo : List (Text { count : Float } msg)
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
count : Text args msg
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
    { zero : List (Text args msg)
    , one : List (Text args msg)
    , two : List (Text args msg)
    , few : List (Text args msg)
    , many : List (Text args msg)
    , other : List (Text args msg)
    }


{-| Create a text which selects between different versions using the
provided function.

    partyInfo : List (Text { gender : Gender } msg)
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
select : (args -> a) -> (a -> List (Text args msg)) -> Text args msg
select accessor selector =
    Debug.crash "TODO"



---- PRINT


{-| Use this function to turn a list of texts into a `String`.

    greeting : String
    greeting =
        printWith { name = "Alice" }
            [ s "Good morning, "
            , string .name
            , s "!"
            ]

Then `greeting` is equal to `"Good morning, Alice!"`.

-}
printWith : args -> List (Text args msg) -> String
printWith args texts =
    let
        printText maybeCount text =
            case text of
                Verbatim text ->
                    text

                NodeText accessor nextTexts ->
                    nextTexts
                        |> List.map (printText maybeCount)
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
                        List.map (printText (Just nextCount)) <|
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
    texts
        |> List.map (printText Nothing)
        |> String.concat


{-| Use this function if your list of texts does not need arguments.
This is basically `print texts = printWith {} texts`.
-}
print : List (Text {} msg) -> String
print texts =
    printWith {} texts


{-| Use this function to turn a list of texts into a list of dom nodes.
You want to do this if one of the texts is a `node`.

    documentationInfo : List (Text { link : List (Html msg) -> Html msg } msg)
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
nodes : args -> List (Text args msg) -> List (Node msg)
nodes args texts =
    let
        toNodes : Maybe String -> Text args msg -> List (Node msg)
        toNodes maybeCount text =
            case text of
                Verbatim text ->
                    [ VirtualDom.text text ]

                NodeText accessor nextTexts ->
                    [ nextTexts
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
    texts
        |> List.map (toNodes Nothing)
        |> List.concat
