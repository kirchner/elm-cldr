module Localized
    exposing
        ( (<>)
        , AllPluralCases
        , NumberFormat
        , PluralCase
            ( Few
            , Many
            , One
            , Other
            , Two
            , Zero
            )
        , Text
        , concat
        , cons
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


## Combining Texts

@docs concat, cons, (<>)


## Number Formatting

@docs decimal, NumberFormat, customNumberFormat


## Pluralization and Selects

Every locale module like for example `Localized.En` exports the
pluralization texts `cardinal` and `ordinal`. You use these like this

    emailInfo : Text { args | ord : Float } msg
    emailInfo =
        Localized.En.ordinal .ord
            Localized.En.decimalStandard
            { one =
                [ s "This is the "
                , count
                , "st message."
                ]
                    |> concat
            , two =
                [ s "This is the "
                , count
                , "nd message."
                ]
                    |> concat
            , few =
                [ s "This is the "
                , count
                , "rd message."
                ]
                    |> concat
            , other =
                [ s "This is the "
                , count
                , "th message."
                ]
                    |> concat
            }

@docs count, customPlural, PluralCase, AllPluralCases

@docs select

-}

import Char
import Internal.Numbers
import Internal.PluralRules
import VirtualDom exposing (Node)


{-| Opaque building block for texts.  Eventually, when you want to turn
a `Text args msg` into a `String` you have to provide some `args`.  If
you turn it into a dom node, it may produce `msg`'s.
-}
type Text args msg
    = Texts (List (Text args msg))
    | Verbatim String
    | NodeText (args -> (List (Node msg) -> Node msg)) (Text args msg)
    | String (args -> String)
    | Decimal (args -> Float) NumberFormat
    | Plural (args -> Float) NumberFormat (String -> PluralCase) (AllPluralCases args msg)
    | Count


{-| Create a text that simply returns the given `String`.

    greeting : Text {} msg
    greeting =
        s "Hello!"

Then `print greeting` is equal to `"Hello!"`.

-}
s : String -> Text args msg
s text =
    Verbatim text


{-| Concatenate a list of texts together.
-}
concat : List (Text args msg) -> Text args msg
concat texts =
    Texts texts


{-| Concatenate two texts.
-}
cons : Text args msg -> Text args msg -> Text args msg
cons textA textB =
    Texts [ textA, textB ]


{-| Infix operator version of `cons`. This lets you write

    personalGreeting : Text { args | name : String } msg
    personalGreeting =
        concat
            [ s "Hello, "
            , string .name
            , s "!"
            ]

as

    personalGreeting : Text { args | name : String } msg
    personalGreeting =
        s "Hello, "
            <> string .name
            <> s "!"

-}
(<>) : Text args msg -> Text args msg -> Text args msg
(<>) =
    cons


{-| Create a placeholder which eventually gets replaced by a `String`
which is provided at runtime.

    personalGreeting : Text { args | name : String } msg
    personalGreeting =
        concat
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

    heightInfo : Text { args | height : Float } msg
    heightInfo =
        concat
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
module provides functions to create the formats which are specified in
the CLDR.
-}
type NumberFormat
    = CustomNumberFormat (Float -> String)


{-| Create your own number format.
-}
customNumberFormat : (Float -> String) -> NumberFormat
customNumberFormat printer =
    CustomNumberFormat printer


{-| Create a text which eventually gets replaced by a dom node.

    documentationInfo : Text { args | link : List (Html msg) -> Html msg } msg
    documentationInfo =
        concat
            [ s "Take a look at our "
            , node .link <|
                s "documentation"
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
node :
    (args -> (List (Node msg) -> Node msg))
    -> Text args msg
    -> Text args msg
node accessor nextTexts =
    NodeText accessor nextTexts


{-| Create a custom pluralized text. Use this function if you want
to provide your own pluralization rules.

**Note**: You usually want to use the `ordinal` or `cardinal` functions
from one of the modules like `Localized.En`, depending on the language
the text is in.

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

    emailInfo : Text { args | count : Float } msg
    emailInfo =
        Localized.En.cardinal .count
            Localized.En.decimalStandard
            { one =
                s "You have one new email"
            , other =
                [ s "You have "
                , count
                , s " new emails"
                ]
                    |> concat
            }

-}
count : Text args msg
count =
    Count


{-| This type represents the different [plural
cases](http://www.unicode.org/reports/tr35/tr35-numbers.html#Language_Plural_Rules)
which are used in the CLDR.
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
    { zero : Text args msg
    , one : Text args msg
    , two : Text args msg
    , few : Text args msg
    , many : Text args msg
    , other : Text args msg
    }


{-| Create a text which selects between different versions using the
provided function.

    partyInfo : Text { args | gender : Gender } msg
    partyInfo =
        select .gender <|
            \gender ->
                case gender of
                    Female ->
                        s "She gives a party."

                    Male ->
                        s "He gives a party."

                    Other ->
                        s "They give a party."

    type Gender
        = Female
        | Male
        | Other

-}
select : (args -> a) -> (a -> Text args msg) -> Text args msg
select accessor selector =
    Debug.crash "TODO"



---- PRINT


{-| Use this function to turn a text into a `String`.

    greeting : String
    greeting =
        [ s "Good morning, "
        , string .name
        , s "!"
        ]
            |> concat
            |> printWith { name = "Alice" }

Then `greeting` is equal to `"Good morning, Alice!"`.

-}
printWith : args -> Text args msg -> String
printWith =
    printWithCount Nothing


printWithCount : Maybe String -> args -> Text args msg -> String
printWithCount maybeCount args text =
    case text of
        Texts nextTexts ->
            nextTexts
                |> List.map (printWithCount maybeCount args)
                |> String.concat

        Verbatim text ->
            text

        NodeText accessor nextTexts ->
            nextTexts
                |> printWithCount maybeCount args

        String accessor ->
            accessor args

        Decimal accessor (CustomNumberFormat printer) ->
            args |> accessor |> printer

        Plural accessor (CustomNumberFormat printer) selector cases ->
            let
                nextCount =
                    args |> accessor |> printer
            in
            printWithCount (Just nextCount) args <|
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


{-| Use this function if your text does not need arguments.
This is basically `print text = printWith {} text`.
-}
print : Text {} msg -> String
print texts =
    printWith {} texts


{-| Use this function to turn a list of texts into a list of dom nodes.
You want to do this if one of the texts is a `node`.

    documentationInfo : Text { args | link : List (Html msg) -> Html msg } msg
    documentationInfo =
        concat
            [ s "Take a look at our "
            , node .link <|
                s "documentation"
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
nodes : args -> Text args msg -> List (Node msg)
nodes =
    nodesWithCount Nothing


nodesWithCount : Maybe String -> args -> Text args msg -> List (Node msg)
nodesWithCount maybeCount args text =
    case text of
        Texts nextTexts ->
            nextTexts
                |> List.map (nodesWithCount maybeCount args)
                |> List.concat

        Verbatim text ->
            [ VirtualDom.text text ]

        NodeText accessor nextText ->
            [ nextText
                |> nodesWithCount maybeCount args
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
                |> nodesWithCount (Just nextCount) args

        Count ->
            case maybeCount of
                Just count ->
                    [ VirtualDom.text count ]

                Nothing ->
                    Debug.crash "no count given"
