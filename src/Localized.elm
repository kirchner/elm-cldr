module Localized
    exposing
        ( (<>)
        , AllPluralCases
        , Case
        , Error(..)
        , Locale
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
        , Translation
        , TranslationSet
        , addCardinalCases
        , addOrdinalCases
        , concat
        , cons
        , count
        , customLocale
        , customNumberFormat
        , customPlural
        , decimal
        , equals
        , fallback
        , final
        , icuMessage
        , name
        , node
        , nodes
        , print
        , printWith
        , s
        , safeTranslateWith
        , select
        , string
        , translate
        , translateWith
        , translationSet
        , when
        )

{-| Create localized texts in a type safe way. You can put all your
translations in a module (say `Translations.En`) which then looks like
this:

    import Localized exposing (Translation, concat, equals, final, s, select, string)
    import Localized.En exposing (cardinal, decimalStandard)

    greeting : Translation args msg
    greeting =
        final "greeting" <|
            s "Good morning!"

    personalGreeting : Translation { args | name : String } msg
    personalGreeting =
        final "personalGreeting" <|
            concat
                [ s "Good morning, "
                , string .name "name"
                , s "!"
                ]

    emailInfo : Translation { args | newEmailCount : Float } msg
    emailInfo =
        final "emailInfo" <|
            cardinal .newEmailCount "newEmailCount" decimalStandard <|
                { one =
                    s "You have one new email."
                , other =
                    [ s "You have "
                    , count
                    , s " new emails."
                    ]
                        |> concat
                }

    type Gender
        = Female
        | Male
        | Other

    partyInfo : Translation { args | gender : Gender } msg
    partyInfo =
        final "partyInfo" <|
            select .gender "gender" (s "They are giving a party.") <|
                [ equals Female "female" <|
                    s "She is giving a party."
                , equals Male "male" <|
                    s "He is giving a party."
                ]

    otherEmailInfo : Translation { args | newEmailCount : Float } msg
    otherEmailInfo =
        final "emailInfo" <|
            cardinal .newEmailCount "newEmailCount" decimalStandard <|
                { one =
                    s "You have one new email."
                , other =
                    select .newEmailCount
                        "newEmailCount"
                        (concat
                            [ s "You have "
                            , count
                            , s " new emails."
                            ]
                        )
                        [ when (\count -> count > 10000) "alot" <|
                            s "Wow, you have a lot of new emails!"
                        ]
                }

And you can use these translations in your view code like so:

    import Localized exposing (print, printWith)
    import Translations.En exposing (..)

    view : String -> Int -> Gender -> Html msg
    view name newEmailCount gender =
        Html.div []
            [ greeting
                |> print
                |> Html.text
            , personalGreeting
                |> printWith { name = name }
                |> Html.text
            , emailInfo
                |> printWith { newEmailCount = toFloat newEmailCount }
                |> Html.text
            , partyInfo
                |> printWith { gender = gender }
                |> Html.text
            , otherEmailInfo
                |> printWith { newEmailCount = toFloat newEmailCount }
                |> Html.text
            ]

If you want to change the translations at runtime without recompiling
your application, you can print them in the following way:

    import Localized exposing (TranslationSet, translate, translateWith, translationSet)
    import Localized.En
    import Translations.En exposing (..)

    view : String -> Int -> Gender -> Html msg
    view name newEmailCount gender =
        Html.div []
            [ greeting
                |> translate translations
                |> Html.text
            , personalGreeting
                |> translateWith translations
                    { name = name }
                |> Html.text
            , emailInfo
                |> translateWith translations
                    { newEmailCount = toFloat newEmailCount }
                |> Html.text
            , partyInfo
                |> translateWith translations
                    { gender = gender }
                |> Html.text
            , otherEmailInfo
                |> translateWith translations
                    { newEmailCount = toFloat newEmailCount }
                |> Html.text
            ]

    translations : TranslationSet
    translations =
        [ ( "greeting", "Guten morgen!" )
        , ( "personalGreeting", "Guten morgen, {name}!" )
        , ( "emailInfo"
          , """
              {newEmailCount, plural,
                one{Du hast eine neue Email.}
                other{Du hast {newEmailCount, number} neue Emails.}
              }
            """
          )
        , ( "partyInfo"
          , """
              {gender, select,
                other{Sie schmeißen eine Party.}
                female{Sie schmeißt eine Party.}
                male{Er schmeißt eine Party.}
              }
            """
          )
        , ( "otherEmailInfo"
          , """
              {newEmailCount, plural,
                one{Du hast eine neue Email.}
                other{{newEmailCount, select,
                  other{Du hast {newEmailCount, number} neue Emails.}
                  alot{Uiuiui, Du hast ganz schön viele neue Emails!}
                }}
              }
            """
          )
        ]
            |> translationSet Localized.En.locale

The `translations` dictionary can of course be created at runtime by
parsing some data.


# Types

@docs Translation, Text, final, fallback


# Printing


## Static

@docs printWith, print, nodes


## Dynamic

@docs translateWith, translate, safeTranslateWith, Error

@docs TranslationSet, translationSet, Locale, customLocale, addCardinalCases, addOrdinalCases


# Creating Texts


## Basic Texts

@docs s, string, node


## Combining Texts

@docs concat, cons, (<>)


## Number Formatting

@docs decimal, NumberFormat, customNumberFormat


## Pluralization

Every locale module like for example `Localized.En` exports the
pluralization texts `cardinal` and `ordinal`. You use these like this

    emailInfo : Translation { args | ord : Float } msg
    emailInfo =
        final "emailInfo" <|
            Localized.En.ordinal .ord "ord" Localized.En.decimalStandard <|
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


## Selects

@docs select, equals, when, Case


# Exporting

@docs icuMessage, name

-}

import Char
import Dict exposing (Dict)
import Internal.Numbers
import Internal.PluralRules
import VirtualDom exposing (Node)


{-| Opaque type which contains a piece of text which is translated into
a language. Eventually, when you want to turn a `Translation args msg`
into a `String` you have to provide some `args`. If you turn it into
a dom node, it may produce `msg`'s.
-}
type Translation args msg
    = Translation String (Text args msg)


{-| Turn some `Text` into a `Translation` by giving it a name. This
translation is `final` and will therefore be exported when you run

    $ elm-intl generate-json

-}
final : String -> Text args msg -> Translation args msg
final name text =
    Translation name text


{-| Like `final` but this translation will not be exported.
-}
fallback : String -> Text args msg -> Translation args msg
fallback name text =
    Debug.crash "TODO"


{-| Opaque building block for texts.
-}
type Text args msg
    = Texts (List (Text args msg))
    | Verbatim String
    | NodeText (args -> (List (Node msg) -> Node msg)) String (Text args msg)
    | String (args -> String) String
    | Decimal (args -> Float) String NumberFormat
    | Plural (args -> Float) String NumberFormat (String -> PluralCase) (AllPluralCases args msg)
    | Count


{-| Create a text that simply returns the given `String`.

    greeting : Translation {} msg
    greeting =
        final "greeting" <|
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

    personalGreeting : Translation { args | name : String } msg
    personalGreeting =
        final "personalGreeting" <|
            concat
                [ s "Hello, "
                , string .name "name"
                , s "!"
                ]

as

    personalGreeting : Translation { args | name : String } msg
    personalGreeting =
        final "personalGreeting"
            (s "Hello, "
                <> string .name "name"
                <> s "!"
            )

-}
(<>) : Text args msg -> Text args msg -> Text args msg
(<>) =
    cons


{-| Create a placeholder which eventually gets replaced by a `String`
which is provided at runtime.

    personalGreeting : Translation { args | name : String } msg
    personalGreeting =
        final "personalGreeting" <|
            concat
                [ s "Hello, "
                , string .name "name"
                , s "!"
                ]

Then `printWith { name = "Alice" } personalGreeting` is equal to
`"Hello, Alice!"`.

-}
string : (args -> String) -> String -> Text args msg
string =
    String


{-| Create a placeholder which gets replaced by a number which is
formatted according to the provided format.

    heightInfo : Translation { args | height : Float } msg
    heightInfo =
        final "heightInfo" <|
            concat
                [ s "Current height: "
                , decimal .height "height" Localized.En.decimalStandard
                , s "."
                ]

Then `printWith { height = 3.1533 } heightInfo` is equal to `"Current
height: 3.153."`.

-}
decimal : (args -> Float) -> String -> NumberFormat -> Text args msg
decimal =
    Decimal


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

    documentationInfo : Translation { args | link : List (Html msg) -> Html msg } msg
    documentationInfo =
        final "documentationInfo" <|
            concat
                [ s "Take a look at our "
                , node .link "link" <|
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
    -> String
    -> Text args msg
    -> Text args msg
node =
    NodeText


{-| Create a custom pluralized text. Use this function if you want
to provide your own pluralization rules.

**Note**: You usually want to use the `ordinal` or `cardinal` functions
from one of the modules like `Localized.En`, depending on the language
the text is in.

-}
customPlural :
    (args -> Float)
    -> String
    -> NumberFormat
    -> (String -> PluralCase)
    -> AllPluralCases args msg
    -> Text args msg
customPlural =
    Plural


{-| Create a text which gets replaced by the formatted number argument
of a pluralized text.

    emailInfo : Translation { args | count : Float } msg
    emailInfo =
        final "emailInfo" <|
            Localized.En.cardinal .count "count" Localized.En.decimalStandard <|
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
provided function. You have to provide a default (`other`) text and
a list of `Case`s.

    partyInfo : Translation { args | gender : Gender } msg
    partyInfo =
        select .gender "gender" (s "They give a party.") <|
            [ equals Female "female" <|
                s "She gives a party."
            , equals Male "male" <|
                s "He gives a party."
            ]

    type Gender
        = Female
        | Male
        | Other

-}
select : (args -> a) -> String -> Text args msg -> List (Case args msg a) -> Text args msg
select accessor name default cases =
    Debug.crash "TODO"


{-| Create a simple `Case` for a `select` text.
-}
equals : a -> String -> Text args msg -> Case args msg a
equals =
    Equals


{-| Create a custom `Case` which gets choosen if the predicate is true.
-}
when : (a -> Bool) -> String -> Text args msg -> Case args msg a
when =
    CustomCase


{-| -}
type Case args msg a
    = Equals a String (Text args msg)
    | CustomCase (a -> Bool) String (Text args msg)



---- EXPORT


{-| Export a `Translation` to the [ICU Message
Format](http://icu-project.org/apiref/icu4j/com/ibm/icu/text/MessageFormat.html).
For example, for the following

    personalGreeting : Translation { args | name : String } msg
    personalGreeting =
        final "personalGreeting" <|
            concat
                [ s "Hello, "
                , string .name "name"
                , s "!"
                ]

we have that `icuMessage personalGreeting` is equal to `"Hello,
{name}!"`.

-}
icuMessage : Translation args msg -> String
icuMessage _ =
    Debug.crash "TODO"


{-| Return the name of a translation. So `name (final "greeting" (s
"Hello!"))` is equal to `"greeting"`.
-}
name : Translation args msg -> String
name (Translation name _) =
    name



---- PRINT


{-| Use this function to turn a `Translation` into a `String`.

    greeting : String
    greeting =
        printWith { name = "Alice" } <|
            final "greeting" <|
                concat <|
                    [ s "Good morning, "
                    , string .name "name"
                    , s "!"
                    ]

Then `greeting` is equal to `"Good morning, Alice!"`.

-}
printWith : args -> Translation args msg -> String
printWith args (Translation _ text) =
    printWithCount Nothing args text


printWithCount : Maybe String -> args -> Text args msg -> String
printWithCount maybeCount args text =
    case text of
        Texts nextTexts ->
            nextTexts
                |> List.map (printWithCount maybeCount args)
                |> String.concat

        Verbatim text ->
            text

        NodeText accessor _ nextTexts ->
            nextTexts
                |> printWithCount maybeCount args

        String accessor _ ->
            accessor args

        Decimal accessor _ (CustomNumberFormat printer) ->
            args |> accessor |> printer

        Plural accessor _ (CustomNumberFormat printer) selector cases ->
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


{-| Use this function if your `Translation` does not need arguments.
This is basically `print translation = printWith {} translation`.
-}
print : Translation {} msg -> String
print translation =
    printWith {} translation


{-| Opaque type holding a set of translations, which can be generated at
runtime. If you use `translateWith` or `translate` for printing your
`Translation`'s you will need to provide a `TranslationSet`.
-}
type TranslationSet
    = TranslationSet Locale (Dict String String)


{-| Create a `TranslationSet` by providing a `Locale` and a list of
translations. The first `String` is the name of the `Translation`, the
second is the content which should be in the [ICU Message
Format](http://icu-project.org/apiref/icu4j/com/ibm/icu/text/MessageFormat.html)
-}
translationSet : Locale -> List ( String, String ) -> TranslationSet
translationSet locale translations =
    translations
        |> Dict.fromList
        |> TranslationSet locale


{-| Opaque type holding information about a locale, like for example
which cardinal/ordinal plural forms are needed. Every `Localized.En`,
... module exports a constructor for `Locale`. If you want to create
your custom locales, take a look at `customLocale`.
-}
type Locale
    = CustomLocale LocaleData


type alias LocaleData =
    { code : String
    , cardinalCases : List PluralCase
    , ordinalCases : List PluralCase
    }


{-| Create a custom `Locale` with minimal properties. Use functions
like `addCardinalCases` or `addOrdinalCases` to add further properties.
For example, the locale `Localized.En.locale` could have also be created
by

    en : Locale
    en =
        customLocale "en"
            |> addCardinalCases [ One ]
            |> addOrdinalCases [ One, Two, Few ]

-}
customLocale : String -> Locale
customLocale code =
    CustomLocale
        { code = code
        , cardinalCases = [ Other ]
        , ordinalCases = [ Other ]
        }


{-| Add cardinal plural forms which the locale should require.
-}
addCardinalCases : List PluralCase -> Locale -> Locale
addCardinalCases pluralCases (CustomLocale data) =
    { data
        | cardinalCases =
            pluralCases ++ data.cardinalCases
    }
        |> CustomLocale


{-| Add ordinal plural forms which the locale should require.
-}
addOrdinalCases : List PluralCase -> Locale -> Locale
addOrdinalCases pluralCases (CustomLocale data) =
    { data
        | ordinalCases =
            pluralCases ++ data.ordinalCases
    }
        |> CustomLocale


{-| Use this function if you want to replace a `Translation` at runtime
with new content. It will use the name of the `Translation` to fetch the
new content from the `TranslationSet`. This content should be in the
[ICU Message
Format](http://icu-project.org/apiref/icu4j/com/ibm/icu/text/MessageFormat.html).
When the content cannot be parsed or does not have the correct
structure, the original `Translation` is printed as if `printWith` was
used. Use `safeTranslateWith` if you want to get an error instead in
such a situation.

    greeting : String
    greeting =
        final "greeting" <|
            concat <|
                [ s "Good morning, "
                , string .name "name"
                , s "!"
                ]

    output : String -> String
    output name =
        greeting
            |> translateWith translations
                { name = name }

    translations : TranslationSet
    translations =
        [ ( "greeting", "Guten morgen, {name}!" )
        ]
            |> translationSet Localized.En.locale

Then `greeting "Alice"` is equal to `"Guten morgen, Alice!"`.

-}
translateWith : TranslationSet -> args -> Translation args msg -> String
translateWith translations args translation =
    Debug.crash "TODO"


{-| This works like `translateWith` but gives back an `Error` if the
content is not a valid ICU message or does not fit the structure of the
`Translation`.
-}
safeTranslateWith : TranslationSet -> args -> Translation args msg -> Result (List Error) String
safeTranslateWith translations args translation =
    Debug.crash "TODO"


{-| -}
type Error
    = InvalidICUMessage String
    | MissingArgument String
    | InvalidDecimalFormat
        { argumentName : String
        , format : String
        }
    | InvalidPluralCases
        { argumentName : String
        , missing : List PluralCase
        , unnecessary : List PluralCase
        }
    | InvalidSelectCases
        { argumentName : String
        , missing : List String
        , unnecessary : List String
        }


{-| Convenience function if the `Translation` does not need arguments.
-}
translate : TranslationSet -> Translation {} msg -> String
translate translations translation =
    Debug.crash "TODO"


{-| Use this function to turn a `Translation` into a list of dom nodes.
You want to do this if one of the `Text`s is a `node`.

    documentationInfo : Translation { args | link : List (Html msg) -> Html msg } msg
    documentationInfo =
        final "documentationInfo" <|
            concat
                [ s "Take a look at our "
                , node .link "link" <|
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
nodes : args -> Translation args msg -> List (Node msg)
nodes args (Translation _ text) =
    nodesWithCount Nothing args text


nodesWithCount : Maybe String -> args -> Text args msg -> List (Node msg)
nodesWithCount maybeCount args text =
    case text of
        Texts nextTexts ->
            nextTexts
                |> List.map (nodesWithCount maybeCount args)
                |> List.concat

        Verbatim text ->
            [ VirtualDom.text text ]

        NodeText accessor _ nextText ->
            [ nextText
                |> nodesWithCount maybeCount args
                |> accessor args
            ]

        String accessor _ ->
            [ args
                |> accessor
                |> VirtualDom.text
            ]

        Decimal accessor _ (CustomNumberFormat printer) ->
            [ args
                |> accessor
                |> printer
                |> VirtualDom.text
            ]

        Plural accessor _ (CustomNumberFormat printer) selector cases ->
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
