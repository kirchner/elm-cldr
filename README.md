Create localized texts in elm.  __This is a first draft!  Ideas, suggestions
and comments are much appreciated!__  Just open an issue or contact me
(@kirchner) on [Slack](http://elmlang.herokuapp.com/).

# Goal

The goal of this package is to expose all the internationalization and
localization data available in the [Unicode Common Locale Data
Repository](http://cldr.unicode.org) as a nice API.  This includes (localized)
formatting of numbers, dates, times and lists, as well as pluralization rules.
In particular, adding placeholders and different plural forms should be able in
a typesafe way.

Another goal is to then generate these localized messages from "standard"
internationalization formats (like for example the [ICU Message
Format](http://userguide.icu-project.org/formatparse/messages))


## Concrete Use Cases

- Having a solutiont to provide texts in several languages
- Localized texts should be able to contain named placeholders, which are
  filled at runtime
- One has to be able to provide pluralized forms of localized texts. (`"1
  second"`, `"2 seconds"`) Which forms are necessary and how they are selected,
  depends on the locale.
- One has to be able to provide different versions depending on some user
  defined data. (for example gender: `"She gives a party"`, `"He gives
  a party"`, `"They give a party"`)
- Numbers, Dates and Times must be formatted either according to the locale or
  as specified by the program
- If you have a text like `"Take a look at our documentation"`, there should be
  a way to specify that in the eventually generated Html `"documentation"` will
  be a link.


# Example Usage

The `Localized` module in this repo contains a first API draft.  If you want to
read its documentation, just put the
[documentation.json](https://rawgit.com/kirchner/elm-cldr/master/documentation.json)
into the [doc preview](http://package.elm-lang.org/help/docs-preview).

Here are some examples how one can use this module:

```elm
import Localized exposing (s, concat, print, printWith)
import Localized.En exposing (cardinal, decimalStandard)



greeting : String
greeting =
    s "Good morning!"
        |> print

-- is equal to: "Good morning!"


personalGreeting : String
personalGreeting =
    [ s "Good morning, "
    , string .name
    , s "!"
    ]
        |> concat
        |> printWith { name = "Alice" }

-- is equal to: "Good morning, Alice!"


pluralizedMessage : String
pluralizedMessage =
    cardinal .newEmailCount
        decimalStandard
        { one =
            s "You have one new email."
        , other =
            concat
                [ s "You have "
                , count
                , s " new emails."
                ]
        }
        |> printWith { newEmailCount = 42 }

-- is equal to: "You have 42 new emails."
```

You can also generate localized dom nodes:

```elm
import Html exposing (Html)
import Html.Attributes
import Localized exposing (s, concat, node, nodes)



documentationInfo : List (Text { link : List (Html msg) -> Html msg } msg)
documentationInfo =
    concat
        [ s "Take a look at our "
        , node .link <|
            s "documentation"
        , s "."
        ]


view url =
    Html.div [] <|
        nodes { link = Html.a [ Html.Attributes.href = url ] }
            documentationInfo

-- is equivalent to:

actualView url =
    Html.div []
        [ Html.text "Take a look at our "
        , Html.a
            [ Html.Attributes.href = url ]
            [ Html.text "documentation" ]
        , Html.text "."
        ]
```

So basically, the idea is that you put all your texts in some module, for
example

```elm
module Translations.En exposing (..)

import Html exposing (Html)
import Localized exposing (Text, s, concat, string, node)
import Localized.En exposing (cardinal, decimalStandard)


greeting : Text args msg
greeting =
    s "Good morning!"


personalGreeting : Text { args | name : String } msg
personalGreeting =
    concat
        [ s "Good morning, "
        , string .name
        , s "!"
        ]


pluralizedMessage : Text { args | newEmailCount : Float } msg
pluralizedMessage =
    cardinal .newEmailCount
        decimalStandard
        { one =
            s "You have one new email."
        , other =
            [ s "You have "
            , count
            , s " new emails."
            ]
                |> concat
        }


documentationInfo : Text { args | link : List (Html msg) -> Html msg } msg
documentationInfo =
    concat
        [ s "Take a look at our "
        , node .link <|
            s "documentation"
        , s "."
        ]
```

And then use these functions in your (view) code.  This way the compiler warns
you if you are not providing values for all placeholders, or when your
pluralized texts do not provide a version for all cases required by the
selected locale.

In order to get your texts for different locales, you can create another module
for each locale.  For example

```elm
module Translations.De exposing (..)

import Html exposing (Html)
import Localized exposing (Text, s, concat, string, node)
import Localized.De exposing (cardinal, decimalStandard)


greeting : Text args msg
greeting =
    s "Guten Morgen!"

...
```

And a module which collects all these translations

```elm
module Translations exposing (..)

import Translations.En as En
import Translations.De as De


type Locale
    = En
    | De


greeting : Locale -> Text args msg
greeting =
    case Locale of
        En ->
            En.greeting

        De ->
            De.greeting

...
```

In your application, you then only import the `Translations` module and provide
each text with the currently selected `Locale`.  Again, this way the compiler
makes sure that you have translations for all your texts and that every
translation of a text uses the same placeholders.

It should not be difficult to generate these translation modules
`Translations`, `Translations.En`, ... from "standard" localization files (like
ICU messages).  Also it should be simple to generate ordinary localization
files from translation modules.


# Remarks

I am not sure what is the best way to expose the localization data provided by
the CLDR. I think, one can do one of these things:

- Automatically generate Elm modules by parsing the CLDR data, which expose the
  different formatting rules and the `plural` and `ordinal` text functions
  (this is what I did here for the pluralization rules)

- Let the user parse the CLDR data themself during runtime

- Use the functions which are exposed by the [Intl Web
  Api](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl)

The first two approaches require a great amount of work and could perhaps
result in rather big asset sizes.  The third approach has the benefit that one
does not have to deal with turning the CLDR data into e.g. localized number
formatting functions as these are then already provided by the browser.

One disadvantage of the third way is that for example, the proposed
[PluralRules
API](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/PluralRules)
does not capture the full possibilities of the CLDR.  When picking the correct
plural form, the CLDR specification takes into account the actual string
representation of the number.  For example in English, one has `"There is
1 second left."` but `"There are 1.0 seconds left."`  The proposed PluralRules
api only provides a function
[select](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/PluralRules/select)
which takes a **number** and returns the plural form, so the distinction
between `"1"` and `"1.0"` is not possible.


# ToDo

- Texts:

    - [x] basic (`s`, `string`, `node`) 
    - [ ] pluralizations (cardinal and ordinal)
    - [ ] select
    - [ ] decimal number formatting
    - [ ] percentage formatting
    - [ ] currency formatting
    - [ ] date and time formatting
    - [ ] lists

- Code generation:

    - [ ] generate translation modules from icu message format
    - [ ] generate icu messages from translation modules


# Other Elm I18n Packages

Please tell me if I am missing some.

- [ChristophP/elm-i18next](https://github.com/ChristophP/elm-i18next)
- [ChristophP/elm-i18n-module-generator](https://github.com/ChristophP/elm-i18n-module-generator)
- [dragonwasrobot/i18n-to-elm](https://github.com/dragonwasrobot/i18n-to-elm)
- [fossar/elm-localization](https://github.com/fossar/elm-localization)
- [GlenDC/elm-l20n](https://github.com/GlenDC/elm-l20n)
- [iosphere/elm-i18n](https://github.com/iosphere/elm-i18n)
- [lukewestby/elm-string-interpolate](https://github.com/lukewestby/elm-string-interpolate)
- [michaeljones/elm-message-format-experiment](https://github.com/michaeljones/elm-message-format-experiment)
- [thetalecrafter/elm-intl](https://github.com/thetalecrafter/elm-intl)


# Misc

The submodules for each locale (e.g. `Localized.En`, ...) are automatically
generated. To generate them on your own, run

```
$ npm install
$ make
```

You need to have `elm-format` installed.
