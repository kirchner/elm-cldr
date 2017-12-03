Create localized texts in elm.  __This is a first draft!  Ideas, suggestions
and comments are much appreciated!__  Just open an issue or contact me
(@kirchner) on [Slack](http://elmlang.herokuapp.com/).

# Goal

The goal of this package is to expose all the internationalization and
localization data available in the [Unicode Common Locale Data
Repository](http://cldr.unicode.org) as a nice API.  This includes (localized)
formatting of numbers, dates, times and lists, as well as pluralization rules.
In particular, adding placeholders and different plural forms should be
possible in a typesafe way.

Another goal is to then generate these localized texts from "standard"
internationalization formats (like for example the [ICU Message
Format](http://userguide.icu-project.org/formatparse/messages))


## Concrete Use Cases

- Provide texts in several languages
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


## Converting Between Elm Translations Modules and Ordinary Translation Files

It should not be difficult to generate these translation modules
`Translations`, `Translations.En`, ... from "standard" localization files (like
ICU messages).  Also it should be simple to generate ordinary localization
files from translation modules.

Using a tool like [iosphere/elm-i18n](https://github.com/iosphere/elm-i18n) is
a good way I think.  Such a tool (lets call it `elm-intl` for now) should
basically support two operations.

For example, if you run

```bash
$ elm-intl generate-json
```

It will convert all `Translations.En`, `Translations.De`, ... modules to the
json files `translations/en.json`, `translations/de.json`, ... .  Each of them
will contain an object, where the keys correspond to the exposed translation
functions (`greeting`, `personalGreeting`, ... in the example above), and whose
values are string representations of their values.  These strings could be in
some (extended) ICU message format (if one wants to support numbers and nodes
for example).

To go the other way, you run

```bash
$ elm-intl generate-elm
```

which turns every `translations/<locale>.json` file into the corresponding
`Translations.<locale>` module and also generates the `Translations` and
a `Translations.Current` module.


## Possible Localization Workflows

### Application First

This is what a localization workflow could look like when you first write your
application (in some language) and later want to translate it.  For example,
you could start with view code, which looks something like this:

```elm
module Main exposing (..)


view =
    Html.div []
        [ Html.h1 []
            [ Html.text "Welcome to our website!" ]
        , Html.p []
            [ Html.text "This is some random text. You can read more on our "
            , Html.a [ Html.Attributes.href "..." ]
                [ Html.text "about page" ]
            , Html.text "."
            ]
        ]
```

Since you want to localize these texts,  you create a translation module for
English containing all these texts:

```elm
module Translations.En exposing (..)

import Localized exposing (s, concat, node)


heading =
    s "Welcome to our website!"


information =
    [ s "This is some random text. You can read more on our "
    , node .aboutPageLink <|
        s "about page"
    , s "."
    ]
        |> concat
```

And use these functions in your view code instead:

```elm
module Main exposing (..)

import Translations.En exposing (..)
import Localized exposing (nodes, print)


view =
    Html.div []
        [ Html.h1 []
            [ heading
                |> print
                |> Html.text
            ]
        , information
            |> nodes { aboutPageLink = Html.a [ Html.Attributes.href "..." ] }
            |> Html.p []
        ]
```

Now, you want to create a localization file from your translations module by
running

```bash
$ elm-intl generate-json
```

which creates a file `translations/en.json` with the following content:

```json
{
  "heading": "Welcome to our website!",
  "information": "This is some random text. You can read more on our {aboutPageLink, node, {about page}}."
}
```

You give this to your translators asking them to give you a German translation of
this data.  So you get back the file `translations/de.json` with the content:

```json
{
  "heading": "Willkommen auf unserer Webseite!",
  "information": "Das ist etwas zufälliger Text. Du kannst auf unserer {aboutPageLink, node, {About Page}} mehr lesen."
}
```

To use these translations in your application you run

```bash
$ elm-intl generate-elm
```

Which will (in addition to the already existing module `Translations.En`)
generate a module `Translations.De` and a module `Translations` with the
following contents:

```elm
module Translations.De exposing (..)

import Localized exposing (s, concat, node)


heading =
    s "Willkommen auf unserer Webseite!"


information =
    [ s "Das ist etwas zufälliger Text. Du kannst auf unserer "
    , node .aboutPageLink <|
        s "About Page"
    , s " mehr lesen."
    ]
        |> concat
```

```elm
module Translations exposing (..)

import Translations.En as En
import Translations.De as De


type Locale
    = En
    | De


heading locale =
    case locale of
        En ->
            En.heading

        De ->
            De.heading

...
```

It will also generate a module `Translations.Current` with the content of
`Translations.En`.

So now you can decide if you want to make the language choice dynamic, by
changing your view code to

```elm
module Main exposing (..)

import Translations exposing (..)
import Localized exposing (nodes, print)


view locale =
    Html.div []
        [ Html.h1 []
            [ heading locale
                |> print
                |> Html.text
            ]
        , information locale
            |> nodes { aboutPageLink = Html.a [ Html.Attributes.href "..." ] }
            |> Html.p []
        ]
```

So you import `Translations` instead of `Translations.En` and provide each
translation function the currently selected `locale`.

Or if you want to generate assets for each locale, you change the view code to

```elm
module Main exposing (..)

import Translations.Current exposing (..)
import Localized exposing (nodes, print)


view =
    Html.div []
        [ Html.h1 []
            [ heading
                |> print
                |> Html.text
            ]
        , information
            |> nodes { aboutPageLink = Html.a [ Html.Attributes.href "..." ] }
            |> Html.p []
        ]
```

This is just replacing `Translations.En` with `Translations.Current`. So, when
you compile you get the asset with the currently selected locale.  If you want
to change it, you run

```bash
$ elm-intl switch-locale de
```

which copies the content from `Translations.De` to `Translations.Current`, and
compile your Elm application again.


### Translations First

Another use case could be, that you get some translations file and want to
start using its content within your application.  So you have a file
`translations/en.json` with the following content

```json
{
  "about": "This product is <b>so amazing</b>! Read what <a href=...>other people</a> think about it."
}
```

You run

```bash
$ elm-intl generate-elm
```

which produces the `Translations` module and the module `Translations.En` with the content

```elm
module Translations.En exposing (..)

import Localized exposing (s)


about =
    s "This product is <b>so amazing</b>! Read what <a href=...>other people</a> think about it."
```

Which is a bit unsatisfying since you don't want to have Html tags within your
`String`'s.  So you edit the Translations.En module to look like this:

```elm
module Translations.En exposing (..)

import Localized exposing (s, concat, node)


about =
    [ s "This product is "
    , node .emph <|
        s "so amazing"
    , "! Read what "
    , node .link <|
        s "other people"
    , s "think about it."
    ]
        |> concat
```

You can use this module (or the dynamic `Translations` module) in your view
code as before.  If you run `elm-intl generate-json`, you will overwrite the
original translations file with the ICU version, so your translators will
(hopefully) not use the Html tags when they eventually provide you with the
translations for other languages.


## Internationalization/Localization of Elm Packages

I am not sure what is the best way to do i18n of elm packages.  I can think of
two approaches.

### Hiding I18n

Say we have written a package which parses CSV files:

```elm
module CSV exposing (CSV, parse)


{-| Parses the provided content as CSV.
-}
parse : String -> Result String CSV


{-| The parsed content of a CSV file.
-}
type CSV
    = ...
```

So `parse` returns a `Result String CSV` where the `String` will be some error
message, telling the user what went wrong while trying to parse the input.  It
totally makes sense, to have these error message available in different
languages!

I guess, one good step would be to do sth like what is done in
[elm-tools/parser](https://github.com/elm-tools/parser), namely providing an
error type which models all possible (combination of) errors.  For the CSV
parser this could be sth like this:

```elm
type Error
    = UnmatchedQuotation
    | UnescapedSymbol String
    | ...
```

From this one can either say that the user of the library has to turn these
`Error`'s into readable content, or one provides some module exposing
a function like `printError : Locale -> Error -> String`.  Internally, one can
of course use some i18n package like this to manage the different translations.
But they are somewhat hidden from the user of the package.


### Provide I18n "Slots"

I suppose, it is rather difficult for package authors to provide translations
for **all** languages.  So it might be a good idea, to give the user the
possibility to drop in their own translations.

Looking at the CSV example above, this package could also expose the following
type

```elm
type alias ErrorTexts =
    { errorIntroduction : Text { count : Float } Never
    , unmatchedQuotation : Text {} Never
    , unescapedSymbol : Text { symbol : String } Never
    , listFormat : ListFormat
    , ...
    }
```

and the `printError` function could be changed to

```elm
printError : ErrorTexts -> Error -> String
```

which prints the error using the provided translations.  Here `ListFormat` is
supposed to be the type holding the list formatting rules of a language.  For
example, one could now do

```elm
import CSV
import Localized exposing (s, concat, count)
import Localized.En exposing (cardinal, decimalStandard, listFormat)


view : String -> Html msg
view csvFile =
    case csvFile |> CSV.parse of
        Ok csv ->
            viewCsv csv

        Err error ->
            Html.div []
                [ error
                    |> CSV.printError
                        { errorIntroduction =
                            cardinal .count
                                decimalStandard
                                { one =
                                    s "There was one error while parsing your CSV file."
                                , other =
                                    [ s "There were "
                                    , count
                                    , s " errors while parsing your CSV file."
                                    ]
                                        |> concat
                                }
                        , unmatchedQuotation =
                            s "There is an unmatched quotation."
                        , unescapedSymbol =
                            [ s "There is the unescaped symbol '"
                            , string .symbol
                            , s "'."
                            ]
                                |> concat
                        , listFormat = listFormat
                        , ...
                        }
                    |> Html.text
                ]
```

And the output in an error situtation would then be `"There were 2 errors while
parsing your CSV file. There is an unmatched quotation and there is the
unescaped symbol ','."`.


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
API only provides a function
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
