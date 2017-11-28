Create localized messages in elm.  __This is a first draft!  Ideas, suggestions
and comments are much appreciated!__  Just open an issue or contact me
(@kirchner) on [Slack](http://elmlang.herokuapp.com/).  The goal is to expose
all the internationalization and localization data available in the [Unicode
Common Locale Data Repository](http://cldr.unicode.org) as a nice API.  This
includes (localized) formatting of numbers, dates, times and lists, as well as
pluralization rules.  In particular, adding placeholders and different plural
forms should be able in a typesafe way.

Here are some examples how one can use the package:

```elm
import Localized exposing (s, print, printWith)
import Localized.En exposing (cardinal)



greeting : String
greeting =
    [ s "Good morning!" ]
        |> print

-- is equal to: "Good morning!"


personalGreeting : String
personalGreeting =
    [ s "Good morning, "
    , string .name
    , s "!"
    ]
        |> printWith { name = "Alice" }

-- is equal to: "Good morning, Alice!"


pluralizedMessage : String
pluralizedMessage =
    [ cardinal .newEmailCount
        { one = [ s "You have one new email." ]
        , other =
            [ s "You have "
            , count
            , s " new emails."
            ]
        }
    ]
        |> printWith { newEmailCount = 42 }

-- is equal to: "You have 42 new emails."
```

You can also generate localized dom nodes:

```elm
import Html exposing (Html)
import Html.Attributes
import Localized exposing (s, node, nodes)



documentationInfo : { link : List (Html msg) -> Html msg } -> List (Html msg)
documentationInfo =
    [ s "Take a look at our "
    , node .link
        [ s "documentation" ]
    , s "."
    ]
        |> nodes


view =
    Html.div [] <|
        documentationInfo
            { link = Html.a [ Html.Attributes.href = "..." ] }

-- is equal to:
--
-- Html.div []
--     [ Html.text "Take a look at our "
--     , Html.a
--         [ Html.Attributes.href = "..." ]
--         [ Html.text "documentation" ]
--     , Html.text "."
--     ]
```

The submodules for each locale (e.g. `Localized.En`, ...) are automatically
generated. To generate them on your own, run

```
$ npm install
$ make
```

You need to have `elm-format` installed.
