module LocalizedTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Localized exposing (..)
import Localized.En as En
import Test exposing (..)


suite : Test
suite =
    describe "print"
        [ test "a simple message" <|
            \_ ->
                simpleMessage
                    |> print {}
                    |> Expect.equal "Hello!"
        , test "a message with string argument" <|
            \_ ->
                stringMessage
                    |> print { name = "Alice" }
                    |> Expect.equal "Good morning, Alice!"
        , describe "en locale"
            [ describe "a cardinal message"
                [ test "count = 1" <|
                    \_ ->
                        enCardinalMsg
                            |> print { count = 1 }
                            |> Expect.equal "There is one new message."
                , test "count = 10" <|
                    \_ ->
                        enCardinalMsg
                            |> print { count = 10 }
                            |> Expect.equal "There are 10 new messages."
                ]
            , describe "an ordinal message"
                [ test "count = 1" <|
                    \_ ->
                        enOrdinalMsg
                            |> print { count = 1 }
                            |> Expect.equal "This is the 1st message."
                , test "count = 2" <|
                    \_ ->
                        enOrdinalMsg
                            |> print { count = 2 }
                            |> Expect.equal "This is the 2nd message."
                , test "count = 3" <|
                    \_ ->
                        enOrdinalMsg
                            |> print { count = 3 }
                            |> Expect.equal "This is the 3rd message."
                , test "count = 7" <|
                    \_ ->
                        enOrdinalMsg
                            |> print { count = 7 }
                            |> Expect.equal "This is the 7th message."
                , test "count = 11" <|
                    \_ ->
                        enOrdinalMsg
                            |> print { count = 11 }
                            |> Expect.equal "This is the 11th message."
                , test "count = 42" <|
                    \_ ->
                        enOrdinalMsg
                            |> print { count = 42 }
                            |> Expect.equal "This is the 42nd message."
                ]
            ]
        ]



---- EXAMPLE MESSAGES


simpleMessage =
    [ s "Hello!" ]


stringMessage =
    [ s "Good morning, "
    , string .name
    , s "!"
    ]


enCardinalMsg =
    [ En.cardinal .count
        { one =
            [ s "There is one new message." ]
        , other =
            [ s "There are "
            , count
            , s " new messages."
            ]
        }
    ]


enOrdinalMsg =
    [ En.ordinal .count
        { one =
            [ s "This is the "
            , count
            , s "st message."
            ]
        , two =
            [ s "This is the "
            , count
            , s "nd message."
            ]
        , few =
            [ s "This is the "
            , count
            , s "rd message."
            ]
        , other =
            [ s "This is the "
            , count
            , s "th message."
            ]
        }
    ]
