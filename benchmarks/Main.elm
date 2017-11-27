module Main exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Localized exposing (..)
import Localized.En exposing (..)


staticMessage args =
    [ ordinal .count
        { one = [ s "one" ]
        , two = [ s "two" ]
        , few = [ s "few" ]
        , other = [ s "other" ]
        }
    ]
        |> printWith args


dynamicMessage args =
    [ ordinalDynamic .count
        { one = [ s "one" ]
        , two = [ s "two" ]
        , few = [ s "few" ]
        , other = [ s "other" ]
        }
    ]
        |> printWith args


suite : Benchmark
suite =
    describe "plural parts"
        [ Benchmark.compare "static and dynamic"
            (benchmark1 "print staticMessage" staticMessage { count = 1 })
            (benchmark1 "print dynamicMessage" dynamicMessage { count = 1 })
        ]


main : BenchmarkProgram
main =
    program suite
