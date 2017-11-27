module Main exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Localized exposing (..)
import Localized.Fil exposing (..)


staticMessage args =
    [ cardinal .count
        { one = [ s "one" ]
        , other = [ s "other" ]
        }
    ]
        |> printWith args


dynamicMessage args =
    [ cardinalDynamic .count
        { one = [ s "one" ]
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
