module Main exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Cldr.Ar.AE exposing (..)
import Text exposing (..)


withCount : Text Static { args | count : Float } node
withCount =
    float .count decimalArabStandard


suite : Benchmark
suite =
    describe "cldr"
        [ Benchmark.compare "number format"
            "cldr"
            (\_ ->
                withCount
                    |> asStringWith { count = 1234567890 }
            )
            "toString"
            (\_ ->
                toString 1234567890
            )
        ]


main : BenchmarkProgram
main =
    program suite
