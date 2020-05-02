module RTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Rationalizer exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Testing valid ingredient inputs"
        [ test "just numbers" <|
            \_ ->
                let
                    test1 =
                        "10"
                in
                Expect.false "Expect this to fail" (isValidIngred test1)
        , test "ingredient with no unit" <|
            \_ ->
                "2 chickens"
                    |> isValidIngred
                    |> Expect.true "2 chickens must work"
        ]
