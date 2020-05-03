module RTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Rationalizer exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Testing Root"
        [ describe "Testing valid ingredient inputs"
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
        , describe "Testing rearrangement of ingredients"
            [ test "moving up" <|
                \_ ->
                    [ parseIngredient "4 chickens", parseIngredient "6 tomatoes", parseIngredient "3 tbsp salt" ]
                        |> moveIngredUp (parseIngredient "3 tbsp salt")
                        |> Expect.equal [ parseIngredient "4 chickens", parseIngredient "3 tbsp salt", parseIngredient "6 tomatoes" ]
            , test "moving down" <|
                \_ ->
                    [ parseIngredient "4 chickens", parseIngredient "6 tomatoes", parseIngredient "3 tbsp salt" ]
                        |> moveIngredUp (parseIngredient "6 tomatoes")
                        |> Expect.equal [ parseIngredient "6 tomatoes", parseIngredient "4 chickens", parseIngredient "3 tbsp salt" ]
            ]
        ]
