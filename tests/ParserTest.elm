module ParserTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser.Simple as Simple exposing (ExprS(..))
import Test exposing (..)


parseTest label input output =
    test label <|
        \_ ->
            input
                |> Simple.parse
                |> .committed
                |> Expect.equal output


suite : Test
suite =
    describe "Parser.Expression and Parser.Simple modules"
        [ describe "Simple.parse"
            [ parseTest "1. word" "word" [ TextS "word" ]
            , parseTest "2. [f a]" "[f a]" [ ExprS "f" [ TextS " ", TextS "a" ] ]
            , parseTest "3. a [f b] c" "a [f b] c" [ TextS "a", TextS " ", ExprS "f" [ TextS " ", TextS "b" ], TextS " ", TextS "c" ]
            , parseTest "4. [f [g a] b]" "[f [g a] b]" [ ExprS "f" [ TextS " ", ExprS "g" [ TextS "a", TextS " " ], TextS " ", TextS "b" ] ]
            ]
        ]
