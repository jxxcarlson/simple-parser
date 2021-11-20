module ParserTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser.Expression as Expression
import Parser.Simple as Simple exposing (ExprS(..))
import Render.Text as Text
import Test exposing (..)


parseTest label input output =
    test label <|
        \_ ->
            input
                |> Simple.parse
                |> .committed
                |> Expect.equal output


idemTest label input =
    test label <|
        \_ ->
            input
                |> idem
                |> Expect.equal input


idem : String -> String
idem str =
    str
        |> Expression.parse
        |> .committed
        |> Text.print


suite : Test
suite =
    describe "Parser.Expression and Parser.Simple modules"
        [ describe "Simple.parse"
            [ parseTest "A1. word" "word" [ TextS "word" ]
            , parseTest "A2. [f a]" "[f a]" [ ExprS "f" [ TextS " ", TextS "a" ] ]
            , parseTest "A3. a [f b] c" "a [f b] c" [ TextS "a", TextS " ", ExprS "f" [ TextS " ", TextS "b" ], TextS " ", TextS "c" ]
            , parseTest "A4. [f [g a] b]" "[f [g a] b]" [ ExprS "f" [ TextS " ", ExprS "g" [ TextS " ", TextS "a" ], TextS " ", TextS "b" ] ]
            ]
        , describe "Idempotency test"
            [ idemTest "B1." "word"
            , idemTest "B2." "[f a]"
            , idemTest "B3." "a [f b] c"
            , idemTest "B4." "[f [g a] b]"
            , idemTest "B5." "[f a [g u v] [h x y]]" -- DEPTH 2 OK
            , idemTest "B6." "[f [g [h b]]]" -- DEPTH 3 FAILS
            , idemTest "B7." "[f [g [h b]] c]"
            , idemTest "B8" "[f[g [h [i b]]] c]"
            ]
        ]
