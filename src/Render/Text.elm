module Render.Text exposing (fromExpr, print)

import Parser.Expression as Expression exposing (Expr(..))


print : List Expr -> String
print expressions =
    List.map fromExpr expressions |> String.join ""


fromExpr : Expr -> String
fromExpr expr =
    case expr of
        Expr name expressions _ ->
            "[" ++ name ++ (List.map fromExpr expressions |> String.join "") ++ "]"

        Text str _ ->
            str

        Verbatim name str _ ->
            case name of
                "math" ->
                    "$" ++ str ++ "$"

                "code" ->
                    "`" ++ str ++ "`"

                _ ->
                    "error: verbatim " ++ name ++ " not recognized"

        EV expr_ ->
            "EV (" ++ fromExpr expr_ ++ ")"

        Error str ->
            "Error (" ++ str ++ ")"