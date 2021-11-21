module Parser.Symbol exposing (Symbol(..), convertTokens, convertTokens2, toString)

import Maybe.Extra
import Parser.Token exposing (Token(..))


type Symbol
    = L
    | R
    | O


symbolToString : Symbol -> String
symbolToString symbol =
    case symbol of
        L ->
            "L"

        R ->
            "R"

        O ->
            "O"


toString : List Symbol -> String
toString symbols =
    List.map symbolToString symbols |> String.join " "


convertTokens : List Token -> List Symbol
convertTokens tokens =
    List.map toSymbol tokens |> Maybe.Extra.values


convertTokens2 : List Token -> List Symbol
convertTokens2 tokens =
    List.map toSymbol2 tokens


toSymbol : Token -> Maybe Symbol
toSymbol token =
    case token of
        LB _ ->
            Just L

        RB _ ->
            Just R

        _ ->
            Nothing


toSymbol2 : Token -> Symbol
toSymbol2 token =
    case token of
        LB _ ->
            L

        RB _ ->
            R

        _ ->
            O
