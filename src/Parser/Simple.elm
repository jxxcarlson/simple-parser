module Parser.Simple exposing (ExprS(..), parse, simplifyToken, tokenize)

import Either exposing (Either)
import Parser.Advanced exposing (DeadEnd)
import Parser.Expression as Expression exposing (Expr(..), State)
import Parser.Token as Token exposing (Token(..))
import Parser.Tools exposing (Context, Problem)


type ExprS
    = ExprS String (List ExprS)
    | TextS String
    | VerbatimS String String
    | EVS ExprS
    | ErrorS String


type alias StateS =
    { step : Int
    , tokens : List SimpleToken
    , committed : List ExprS
    , stack : List SimpleToken
    , bracketCount : Int
    }


type SimpleToken
    = LBS
    | RBS
    | SS String
    | WS String
    | VerbatimTokenS String String
    | TokenErrorS (List (DeadEnd Context Problem))


tokenize : String -> List SimpleToken
tokenize str =
    Token.run str |> List.map simplifyToken


parse : String -> StateS
parse str =
    Expression.parse str |> toStateS


toStateS : State -> StateS
toStateS state =
    { step = state.step
    , tokens = List.map simplifyToken state.tokens
    , committed = List.map simplify state.committed
    , stack = List.reverse <| simplifyStack state.stack
    , bracketCount = state.bracketCount
    }


simplifyToken : Token -> SimpleToken
simplifyToken token =
    case token of
        LB _ ->
            LBS

        RB _ ->
            RBS

        S str _ ->
            SS str

        W str _ ->
            WS str

        VerbatimToken name str _ ->
            VerbatimTokenS name str

        TokenError list _ ->
            TokenErrorS list


simplifyStack : List Token -> List SimpleToken
simplifyStack stack =
    List.map simplifyToken stack


simplify : Expr -> ExprS
simplify expr =
    case expr of
        Expr str expresssions _ ->
            ExprS str (List.map simplify expresssions)

        Text str _ ->
            TextS str

        Verbatim name str _ ->
            VerbatimS name str

        EV expr_ ->
            EVS (simplify expr_)

        Error str ->
            ErrorS str



-- HELPERS
