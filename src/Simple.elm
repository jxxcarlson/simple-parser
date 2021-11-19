module Simple exposing (parse, tokenize)

import Either exposing (Either(..))
import Expression exposing (Expr(..), State)
import Parser.Advanced exposing (DeadEnd)
import ParserTools exposing (Context, Problem)
import Token exposing (Token(..))


type ExprS
    = ExprS String (List ExprS)
    | LS (List ExprS)
    | TextS String
    | VerbatimS String String
    | EVS ExprS
    | ErrorS String


type alias StateS =
    { step : Int
    , tokens : List SimpleToken
    , committed : List ExprS
    , stack : List (Either SimpleToken ExprS)
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


simplifyStack : List (Either Token Expr) -> List (Either SimpleToken ExprS)
simplifyStack stack =
    List.map (Either.mapBoth simplifyToken simplify) stack


simplify : Expr -> ExprS
simplify expr =
    case expr of
        Expr str expresssions loc ->
            ExprS str (List.map simplify expresssions)

        L expressions ->
            LS (List.map simplify expressions)

        Text str loc ->
            TextS str

        Verbatim name str loc ->
            VerbatimS name str

        EV expr_ ->
            EVS (simplify expr_)

        Error str ->
            ErrorS str



-- HELPERS


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s f =
    case f s of
        Loop s_ ->
            loop s_ f

        Done b ->
            b
