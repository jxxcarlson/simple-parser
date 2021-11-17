module Expression exposing (..)

import Array exposing (Array)
import Either exposing (Either(..))
import Maybe.Extra
import Token exposing (Loc, SimpleToken(..), Token(..), TokenType(..), run)


parse : String -> State
parse str =
    run (init str)


viewStack state =
    state |> .stack |> simplifyStack |> List.reverse


simplifyStack : List (Either Token Expr) -> List (Either SimpleToken ExprS)
simplifyStack stack =
    List.map (Either.mapBoth Token.simplify simplify) stack


init : String -> State
init str =
    { tokens = Array.toList (Token.run str), committed = [], stack = [] }


run : State -> State
run state =
    loop state nextStep
        |> (\state_ -> { state_ | committed = List.reverse state_.committed })


type alias State =
    { tokens : List Token, committed : List Expr, stack : List (Either Token Expr) }


nextStep : State -> Step State State
nextStep state =
    case List.head state.tokens of
        Nothing ->
            Done (reduce state)

        Just token ->
            case token of
                S _ _ ->
                    Loop (pushOrCommit token (reduce state))

                W _ _ ->
                    Loop (pushOrCommit token (reduce state))

                Math _ _ ->
                    Loop (pushOrCommit token (reduce state))

                Code _ _ ->
                    Loop (pushOrCommit token (reduce state))

                LB _ ->
                    Loop (pushLeft token (reduce state))

                RB _ ->
                    Loop (pushLeft token (reduce state))

                TokenError _ _ ->
                    Loop (pushLeft token (reduce state))


reduce : State -> State
reduce state =
    state


pushLeft : Token -> State -> State
pushLeft token state =
    { state | stack = Left token :: state.stack, tokens = List.drop 1 state.tokens }


pushOrCommit : Token -> State -> State
pushOrCommit token state =
    if List.isEmpty state.stack then
        commit token state

    else
        push token state


commit : Token -> State -> State
commit token state =
    case exprOfToken token of
        Nothing ->
            state

        Just expr ->
            { state | tokens = List.drop 1 state.tokens, committed = expr :: state.committed }


push : Token -> State -> State
push token state =
    case exprOfToken token of
        Nothing ->
            state

        Just expr ->
            { state | tokens = List.drop 1 state.tokens, stack = Right expr :: state.stack }


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


type Expr
    = Text
    | FExpr String (List Expr) Loc
    | L (List Expr)
    | StringExpr String Loc
    | MathExpr String Loc
    | CodeExpr String Loc


type ExprS
    = TextS
    | FExprS String (List ExprS)
    | LS (List ExprS)
    | StringExprS String
    | MathExprS String
    | CodeExprS String


simplify : Expr -> ExprS
simplify expr =
    case expr of
        Text ->
            TextS

        FExpr str expresssions loc ->
            FExprS str (List.map simplify expresssions)

        L expressions ->
            LS (List.map simplify expressions)

        StringExpr str loc ->
            StringExprS str

        MathExpr str loc ->
            MathExprS str

        CodeExpr str loc ->
            CodeExprS str


type alias StackItem =
    Either Token Expr


type alias Stack =
    Array StackItem


exprOfToken : Token -> Maybe Expr
exprOfToken token =
    case token of
        S str loc ->
            Just (StringExpr str loc)

        W str loc ->
            Just (StringExpr str loc)

        Math str loc ->
            Just (MathExpr str loc)

        Code str loc ->
            Just (CodeExpr str loc)

        _ ->
            Nothing


type Text
    = T (List String)



--
--type FExpr
--    = F String (List Expr) Loc
