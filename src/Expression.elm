module Expression exposing (..)

import Array exposing (Array)
import Either exposing (Either(..))
import List.Extra
import Token exposing (Loc, SimpleToken(..), Token(..), TokenType(..), run)


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


type alias State =
    { step : Int
    , stackPointer : Int
    , tokens : List Token
    , committed : List Expr
    , stack : List (Either Token Expr)
    }


type alias StateS =
    { step : Int
    , stackPointer : Int
    , tokens : List SimpleToken
    , committed : List ExprS
    , stack : List (Either SimpleToken ExprS)
    }


toStateS : State -> StateS
toStateS state =
    { step = state.step
    , stackPointer = state.stackPointer
    , tokens = List.map Token.simplify state.tokens
    , committed = List.map simplify state.committed
    , stack = List.reverse <| simplifyStack state.stack
    }


parseS : String -> StateS
parseS str =
    parse str |> toStateS


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
    { step = 0
    , stackPointer = 0
    , tokens = Token.run str |> List.reverse
    , committed = []
    , stack = []
    }


run : State -> State
run state =
    loop state nextStep
        |> (\state_ -> { state_ | committed = List.reverse state_.committed })


displayState1 : State -> State
displayState1 state =
    let
        _ =
            Debug.log "tokens" (state.tokens |> List.map Token.simplify)
    in
    state


displayState2 : State -> State
displayState2 state =
    let
        _ =
            Debug.log "stack" (state.stack |> simplifyStack)

        _ =
            Debug.log "(N, P)" ( List.length state.stack, state.stackPointer )

        _ =
            Debug.log "committed" (state.committed |> List.map simplify)
    in
    state


pushToken : Token -> State -> State
pushToken token state =
    case token of
        S _ _ ->
            pushOrCommit token state

        W _ _ ->
            pushOrCommit token state

        Math _ _ ->
            pushOrCommit token state

        Code _ _ ->
            pushOrCommit token state

        LB _ ->
            pushLeft token state

        RB _ ->
            pushLeft token state

        TokenError _ _ ->
            pushLeft token state


nextStep : State -> Step State State
nextStep state =
    case List.head state.tokens of
        Nothing ->
            Done (reduce state)

        Just token ->
            let
                _ =
                    Debug.log "=====" (state.step + 1)

                _ =
                    Debug.log "Token" token
            in
            pushToken token state
                |> displayState1
                |> reduce
                |> displayState2
                |> (\st -> { st | step = st.step + 1 })
                |> Loop



-- |> (\st -> {st | step = st.step + 1})


reduce : State -> State
reduce state =
    case state.stack of
        -- Rule F
        (Left (S name meta2)) :: (Left (LB meta1)) :: rest ->
            let
                _ =
                    Debug.log "RULE" 'F'
            in
            { state
                | stack = Right (FExpr name [] { begin = meta1.begin, end = meta2.end }) :: List.drop 1 state.stack
            }

        -- Rule A1
        (Left (S content meta2)) :: (Right (FExpr name exprs meta1)) :: rest ->
            let
                _ =
                    Debug.log "RULE" "A1"
            in
            { state
                | stack = Right (FExpr name (StringExpr content meta1 :: exprs) { begin = meta1.begin, end = meta2.end }) :: List.drop 2 state.stack
            }

        -- Rule A2
        (Left (W content meta2)) :: (Right (FExpr name exprs meta1)) :: rest ->
            let
                _ =
                    Debug.log "RULE" "A2"
            in
            { state
                | stack = Right (FExpr name (StringExpr content meta1 :: exprs) { begin = meta1.begin, end = meta2.end }) :: List.drop 2 state.stack
            }

        -- Rule A3
        (Right (FExpr name2 exprs2 meta2)) :: (Right (FExpr name1 exprs1 meta1)) :: rest ->
            let
                _ =
                    Debug.log "RULE" "A3"
            in
            { state
                | stack = Right (FExpr name1 (exprs1 ++ [ FExpr name2 exprs2 meta2 ]) { begin = meta1.begin, end = meta2.end }) :: List.drop 2 state.stack
            }

        -- Rule M
        (Left (RB meta)) :: rest ->
            let
                _ =
                    Debug.log "RULE" 'M'

                prefix_ =
                    prefix rest

                suffix =
                    List.drop (List.length prefix_ + 1) rest
            in
            { state
                | stack = prefix_ ++ suffix
            }

        _ ->
            state


prefix : List (Either Token Expr) -> List (Either Token Expr)
prefix stack =
    List.Extra.takeWhile (\t -> not (isLB t)) stack


isLB : Either Token Expr -> Bool
isLB stackItem =
    case stackItem of
        Right _ ->
            False

        Left token ->
            Token.type_ token == TLB


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
    { state | tokens = List.drop 1 state.tokens, stack = Left token :: state.stack }


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
