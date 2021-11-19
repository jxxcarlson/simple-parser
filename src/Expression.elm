module Expression exposing (..)

import Array exposing (Array)
import Either exposing (Either(..))
import List.Extra
import Token exposing (Loc, SimpleToken(..), Token(..), TokenType(..), run)



-- TYPES


type Expr
    = Text
    | FExpr String (List Expr) Loc
    | L (List Expr)
    | StringExpr String Loc
    | MathExpr String Loc
    | CodeExpr String Loc
    | EV Expr


type ExprS
    = TextS
    | FExprS String (List ExprS)
    | LS (List ExprS)
    | StringExprS String
    | MathExprS String
    | CodeExprS String
    | EVS ExprS


type alias State =
    { step : Int
    , stackPointer : Int
    , tokens : List Token
    , committed : List Expr
    , stack : List (Either Token Expr)
    , bracketCount : Int
    }


type Rule
    = F
    | A1
    | A2
    | A3
    | M
    | NoRule


init : String -> State
init str =
    { step = 0
    , stackPointer = 0
    , tokens = Token.run str |> List.reverse
    , committed = []
    , stack = []
    , bracketCount = 0
    }


type alias StateS =
    { step : Int
    , stackPointer : Int
    , tokens : List SimpleToken
    , committed : List ExprS
    , stack : List (Either SimpleToken ExprS)
    , bracketCount : Int
    }


toStateS : State -> StateS
toStateS state =
    { step = state.step
    , stackPointer = state.stackPointer
    , tokens = List.map Token.simplify state.tokens
    , committed = List.map simplify state.committed
    , stack = List.reverse <| simplifyStack state.stack
    , bracketCount = state.bracketCount
    }



-- PARSER


parseS : String -> StateS
parseS str =
    parse str |> toStateS


parse : String -> State
parse str =
    run (init str)


simplifyStack : List (Either Token Expr) -> List (Either SimpleToken ExprS)
simplifyStack stack =
    List.map (Either.mapBoth Token.simplify simplify) stack


run : State -> State
run state =
    loop state nextStep
        |> (\state_ -> { state_ | committed = List.reverse state_.committed })


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
            pushLeft token { state | bracketCount = state.bracketCount + 1 }

        RB _ ->
            pushLeft token state

        TokenError _ _ ->
            pushLeft token state


nextStep : State -> Step State State
nextStep state =
    case List.head state.tokens of
        Nothing ->
            let
                state2 =
                    reduceState state
            in
            case List.head state2.stack of
                Just (Right expr) ->
                    Done { state2 | committed = expr :: state.committed, stack = List.drop 1 state.stack }

                _ ->
                    Done state

        Just token ->
            pushToken token state
                --|> displayState1
                |> reduceState
                --  |> displayState2
                |> (\st -> { st | step = st.step + 1 })
                |> Loop


reduceState : State -> State
reduceState state =
    let
        reduction =
            reduce state.stack

        bracketCount =
            if reduction.rule == M then
                state.bracketCount - 1

            else
                state.bracketCount
    in
    if reduction.rule == M && bracketCount == 0 then
        reduceStateM { state | stack = reduction.result, bracketCount = bracketCount }

    else
        { state | stack = reduction.result, bracketCount = bracketCount }


reduceStateM : State -> State
reduceStateM state =
    let
        finalExpr : Expr
        finalExpr =
            (reduce state.stack).result |> List.map unevaluated |> apply

        committed =
            finalExpr :: state.committed

        stack =
            if state.bracketCount == 0 then
                []

            else
                state.stack
    in
    { state | stack = stack, committed = committed }


reduce : List (Either Token Expr) -> { rule : Rule, result : List (Either Token Expr) }
reduce list =
    case list of
        (Left (S name meta2)) :: (Left (LB meta1)) :: rest ->
            { rule = F, result = Right (FExpr name [] { begin = meta1.begin, end = meta2.end }) :: List.drop 1 list }

        (Left (S content meta2)) :: (Right (FExpr name exprs meta1)) :: rest ->
            { rule = A1, result = Right (FExpr name (StringExpr content meta1 :: exprs) { begin = meta1.begin, end = meta2.end }) :: List.drop 2 list }

        (Left (W content meta2)) :: (Right (FExpr name exprs meta1)) :: rest ->
            { rule = A2, result = Right (FExpr name (StringExpr content meta1 :: exprs) { begin = meta1.begin, end = meta2.end }) :: List.drop 2 list }

        (Right (FExpr name2 exprs2 meta2)) :: (Right (FExpr name1 exprs1 meta1)) :: rest ->
            { rule = A3, result = Right (FExpr name1 (exprs1 ++ [ FExpr name2 exprs2 meta2 ]) { begin = meta1.begin, end = meta2.end }) :: List.drop 2 list }

        (Left (RB meta)) :: rest ->
            let
                prefix =
                    List.Extra.takeWhile (\t -> not (isLB t)) rest |> List.map evaluated

                suffix =
                    List.drop (List.length prefix + 1) rest
            in
            { rule = M, result = prefix ++ suffix }

        _ ->
            { rule = NoRule, result = list }


dummyLoc =
    { begin = 0, end = 0 }


apply : List (Either Token Expr) -> Expr
apply list_ =
    let
        list =
            List.reverse list_
    in
    case List.head list of
        Nothing ->
            StringExpr "Error: empty list in function apply" dummyLoc

        Just f ->
            case f of
                Right (FExpr name args_ meta) ->
                    let
                        args =
                            List.foldl (\item acc -> makeArg item :: acc) args_ (List.drop 1 list)
                                |> List.reverse
                    in
                    FExpr name args meta

                _ ->
                    StringExpr "Error: expected FExpr in first position" dummyLoc


makeArg : Either Token Expr -> Expr
makeArg item =
    case item of
        Right expr ->
            expr

        Left token ->
            case token of
                S str meta ->
                    StringExpr str meta

                W str meta ->
                    StringExpr str meta

                _ ->
                    StringExpr "Error in converting token to expr" dummyLoc


evaluated : Either Token Expr -> Either Token Expr
evaluated item =
    case item of
        Left _ ->
            item

        Right (EV _) ->
            item

        Right expr ->
            Right (EV expr)


unevaluated : Either Token Expr -> Either Token Expr
unevaluated item =
    case item of
        Left _ ->
            item

        Right (EV expr) ->
            Right expr

        Right expr ->
            Right expr


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

        EV expr_ ->
            EVS (simplify expr_)


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



-- DEBUGGING


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
            Debug.log "(N, B)" ( List.length state.stack, state.bracketCount )

        _ =
            Debug.log "committed" (state.committed |> List.map simplify)
    in
    state
