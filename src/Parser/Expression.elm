module Parser.Expression exposing
    ( Expr(..)
    , Rule(..)
    , State
    , parse
    , run
    )

import Either exposing (Either(..))
import List.Extra
import Parser.Token as Token exposing (Loc, Token(..), TokenType(..))



-- TYPES


type Expr
    = Expr String (List Expr) Loc
    | Text String Loc
    | Verbatim String String Loc
    | EV Expr
    | Error String


type alias State =
    { step : Int
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



-- STATE FOR THE PARSER


init : String -> State
init str =
    { step = 0
    , tokens = Token.run str |> List.reverse
    , committed = []
    , stack = []
    , bracketCount = 0
    }



-- PARSER


parse : String -> State
parse str =
    run (init str)


run : State -> State
run state =
    loop state nextStep
        |> (\state_ -> { state_ | committed = List.reverse state_.committed })


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
                |> reduceState
                |> (\st -> { st | step = st.step + 1 })
                |> Loop



-- PUSH


pushToken : Token -> State -> State
pushToken token state =
    case token of
        S _ _ ->
            pushOrCommit token state

        W _ _ ->
            pushOrCommit token state

        VerbatimToken _ _ _ ->
            pushOrCommit token state

        LB _ ->
            pushLeft token { state | bracketCount = state.bracketCount + 1 }

        RB _ ->
            pushLeft token state

        TokenError _ _ ->
            pushLeft token state


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


exprOfToken : Token -> Maybe Expr
exprOfToken token =
    case token of
        S str loc ->
            Just (Text str loc)

        W str loc ->
            Just (Text str loc)

        VerbatimToken name str loc ->
            Just (Verbatim name str loc)

        _ ->
            Nothing


push : Token -> State -> State
push token state =
    { state | tokens = List.drop 1 state.tokens, stack = Left token :: state.stack }



-- REDUCE


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


reduce : List (Either Token Expr) -> { rule : Rule, result : List (Either Token Expr) }
reduce list =
    case list of
        (Left (S name meta2)) :: (Left (LB meta1)) :: _ ->
            let
                _ =
                    Debug.log "RULE" "F"

                _ =
                    Debug.log "stack" list
            in
            { rule = F, result = Right (Expr name [] { begin = meta1.begin, end = meta2.end }) :: List.drop 1 list }

        (Left (S content meta2)) :: (Right (Expr name exprs meta1)) :: _ ->
            let
                _ =
                    Debug.log "RULE" "A1"

                _ =
                    Debug.log "stack" list
            in
            { rule = A1, result = Right (Expr name (Text content meta1 :: exprs) { begin = meta1.begin, end = meta2.end }) :: List.drop 2 list }

        (Left (W content meta2)) :: (Right (Expr name exprs meta1)) :: _ ->
            let
                _ =
                    Debug.log "RULE" "A2"

                _ =
                    Debug.log "stack" list
            in
            { rule = A2, result = Right (Expr name (Text content meta1 :: exprs) { begin = meta1.begin, end = meta2.end }) :: List.drop 2 list }

        (Right (Expr name2 exprs2 meta2)) :: (Right (Expr name1 exprs1 meta1)) :: _ ->
            let
                _ =
                    Debug.log "RULE" "A3"

                _ =
                    Debug.log "stack" list
            in
            { rule = A3, result = Right (Expr name1 (exprs1 ++ [ Expr name2 exprs2 meta2 ]) { begin = meta1.begin, end = meta2.end }) :: List.drop 2 list }

        (Left (RB _)) :: rest ->
            let
                _ =
                    Debug.log "RULE" "M"

                _ =
                    Debug.log "stack" ( List.length list, list )

                prefix =
                    List.Extra.takeWhile (\t -> not (isLB t)) rest |> List.map evaluated

                suffix =
                    List.drop (List.length prefix + 1) rest
            in
            { rule = M, result = prefix ++ suffix }

        _ ->
            { rule = NoRule, result = list }


reduceStateM : State -> State
reduceStateM state =
    let
        _ =
            Debug.log "PENULTIMATE" state.stack

        finalExpr : Expr
        finalExpr =
            (reduce state.stack).result |> List.map unevaluated |> apply

        -- apply
        committed =
            finalExpr :: state.committed

        stack =
            if state.bracketCount == 0 then
                []

            else
                state.stack
    in
    { state | stack = stack, committed = committed }


eval : List (Either Token Expr) -> List (Either Token Expr)
eval items =
    items



-- APPLY (HELPER FOR reduceM)


apply : List (Either Token Expr) -> Expr
apply list_ =
    list_ |> Debug.log "apply (IN)" |> apply_ |> Debug.log "apply (OUT)"


apply_ : List (Either Token Expr) -> Expr
apply_ list_ =
    let
        list =
            List.reverse list_
    in
    case List.head list of
        Nothing ->
            Text "Error: empty list in function apply" dummyLoc

        Just f ->
            case f of
                Right (Expr name args_ meta) ->
                    let
                        args =
                            List.foldl (\item acc -> makeArg item :: acc) args_ (List.drop 1 list)
                                |> List.reverse
                    in
                    Expr name args meta

                _ ->
                    Text "Error: expected FExpr in first position" dummyLoc


makeArg : Either Token Expr -> Expr
makeArg item =
    case item of
        Right expr ->
            expr
                |> Debug.log "makeArg (EXPR, INT)"
                |> reverseArgs
                |> Debug.log "makeArg (EXPR, OUT)"

        Left token ->
            case token of
                S str meta ->
                    Text str meta

                W str meta ->
                    Text str meta

                _ ->
                    Text "Error in converting token to expr" dummyLoc


reverseArgs : Expr -> Expr
reverseArgs expr =
    case expr of
        Expr name args loc ->
            Expr name (List.reverse args) loc

        _ ->
            expr



-- EVALUATED AND UNEVALUATED (HELPERS FOR reduce and reduceM)


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



-- HELPERS


isLB : Either Token Expr -> Bool
isLB stackItem =
    case stackItem of
        Right _ ->
            False

        Left token ->
            Token.type_ token == TLB


dummyLoc =
    { begin = 0, end = 0 }



-- LOOP


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
