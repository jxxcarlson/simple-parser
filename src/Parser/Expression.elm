module Parser.Expression exposing
    ( Expr(..)
    , Rule(..)
    , State
    , parse
    , run
    )

import Either exposing (Either(..))
import Maybe.Extra
import Parser.Match as M exposing (Symbol(..))
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
    , stack : List Token
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
    { state | stack = token :: state.stack, tokens = List.drop 1 state.tokens }


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
    { state | tokens = List.drop 1 state.tokens, stack = token :: state.stack }



-- REDUCE


toSymbols : List Token -> List Symbol
toSymbols tokens =
    List.map toSymbol tokens |> Maybe.Extra.values


toSymbols2 : List Token -> List Symbol
toSymbols2 tokens =
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


reduceState : State -> State
reduceState state =
    if M.reducible (state.stack |> toSymbols |> List.reverse) then
        let
            reducedStack =
                eval (state.stack |> List.reverse)
        in
        { state | stack = [], committed = reducedStack ++ state.committed }

    else
        recoverFromError2 state


unbracket : List a -> List a
unbracket list =
    List.drop 1 (List.take (List.length list - 1) list)


eval : List Token -> List Expr
eval tokens =
    if
        List.map Token.type_ (List.take 1 tokens)
            == [ TLB ]
            && List.map Token.type_ (List.take 1 (List.reverse tokens))
            == [ TRB ]
    then
        let
            args =
                unbracket tokens
        in
        case List.head args of
            Just (S name meta) ->
                [ Expr name (evalList (List.drop 1 args)) meta ]

            Nothing ->
                [ Text "error: expected S name ..." dummyLoc ]

            _ ->
                [ Text "error: expected something different ..." dummyLoc ]

    else
        []


evalList : List Token -> List Expr
evalList tokens =
    case List.head tokens of
        Just token ->
            case Token.type_ token of
                TLB ->
                    case M.match (toSymbols2 tokens) of
                        Nothing ->
                            [ Text "error on match" dummyLoc ]

                        Just k ->
                            let
                                ( a, b ) =
                                    M.splitAt (k + 1) tokens
                            in
                            eval a ++ evalList b

                _ ->
                    case exprOfToken token of
                        Just expr ->
                            expr :: evalList (List.drop 1 tokens)

                        Nothing ->
                            [ Text "error converting Token" dummyLoc ]

        _ ->
            []


recoverFromError1 unreducedStack state =
    state


recoverFromError2 state =
    state


reduce : List Token -> Either (List Token) Expr
reduce stack =
    Right (Text "done (ha ha)" { begin = 0, end = 0 })



-- HELPERS


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
