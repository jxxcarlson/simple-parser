module Parser.Expression exposing
    ( Expr(..)
    , State
    , parse
    , run
    )

import Either exposing (Either(..))
import List.Extra
import Parser.Match as M
import Parser.Symbol as Symbol exposing (Symbol(..))
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
    , numberOfTokens : Int
    , tokenIndex : Int
    , committed : List Expr
    , stack : List Token
    }



-- STATE FOR THE PARSER


init : String -> State
init str =
    let
        tokens =
            Token.run str |> List.reverse
    in
    { step = 0
    , tokens = tokens
    , numberOfTokens = List.length tokens
    , tokenIndex = 0
    , committed = []
    , stack = []
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
    case List.Extra.getAt state.tokenIndex state.tokens of
        Nothing ->
            if List.isEmpty state.stack then
                Done state

            else
                recoverFromError state

        Just token ->
            pushToken token { state | tokenIndex = state.tokenIndex + 1 }
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
            pushLeft token state

        RB _ ->
            pushLeft token state

        TokenError _ _ ->
            pushLeft token state


pushLeft : Token -> State -> State
pushLeft token state =
    { state | stack = token :: state.stack }


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
            { state | committed = expr :: state.committed }


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
    { state | stack = token :: state.stack }



-- REDUCE


reduceState : State -> State
reduceState state =
    if M.reducible (state.stack |> Symbol.convertTokens |> List.reverse) then
        let
            reducedStack =
                eval (state.stack |> List.reverse)
        in
        { state | stack = [], committed = reducedStack ++ state.committed }

    else
        state


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
                    case M.match (Symbol.convertTokens2 tokens) of
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


errorMessage : String -> Expr
errorMessage message =
    Expr "red" [ Text message dummyLoc ] dummyLoc


addErrorMessage : String -> State -> State
addErrorMessage message state =
    { state | committed = errorMessage message :: state.committed }


recoverFromError : State -> Step State State
recoverFromError state =
    let
        k =
            Symbol.balance <| Symbol.convertTokens (List.reverse state.stack)

        newStack =
            List.repeat k (RB dummyLoc) ++ state.stack

        newSymbols =
            Symbol.convertTokens (List.reverse newStack)

        reducible =
            M.reducible newSymbols
    in
    if reducible then
        Done <| addErrorMessage " ] " <| reduceState <| { state | stack = newStack, tokenIndex = 0, numberOfTokens = List.length newStack }

    else
        Done
            { state
                | committed =
                    Expr "red" [ Text (" << braces (" ++ String.fromInt k ++ ")") dummyLoc ] dummyLoc
                        :: Expr "blue" [ Text (" " ++ Token.toString state.tokens) dummyLoc ] dummyLoc
                        :: state.committed
            }



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
