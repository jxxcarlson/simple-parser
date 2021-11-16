module Expression exposing (..)

import Array exposing (Array)
import Either exposing (Either(..))
import Maybe.Extra
import Token exposing (Loc, SimpleToken(..), Token(..), TokenType(..), run)


type Expr
    = Text
    | FExpr String (Array Expr) Loc
    | L (Array Expr)
    | StringExpr String Loc
    | MathExpr String Loc
    | CodeExpr String Loc


type ExprS
    = TextS
    | FExprS String (Array ExprS)
    | LS (Array ExprS)
    | StringExprS String
    | MathExprS String
    | CodeExprS String


simplify : Expr -> ExprS
simplify expr =
    case expr of
        Text ->
            TextS

        FExpr str expresssions loc ->
            FExprS str (Array.map simplify expresssions)

        L expressions ->
            LS (Array.map simplify x`expressions)

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
    = T (Array String)


type FExpr
    = F String (Array Expr) Loc


first : Array Token -> Maybe Token
first tokens =
    Array.get 0 tokens


second : Array Token -> Maybe Token
second tokens =
    Array.get 1 tokens


last : Array Token -> Maybe Token
last tokens =
    Array.get (Array.length tokens - 1) tokens


reduce : Array Token -> Array Expr
reduce tokens =
    case Maybe.map Token.type_ (first tokens) of
        Just TLBR ->
            case Maybe.map Token.type_ (last tokens) of
                Just TRBR ->
                    case second tokens of
                        Just (S name meta) ->
                            let
                                tokens2 : Array Expr
                                tokens2 =
                                    reduce2 (Array.slice 2 (Array.length tokens - 1) tokens |> Debug.log "TOKENS2 (1)") |> Debug.log "TOKENS2 (2)"
                            in
                            Array.fromList [ FExpr name tokens2 meta ]

                        _ ->
                            Array.empty

                _ ->
                    Array.empty

        _ ->
            Array.empty


reduce2 : Array Token -> Array Expr
reduce2 tokens =
    let
        _ =
            tokens |> Debug.log "reduce2 (top)"
    in
    case first tokens of
        Just (S str meta) ->
            let
                array2 : Array Expr
                array2 =
                    reduce2 (Array.slice 1 (Array.length tokens - 1) tokens |> Debug.log "reduce2 (1a)") |> Debug.log "reduce2 (1b)"
            in
            Array.append (Array.fromList [ StringExpr str meta ]) array2 |> Debug.log "reduce2 (1b)"

        Just (W str meta) ->
            let
                array2 : Array Expr
                array2 =
                    reduce2 (Array.slice 1 (Array.length tokens) (tokens |> Debug.log "reduce2 (2a)") |> Debug.log "reduce2 (2b)") |> Debug.log "reduce2 (2c)"
            in
            Array.append (Array.fromList [ StringExpr str meta ]) array2

        Just (Math str meta) ->
            let
                array2 : Array Expr
                array2 =
                    reduce2 (Array.slice 1 (Array.length tokens - 1) tokens) |> Debug.log "reduce2 (3)"
            in
            Array.append (Array.fromList [ MathExpr str meta ]) array2

        Just (Code str meta) ->
            let
                array2 : Array Expr
                array2 =
                    reduce2 (Array.slice 1 (Array.length tokens - 1) tokens)
            in
            Array.append (Array.fromList [ CodeExpr str meta ]) array2 |> Debug.log "reduce2 (4)"

        _ ->
            reduce tokens



--
--reduce : Stack -> Stack
--reduce stack =
--    --let
--    --    _ =
--    --        stack |> Array.map (\item -> Either.mapBoth simplify Token.simplify item) |> Debug.log "TOKENS (TOP)"
--    --in
--    if
--        (Array.get 0 stack |> Maybe.map Token.simplify)
--            == Just LBRS
--            && (Array.get (Array.length tokens - 1) tokens |> Maybe.map Token.simplify)
--            == Just RBRS
--    then
--        let
--            sl =
--                Array.slice 1 -1 tokens |> Debug.log "SLICE"
--        in
--        reduce sl |> Debug.log "TOKENS (1)"
--
--    else if List.all (\t -> not (List.member (Token.type_ t) [ TRBR, TLBR, TTokenError ])) (Array.toList tokens) then
--        let
--            _ =
--                Debug.log "TOKENS (1.5)" tokens
--
--            fName : String
--            fName =
--                Array.get 0 tokens |> Maybe.map Token.stringValue |> Maybe.withDefault "function"
--
--            args : Array Expr
--            args =
--                Array.map exprOfToken (Array.slice 1 (Array.length tokens) tokens)
--                    |> Maybe.Extra.combineArray
--                    |> Maybe.withDefault Array.empty
--        in
--        Array.fromList [ FExpr fName args ]
--
--    else
--        Array.empty |> Debug.log "TOKENS (3)"
