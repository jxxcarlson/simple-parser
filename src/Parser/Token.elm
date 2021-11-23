module Parser.Token exposing
    ( Meta
    , Token(..)
    , TokenType(..)
    , codeParser
    , init
    , mathParser
    , nextStep
    , run
    , toString
    , type_
    )

import Parser.Advanced as Parser exposing (DeadEnd, Parser)
import Parser.Tools as PT exposing (Context, Problem)



-- TYPES


type Token
    = LB Meta
    | RB Meta
    | S String Meta
    | W String Meta
    | VerbatimToken String String Meta
    | TokenError (List (DeadEnd Context Problem)) Meta


type alias Meta =
    { begin : Int, end : Int, index : Int }


type alias State a =
    { source : String
    , scanpointer : Int
    , tokenIndex : Int
    , sourceLength : Int
    , tokens : List a
    }


type TokenType
    = TLB
    | TRB
    | TS
    | TW
    | TVerbatim
    | TTokenError


type_ : Token -> TokenType
type_ token =
    case token of
        LB _ ->
            TLB

        RB _ ->
            TRB

        S _ _ ->
            TS

        W _ _ ->
            TW

        VerbatimToken _ _ _ ->
            TVerbatim

        TokenError _ _ ->
            TTokenError


stringValue : Token -> String
stringValue token =
    case token of
        LB _ ->
            "["

        RB _ ->
            "]"

        S str _ ->
            str

        W str _ ->
            str

        VerbatimToken name str _ ->
            case name of
                "math" ->
                    "$" ++ str ++ "$"

                "code" ->
                    "`" ++ str ++ "`"

                _ ->
                    "unrecognized verbatim token"

        TokenError _ _ ->
            "tokenError"


toString : List Token -> String
toString tokens =
    List.map stringValue tokens |> String.join ""


length : Token -> Int
length token =
    case token of
        LB meta ->
            meta.end - meta.begin

        RB meta ->
            meta.end - meta.begin

        S _ meta ->
            meta.end - meta.begin

        VerbatimToken _ _ meta ->
            meta.end - meta.begin

        W _ meta ->
            meta.end - meta.begin

        TokenError _ meta ->
            meta.end - meta.begin


init : String -> State a
init str =
    { source = str, scanpointer = 0, sourceLength = String.length str, tokens = [], tokenIndex = 0 }


type alias TokenParser =
    Parser Context Problem Token


run : String -> List Token
run source =
    loop (init source) nextStep


{-|

    NOTES. In the computation of the end field of the Meta component of a Token,
    one must use the code `end = start + data.end - data.begin  - 1`.  The
    `-1` is because the data.end comes from the position of the scanPointer,
    which is at this juncture pointing one character beyond the string chomped.

-}
get : State Token -> Int -> String -> Token
get state start input =
    case Parser.run (tokenParser state start state.tokenIndex) input of
        Ok token ->
            token

        Err errorList ->
            TokenError errorList { begin = start, end = start + 1, index = state.tokenIndex }


nextStep : State Token -> Step (State Token) (List Token)
nextStep state =
    if state.scanpointer >= state.sourceLength then
        Done state.tokens

    else
        let
            token =
                get state state.scanpointer (String.dropLeft state.scanpointer state.source)

            newScanPointer =
                state.scanpointer + length token + 1
        in
        Loop { state | tokens = token :: state.tokens, scanpointer = newScanPointer, tokenIndex = state.tokenIndex + 1 }


{-| Expression.Tokenizer.tokenParser calls L1.tokenParser
with arguments tokenStack and start. The first argument
is not used (although it is for the Markdown parser)
-}
tokenParser : a -> Int -> Int -> TokenParser
tokenParser _ start index =
    tokenParser_ start index


languageChars =
    [ '[', ']', '`', '$' ]


tokenParser_ : Int -> Int -> TokenParser
tokenParser_ start index =
    Parser.oneOf
        [ textParser start index
        , mathParser start index
        , codeParser start index
        , leftBracketParser start index
        , rightBracketParser start index
        , whiteSpaceParser start index
        ]


whiteSpaceParser : Int -> Int -> TokenParser
whiteSpaceParser start index =
    PT.text (\c -> c == ' ') (\c -> c == ' ')
        |> Parser.map (\data -> W data.content { begin = start, end = start, index = index })


leftBracketParser : Int -> Int -> TokenParser
leftBracketParser start index =
    PT.text (\c -> c == '[') (\_ -> False)
        |> Parser.map (\_ -> LB { begin = start, end = start, index = index })


rightBracketParser : Int -> Int -> TokenParser
rightBracketParser start index =
    PT.text (\c -> c == ']') (\_ -> False)
        |> Parser.map (\_ -> RB { begin = start, end = start, index = index })


textParser start index =
    PT.text (\c -> not <| List.member c (' ' :: languageChars)) (\c -> not <| List.member c (' ' :: languageChars))
        |> Parser.map (\data -> S data.content { begin = start, end = start + data.end - data.begin - 1, index = index })


mathParser : Int -> Int -> TokenParser
mathParser start index =
    PT.textWithEndSymbol "$" (\c -> c == '$') (\c -> c /= '$')
        |> Parser.map (\data -> VerbatimToken "math" (String.dropRight 1 (String.dropLeft 1 data.content)) { begin = start, end = start + data.end - data.begin - 1, index = index })


codeParser : Int -> Int -> TokenParser
codeParser start index =
    PT.textWithEndSymbol "`" (\c -> c == '`') (\c -> c /= '`')
        |> Parser.map (\data -> VerbatimToken "code" (String.dropRight 1 (String.dropLeft 1 data.content)) { begin = start, end = start + data.end - data.begin - 1, index = index })



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
