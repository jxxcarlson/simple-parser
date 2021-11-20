module Parser.Token exposing
    ( Loc
    , Token(..)
    , TokenType(..)
    , init
    , nextStep
    , run
    , stringValue
    , type_
    )

import Parser.Advanced as Parser exposing (DeadEnd, Parser)
import Parser.Tools as PT exposing (Context, Problem)



-- TYPES


type Token
    = LB Loc
    | RB Loc
    | S String Loc
    | W String Loc
    | VerbatimToken String String Loc
    | TokenError (List (DeadEnd Context Problem)) Loc


type alias Loc =
    { begin : Int, end : Int }


type alias State a =
    { source : String
    , scanpointer : Int
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

        VerbatimToken _ str _ ->
            str

        TokenError _ _ ->
            "tokenError"


length : Token -> Int
length token =
    case token of
        LB _ ->
            1

        RB _ ->
            1

        S str _ ->
            String.length str

        VerbatimToken _ str _ ->
            String.length str

        W str _ ->
            String.length str

        TokenError _ _ ->
            3


init : String -> State a
init str =
    { source = str, scanpointer = 0, sourceLength = String.length str, tokens = [] }


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
    case Parser.run (tokenParser state start) input of
        Ok token ->
            token

        Err errorList ->
            TokenError errorList { begin = start, end = start + 1 }


nextStep : State Token -> Step (State Token) (List Token)
nextStep state =
    if state.scanpointer >= state.sourceLength then
        Done state.tokens

    else
        let
            token =
                get state state.scanpointer (String.dropLeft state.scanpointer state.source)

            newScanPointer =
                state.scanpointer + length token
        in
        Loop { state | tokens = token :: state.tokens, scanpointer = newScanPointer }


{-| Expression.Tokenizer.tokenParser calls L1.tokenParser
with arguments tokenStack and start. The first argument
is not used (although it is for the Markdown parser)
-}
tokenParser : a -> Int -> TokenParser
tokenParser _ start =
    tokenParser_ start


l1LanguageChars =
    [ '[', ']', '`', '$' ]


tokenParser_ : Int -> TokenParser
tokenParser_ start =
    Parser.oneOf
        [ textParser start
        , mathParser start
        , codeParser start
        , functionPartsParser start
        , leftBracketParser start
        , rightBracketParser start
        , whiteSpaceParser start
        ]


whiteSpaceParser : Int -> TokenParser
whiteSpaceParser start =
    PT.text (\c -> c == ' ') (\c -> c == ' ')
        |> Parser.map (\data -> W data.content { begin = start, end = start })


leftBracketParser : Int -> TokenParser
leftBracketParser start =
    PT.text (\c -> c == '[') (\_ -> False)
        |> Parser.map (\_ -> LB { begin = start, end = start })


rightBracketParser : Int -> TokenParser
rightBracketParser start =
    PT.text (\c -> c == ']') (\_ -> False)
        |> Parser.map (\_ -> RB { begin = start, end = start })


textParser start =
    PT.text (\c -> not <| List.member c (' ' :: l1LanguageChars)) (\c -> not <| List.member c (' ' :: l1LanguageChars))
        |> Parser.map (\data -> S data.content { begin = start, end = start + data.end - data.begin - 1 })


mathParser : Int -> TokenParser
mathParser start =
    PT.textWithEndSymbol "$" (\c -> c == '$') (\c -> c /= '$')
        |> Parser.map (\data -> VerbatimToken "math" data.content { begin = start, end = start + data.end - data.begin - 1 })


codeParser : Int -> TokenParser
codeParser start =
    PT.textWithEndSymbol "`" (\c -> c == '`') (\c -> c /= '`')
        |> Parser.map (\data -> VerbatimToken "code" data.content { begin = start, end = start + data.end - data.begin - 1 })


functionPartsParser : Int -> TokenParser
functionPartsParser start =
    PT.textWithEndSymbol " " Char.isAlphaNum (\c -> c /= ' ')
        |> Parser.map (\data -> S data.content { begin = start, end = start + data.end - data.begin - 1 })



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
