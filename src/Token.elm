module Token exposing
    ( Loc
    , SimpleToken(..)
    , Token(..)
    , TokenType(..)
    , run
    , runS
    , simplify
    , stringValue
    , type_
    )

import Array exposing (Array)
import Parser.Advanced as Parser exposing (DeadEnd, Parser)
import ParserTools exposing (Context, Problem)


type Token
    = LBR Loc
    | RBR Loc
    | S String Loc
    | W String Loc
    | Math String Loc
    | Code String Loc
    | TokenError (List (DeadEnd Context Problem)) Loc


stringValue : Token -> String
stringValue token =
    case token of
        LBR _ ->
            "["

        RBR _ ->
            "]"

        S str _ ->
            str

        W str _ ->
            str

        Math str _ ->
            str

        Code str _ ->
            str

        TokenError list _ ->
            "tokenError"


type TokenType
    = TLBR
    | TRBR
    | TS
    | TW
    | TMath
    | TCode
    | TTokenError


type SimpleToken
    = LBRS
    | RBRS
    | SS String
    | WS String
    | MathS String
    | CodeS String
    | TokenErrorS (List (DeadEnd Context Problem))


simplify : Token -> SimpleToken
simplify token =
    case token of
        LBR _ ->
            LBRS

        RBR _ ->
            RBRS

        S str _ ->
            SS str

        W str _ ->
            WS str

        Math str _ ->
            MathS str

        Code str _ ->
            CodeS str

        TokenError list _ ->
            TokenErrorS list


type_ : Token -> TokenType
type_ token =
    case token of
        LBR _ ->
            TLBR

        RBR _ ->
            TRBR

        S str _ ->
            TS

        W str _ ->
            TW

        Math str _ ->
            TMath

        Code str _ ->
            TCode

        TokenError list _ ->
            TTokenError


length : Token -> Int
length token =
    case token of
        LBR _ ->
            1

        RBR _ ->
            1

        S str _ ->
            String.length str

        Math str _ ->
            String.length str

        Code str _ ->
            String.length str

        W str _ ->
            String.length str

        TokenError data _ ->
            3


type alias Loc =
    { begin : Int, end : Int }


type alias State a =
    { source : String, scanpointer : Int, sourceLength : Int, tokens : Array a }


init : String -> State a
init str =
    { source = str, scanpointer = 0, sourceLength = String.length str, tokens = Array.empty }


type alias TokenParser =
    Parser Context Problem Token


run : String -> Array Token
run source =
    loop (init source) nextStep


runS : String -> Array SimpleToken
runS source =
    loop (init source) nextStep |> Array.map simplify


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


nextStep : State Token -> Step (State Token) (Array Token)
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
        Loop { state | tokens = Array.push token state.tokens, scanpointer = newScanPointer }


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
    ParserTools.text (\c -> c == ' ') (\c -> c == ' ')
        |> Parser.map (\data -> W data.content { begin = start, end = start })


leftBracketParser : Int -> TokenParser
leftBracketParser start =
    ParserTools.text (\c -> c == '[') (\_ -> False)
        |> Parser.map (\_ -> LBR { begin = start, end = start })


rightBracketParser : Int -> TokenParser
rightBracketParser start =
    ParserTools.text (\c -> c == ']') (\_ -> False)
        |> Parser.map (\_ -> RBR { begin = start, end = start })


textParser start =
    ParserTools.text (\c -> not <| List.member c (' ' :: l1LanguageChars)) (\c -> not <| List.member c (' ' :: l1LanguageChars))
        |> Parser.map (\data -> S data.content { begin = start, end = start + data.end - data.begin - 1 })


mathParser : Int -> TokenParser
mathParser start =
    ParserTools.textWithEndSymbol "$" (\c -> c == '$') (\c -> c /= '$')
        |> Parser.map (\data -> Math data.content { begin = start, end = start + data.end - data.begin - 1 })


codeParser : Int -> TokenParser
codeParser start =
    ParserTools.textWithEndSymbol "`$`" (\c -> c == '`') (\c -> c /= '`')
        |> Parser.map (\data -> Code data.content { begin = start, end = start + data.end - data.begin - 1 })


functionPartsParser : Int -> TokenParser
functionPartsParser start =
    ParserTools.textWithEndSymbol " " Char.isAlphaNum (\c -> c /= ' ')
        |> Parser.map (\data -> S data.content { begin = start, end = start + data.end - data.begin - 1 })
