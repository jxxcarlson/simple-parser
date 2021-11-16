module Token exposing (..)

import Parser.Advanced as Parser exposing (DeadEnd, Parser)
import ParserTools exposing (Context, Problem)


type Token
    = LBR Loc
    | RBR Loc
    | F String Loc
    | T String Loc
    | W String Loc
    | Math String Loc
    | Code String Loc
    | TokenError (List (DeadEnd Context Problem)) Loc


type SimpleToken
    = LBRS
    | RBRS
    | FS String
    | TS String
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

        F str _ ->
            FS str

        T str _ ->
            TS str

        W str _ ->
            WS str

        Math str _ ->
            MathS str

        Code str _ ->
            CodeS str

        TokenError list _ ->
            TokenErrorS list


length : Token -> Int
length token =
    case token of
        LBR _ ->
            1

        RBR _ ->
            1

        F str _ ->
            String.length str

        T str _ ->
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
    { source : String, scanpointer : Int, sourceLength : Int, tokens : List a }


init : String -> State a
init str =
    { source = str, scanpointer = 0, sourceLength = String.length str, tokens = [] }


type alias TokenParser =
    Parser Context Problem Token


run : String -> List Token
run source =
    loop (init source) nextStep


runS : String -> List SimpleToken
runS source =
    loop (init source) nextStep |> List.map simplify


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
        Done (List.reverse state.tokens)

    else
        let
            token =
                get state state.scanpointer (String.dropLeft state.scanpointer state.source)

            newScanPointer =
                state.scanpointer + length token
        in
        Loop { state | tokens = token :: state.tokens, scanpointer = newScanPointer }


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
        |> Parser.map (\data -> T data.content { begin = start, end = start + data.end - data.begin - 1 })


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
        |> Parser.map (\data -> F data.content { begin = start, end = start + data.end - data.begin - 1 })
