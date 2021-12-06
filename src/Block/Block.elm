module Block.Block exposing (BlockType(..), L0BlockE(..), toL0Block, toL0BlockE)

import Either exposing (Either(..))
import Parser.Expression exposing (Expr(..))
import Tree.Blocks exposing (Block)


type L0Block
    = L0Block
        { name : Maybe String
        , args : List String
        , indent : Int
        , blockType : BlockType
        , content : String
        , children : List L0Block
        }


type L0BlockE
    = L0BlockE
        { name : Maybe String
        , args : List String
        , indent : Int
        , blockType : BlockType
        , content : Either String (List Expr)
        , children : List L0BlockE
        }


toL0BlockE : Block -> L0BlockE
toL0BlockE block =
    let
        blockType =
            classify block
    in
    case blockType of
        Paragraph ->
            L0BlockE
                { name = Nothing
                , args = []
                , indent = block.indent
                , content = Right (Parser.Expression.parse_ block.content)
                , blockType = blockType
                , children = []
                }

        OrdinaryBlock args ->
            L0BlockE
                { name = List.head args
                , args = List.drop 1 args
                , indent = block.indent
                , content = Right (Parser.Expression.parse_ block.content)
                , blockType = blockType
                , children = []
                }

        VerbatimBlock args ->
            L0BlockE
                { name = List.head args
                , args = List.drop 1 args
                , indent = block.indent
                , content = Left block.content
                , blockType = blockType
                , children = []
                }


toL0Block : Block -> L0Block
toL0Block block =
    let
        blockType =
            classify block
    in
    case blockType of
        Paragraph ->
            L0Block
                { name = Nothing
                , args = []
                , indent = block.indent
                , content = block.content
                , blockType = blockType
                , children = []
                }

        OrdinaryBlock args ->
            L0Block
                { name = List.head args
                , args = List.drop 1 args
                , indent = block.indent
                , content = block.content
                , blockType = blockType
                , children = []
                }

        VerbatimBlock args ->
            L0Block
                { name = List.head args
                , args = List.drop 1 args
                , indent = block.indent
                , content = block.content
                , blockType = blockType
                , children = []
                }


type BlockType
    = Paragraph
    | OrdinaryBlock (List String)
    | VerbatimBlock (List String)


classify : Block -> BlockType
classify block =
    let
        str_ =
            String.trim block.content
    in
    if String.left 2 str_ == "||" then
        VerbatimBlock (str_ |> String.lines |> List.head |> Maybe.withDefault "" |> String.words |> List.drop 1)

    else if String.left 1 str_ == "|" then
        OrdinaryBlock (str_ |> String.lines |> List.head |> Maybe.withDefault "" |> String.words |> List.drop 1)

    else if String.left 2 str_ == "$$" then
        VerbatimBlock [ "math" ]

    else
        Paragraph


a =
    """
one
two
three
"""


b =
    """
|| a b c
one
two
three
"""


c =
    """
| a b c
one
two
three
"""


test : String -> List BlockType
test s =
    s
        |> Tree.Blocks.fromStringAsParagraphs
        |> Debug.log "BLOCKS"
        |> List.map classify


test2 : String -> List L0Block
test2 s =
    s
        |> Tree.Blocks.fromStringAsParagraphs
        |> Debug.log "BLOCKS"
        |> List.map toL0Block


str =
    [ a, b, c ] |> String.join "\n\n"


testResult =
    test str


testResult2 =
    test2 str
