module L0 exposing (AST, parse)

import Block.Block
import Tree exposing (Tree)
import Tree.BlocksV
import Tree.Build exposing (Error)


type alias AST =
    List (Tree Block.Block.L0BlockE)


isVerbatimLine : String -> Bool
isVerbatimLine str =
    String.left 2 str == "||"


parse : String -> AST
parse sourceText =
    sourceText
        |> Tree.BlocksV.fromStringAsParagraphs isVerbatimLine
        |> Tree.Build.forestFromBlocks Block.Block.l0Empty Block.Block.toL0BlockE Block.Block.toBlock
        |> Result.withDefault []
