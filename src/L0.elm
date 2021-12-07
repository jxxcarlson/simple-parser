module L0 exposing (..)

import Block.Block
import Element exposing (Element)
import Render.Block
import Render.Msg exposing (MarkupMsg)
import Render.Settings exposing (Settings)
import Tree.BlocksV


renderFromString : Int -> Settings -> String -> List (Element MarkupMsg)
renderFromString count settings sourceText =
    sourceText
        |> Tree.BlocksV.fromStringAsParagraphs isVerbatimLine
        |> List.map Block.Block.toL0BlockE
        |> List.map (Render.Block.render count settings)


isVerbatimLine : String -> Bool
isVerbatimLine str =
    String.left 2 str == "||"
