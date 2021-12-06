module Render.Block exposing (render)

import Block.Block exposing (BlockType(..), L0BlockE(..))
import Either exposing (Either(..))
import Element exposing (Element)
import Render.Elm
import Render.Math exposing (DisplayMode(..))
import Render.Msg exposing (MarkupMsg)
import Render.Settings exposing (Settings)


render : Int -> Settings -> L0BlockE -> Element MarkupMsg
render count settings (L0BlockE { name, args, indent, blockType, content, children }) =
    case blockType of
        Paragraph ->
            case content of
                Right exprs ->
                    List.map (Render.Elm.render count settings) exprs |> (\x -> Element.paragraph [] x)

                Left _ ->
                    Element.none

        VerbatimBlock _ ->
            case content of
                Right _ ->
                    Element.none

                Left str ->
                    renderVerbatim count (name |> Maybe.withDefault "unknown name") args str

        _ ->
            Element.none


renderVerbatim count name args str =
    if name == "math" then
        Render.Math.mathText count "id" DisplayMathMode (removeFirstLine str)

    else
        Element.none


removeFirstLine : String -> String
removeFirstLine str =
    str |> String.trim |> String.lines |> List.drop 1 |> String.join "\n"
