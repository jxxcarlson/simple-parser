module L0 exposing (..)

import Element exposing (Element)
import Parser.Expression
import Render.Elm
import Render.Msg exposing (MarkupMsg)
import Render.Settings exposing (Settings)
import Tree.Blocks


renderFromString : Int -> Settings -> String -> List (Element MarkupMsg)
renderFromString count settings sourceText =
    sourceText
        |> Tree.Blocks.fromStringAsParagraphs
        |> Debug.log "PARAGRAPHS"
        |> List.map (.content >> Parser.Expression.parse_)
        |> List.map (List.map (\expr -> Render.Elm.render count settings expr))
        |> List.map (\x -> Element.paragraph [] x)
