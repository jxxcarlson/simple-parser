module L0 exposing (..)

import Element exposing (Element)
import Parser.Expression
import Render.Elm
import Render.Msg exposing (MarkupMsg)
import Render.Settings exposing (Settings)


renderFromString : Int -> Settings -> String -> List (Element MarkupMsg)
renderFromString count settings sourceText =
    sourceText
        |> Parser.Expression.parse_
        |> List.map (\expr -> Render.Elm.render 0 settings expr)



-- |> List.map (Element.map Render)
