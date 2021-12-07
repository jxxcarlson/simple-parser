module Render.Block exposing (render)

import Block.Block exposing (BlockType(..), L0BlockE(..))
import Dict exposing (Dict)
import Either exposing (Either(..))
import Element exposing (Element)
import Element.Font as Font
import Parser.Expression exposing (Expr)
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
                    case name of
                        Nothing ->
                            Element.none

                        Just functionName ->
                            case Dict.get functionName verbatimDict of
                                Nothing ->
                                    noSuchBlock functionName str

                                Just f ->
                                    f count args str

        OrdinaryBlock _ ->
            case content of
                Left _ ->
                    Element.none

                Right exprs ->
                    case name of
                        Nothing ->
                            Element.none

                        Just functionName ->
                            case Dict.get functionName blockDict of
                                Nothing ->
                                    noSuchOrdinaryBlock count settings functionName exprs

                                Just f ->
                                    f count settings args exprs


noSuchBlock : String -> String -> Element MarkupMsg
noSuchBlock functionName content =
    Element.column []
        (Element.el [ Font.color (Element.rgb255 180 0 0) ] (Element.text <| "No such block or misspelling of " ++ "\"" ++ functionName ++ "\":")
            :: List.map (\t -> Element.el [] (Element.text t)) (String.lines content)
        )


noSuchOrdinaryBlock : Int -> Settings -> String -> List Expr -> Element MarkupMsg
noSuchOrdinaryBlock count settings functionName exprs =
    Element.column []
        (Element.el [ Font.color (Element.rgb255 180 0 0) ] (Element.text <| "No such block or misspelling of " ++ "\"" ++ functionName ++ "\":")
            :: List.map (Render.Elm.render count settings) exprs
        )


blockDict : Dict String (Int -> Settings -> List String -> List Expr -> Element MarkupMsg)
blockDict =
    Dict.fromList
        [ ( "indent", \count args exprs -> indented count args exprs ) ]


indented count settings args exprs =
    Element.paragraph [ Element.paddingEach { left = 36, right = 0, top = 0, bottom = 0 } ]
        (List.map (Render.Elm.render count settings) exprs)


verbatimDict : Dict String (Int -> List String -> String -> Element MarkupMsg)
verbatimDict =
    Dict.fromList
        [ ( "math", renderDisplayMath )
        , ( "code", renderCode )
        ]


renderDisplayMath count args str =
    Render.Math.mathText count "id" DisplayMathMode (removeFirstLine str)


renderCode count args str =
    Element.column
        [ Font.color (Element.rgb255 170 0 250)
        , Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Element.spacing 8
        , Element.paddingEach { left = 24, right = 0, top = 0, bottom = 0 }
        ]
        (List.map (\t -> Element.el [] (Element.text t)) (List.drop 1 <| String.lines (String.trim str)))


removeFirstLine : String -> String
removeFirstLine str =
    str |> String.trim |> String.lines |> List.drop 1 |> String.join "\n"
