module Render.Settings exposing (Settings, maxHeadingFontSize)


type alias Settings =
    { paragraphSpacing : Int
    , selectedId : String
    , showErrorMessages : Bool
    , showTOC : Bool
    , titleSize : Int
    , width : Int
    }


maxHeadingFontSize : Float
maxHeadingFontSize =
    32
