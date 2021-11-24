module Block.Block exposing (..)

import Block.Line exposing (LineType(..))


type Block
    = Block
        { name : Maybe String
        , content : String
        , children : List Block
        }


type alias State =
    { block : List Block
    , currentBlock : Block
    , lines : List String
    , indent : Int
    , previousLineType : LineType
    }
