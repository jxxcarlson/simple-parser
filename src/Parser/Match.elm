module Parser.Match exposing (..)


type Symbol
    = L
    | R
    | O


symbolToString : Symbol -> String
symbolToString symbol =
    case symbol of
        L ->
            "L"

        R ->
            "R"

        O ->
            "O"


symbolsToString : List Symbol -> String
symbolsToString symbols =
    List.map symbolToString symbols |> String.join " "


symbolIndex : Symbol -> Int
symbolIndex symbol =
    case symbol of
        L ->
            1

        R ->
            -1

        O ->
            0


reducible : List Symbol -> Bool
reducible symbols =
    case List.head symbols of
        Nothing ->
            True

        Just R ->
            False

        Just O ->
            False

        Just L ->
            case match symbols of
                Nothing ->
                    False

                Just k ->
                    reducible (List.drop 1 (deleteAt k symbols))


{-|

> deleteAt 1 [0, 1, 2]

     [0,2] : List number

-}
deleteAt : Int -> List a -> List a
deleteAt k list =
    List.take k list ++ List.drop (k + 1) list


{-|

    > splitAt 2 [0, 1, 2, 3, 4]
      ([0,1],[3,4])

-}
splitAt : Int -> List a -> ( List a, List a )
splitAt k list =
    ( List.take k list, List.drop (k + 0) list )


type alias State =
    { symbols : List Symbol, index : Int, brackets : Int }


match : List Symbol -> Maybe Int
match symbols =
    case List.head symbols of
        Nothing ->
            Nothing

        Just symbol ->
            if symbolIndex symbol < 0 then
                Nothing

            else
                loop { symbols = List.drop 1 symbols, index = 1, brackets = symbolIndex symbol } nextStep


nextStep : State -> Step State (Maybe Int)
nextStep state =
    case List.head state.symbols of
        Nothing ->
            Done Nothing

        Just sym ->
            let
                brackets =
                    state.brackets + symbolIndex sym
            in
            if brackets < 0 then
                Done Nothing

            else if brackets == 0 then
                Done (Just state.index)

            else
                Loop { symbols = List.drop 1 state.symbols, index = state.index + 1, brackets = brackets }


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
