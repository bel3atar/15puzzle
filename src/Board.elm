module Board exposing (Board, Direction(..), Tile, Tiles, move, new, random)

import List.Extra exposing (..)
import Maybe.Extra exposing (..)
import Random exposing (Generator)
import Random.List


type Direction
    = Up
    | Down
    | Left
    | Right
    | None


type alias Tile =
    Int


type alias Tiles =
    List Tile


type alias Board =
    { size : Int, tiles : Tiles }


new : Int -> Board
new size =
    Board size <| List.range 0 (size ^ 2 - 1)


random : Int -> Generator Board
random size =
    let
        shuffledTiles : Generator Tiles
        shuffledTiles =
            Random.List.shuffle <| List.range 0 (size ^ 2 - 1)

        tilesToBoard : Tiles -> Board
        tilesToBoard tiles =
            Board size tiles
    in
    Random.map tilesToBoard shuffledTiles


calculateDirection : Tile -> Tile -> Board -> Direction
calculateDirection tile indexOfEmptySpot ({ tiles, size } as board) =
    let
        indexOfTile =
            unwrap 0 identity <| elemIndex tile tiles

        isTileBeforeEmptySpot =
            indexOfTile < indexOfEmptySpot

        distanceBetweenTheTwo =
            abs <| indexOfTile - indexOfEmptySpot
    in
    if distanceBetweenTheTwo == 0 then
        None

    else
        case isTileBeforeEmptySpot of
            True ->
                if distanceBetweenTheTwo == 1 then
                    Left

                else if distanceBetweenTheTwo == size then
                    Up

                else
                    None

            False ->
                if distanceBetweenTheTwo == 1 then
                    Right

                else if distanceBetweenTheTwo == size then
                    Down

                else
                    None


move : Tile -> Board -> Board
move tile ({ size, tiles } as board) =
    let
        indexOfEmptySpot =
            unwrap 0 identity <| elemIndex 0 tiles

        direction =
            calculateDirection tile indexOfEmptySpot board

        isMovePossible =
            case direction of
                None ->
                    False

                Up ->
                    let
                        destination =
                            indexOfEmptySpot - size
                    in
                    destination >= 0

                Down ->
                    let
                        destination =
                            indexOfEmptySpot + size
                    in
                    destination < size ^ 2

                _ ->
                    let
                        mod =
                            indexOfEmptySpot |> modBy size
                    in
                    if direction == Left then
                        mod > 0

                    else
                        mod < size - 1
    in
    if isMovePossible then
        { board
            | tiles =
                let
                    swapDestination =
                        case direction of
                            Up ->
                                indexOfEmptySpot - size

                            Down ->
                                indexOfEmptySpot + size

                            Left ->
                                indexOfEmptySpot - 1

                            Right ->
                                indexOfEmptySpot + 1

                            {- impossible case -}
                            _ ->
                                -1
                in
                swapAt swapDestination indexOfEmptySpot tiles
        }

    else
        board
