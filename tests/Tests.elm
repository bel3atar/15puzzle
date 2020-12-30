module Tests exposing (..)

import Board exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


tilesToString : Tiles -> String
tilesToString tiles =
    "[" ++ (String.join ", " <| List.map String.fromInt tiles) ++ "]"



{-
   test1 board of size 3
   +---+---+---+
   | 1 | 2 | 3 |
   +---+---+---+
   | 4 |   | 5 |
   +---+---+---+
   | 6 | 7 | 8 |
   +---+---+---+
-}


validMoves : Test
validMoves =
    let
        suite =
            [ ( 5, [ 1, 2, 3, 4, 5, 0, 6, 7, 8 ] )
            , ( 7, [ 1, 2, 3, 4, 7, 5, 6, 0, 8 ] )
            , ( 4, [ 1, 2, 3, 0, 4, 5, 6, 7, 8 ] )
            , ( 2, [ 1, 0, 3, 4, 2, 5, 6, 7, 8 ] )
            ]

        makeExpectation : ( Tile, Tiles ) -> Expectation
        makeExpectation ( tile, tiles ) =
            let
                board =
                    Board 3 [ 1, 2, 3, 4, 0, 5, 6, 7, 8 ]
            in
            move tile board |> .tiles |> Expect.equal tiles

        makeTest : ( Tile, Tiles ) -> Test
        makeTest (( tile, tiles ) as testCase) =
            let
                tileString =
                    String.fromInt tile
            in
            test tileString (\_ -> makeExpectation testCase)
    in
    describe "Move when it's possible" (suite |> List.map makeTest)


type alias Impossible =
    { tiles : Tiles, unmovables : Tiles }


invalidMoves : Test
invalidMoves =
    let
        suite =
            [ Impossible [ 0, 1, 2, 3 ] [ 3 ] -- 3 can't move here
            , Impossible [ 1, 0, 2, 3 ] [ 2 ] -- 2 can't move here
            , Impossible [ 1, 2, 0, 3 ] [ 2 ] -- … and so on
            , Impossible [ 1, 2, 3, 0 ] [ 1 ]
            ]

        makeExpectation : Tiles -> Tile -> Expectation
        makeExpectation tiles unmovable =
            let
                board =
                    Board 2 tiles
            in
            move unmovable board |> .tiles |> Expect.equal tiles

        mapImpossibleToTests : Impossible -> List Test
        mapImpossibleToTests ({ tiles, unmovables } as impossibleCase) =
            let
                mapUnmovableToTest : Tile -> Test
                mapUnmovableToTest unmovableTile =
                    let
                        testDescription =
                            String.fromInt unmovableTile ++ " with " ++ tilesToString tiles
                    in
                    test testDescription (\_ -> makeExpectation tiles unmovableTile)
            in
            List.map mapUnmovableToTest unmovables
    in
    describe "No change if move isn't possible" (List.concat <| List.map mapImpossibleToTests suite)
