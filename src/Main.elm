module Main exposing (..)

import Html exposing (Html)
import Maze exposing (Maze)
import Random


threeByThree : Maze ( Int, Int ) { present : Bool }
threeByThree =
    Maze.squares identity { present = True } { width = 3, height = 3 }


seed : Random.Seed
seed =
    Random.initialSeed 0


main : Html msg
main =
    Html.main_ []
        [ Html.p [] [ Html.text (Debug.toString threeByThree) ]
        , Html.p [] [ Html.text (Debug.toString (Maze.generate 1 9 threeByThree seed)) ]
        ]
