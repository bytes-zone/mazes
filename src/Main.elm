module Main exposing (..)

import Graph exposing (Graph)
import Html exposing (Html)
import Maze
import Random


{-| IDs are laid out like:

| 1 | 2 | 3 |
| 4 | 5 | 6 |
| 7 | 8 | 9 |

-}
threeByThree : Graph ( Int, Int ) { present : Bool }
threeByThree =
    Graph.empty
        |> Graph.setNode 1 ( 0, 0 )
        |> Graph.setNode 2 ( 0, 1 )
        |> Graph.setNode 3 ( 0, 2 )
        |> Graph.setNode 4 ( 1, 0 )
        |> Graph.setNode 5 ( 1, 1 )
        |> Graph.setNode 6 ( 1, 2 )
        |> Graph.setNode 7 ( 2, 0 )
        |> Graph.setNode 8 ( 2, 1 )
        |> Graph.setNode 9 ( 2, 2 )
        |> Graph.setEdge 1 2 { present = True }
        |> Graph.setEdge 1 4 { present = True }
        |> Graph.setEdge 2 3 { present = True }
        |> Graph.setEdge 2 5 { present = True }
        |> Graph.setEdge 3 6 { present = True }
        |> Graph.setEdge 4 7 { present = True }
        |> Graph.setEdge 4 5 { present = True }
        |> Graph.setEdge 5 6 { present = True }
        |> Graph.setEdge 5 8 { present = True }
        |> Graph.setEdge 7 8 { present = True }
        |> Graph.setEdge 8 9 { present = True }


seed : Random.Seed
seed =
    Random.initialSeed 0


main : Html msg
main =
    Html.main_ []
        [ Html.p [] [ Html.text (Debug.toString threeByThree) ]
        , Html.p [] [ Html.text (Debug.toString (Maze.generate 1 9 (Maze.custom threeByThree) seed)) ]
        ]
