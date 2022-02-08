module Main exposing (..)

import Html as RootHtml
import Html.Styled as Html exposing (Html)
import Maze exposing (Maze)
import Random
import Svg.Styled as Svg


threeByThree : Maze { coords : ( Int, Int ) } { wall : Bool }
threeByThree =
    Maze.squares
        (\coords -> { coords = coords })
        { wall = True }
        { width = 3, height = 3 }


seed : Random.Seed
seed =
    Random.initialSeed 0


main : RootHtml.Html msg
main =
    Html.main_ []
        [ Html.h1 [] [ Html.text "Mazes!" ]
        , Html.h2 [] [ Html.text "Ungenerated" ]
        , Maze.view threeByThree
        , Html.h2 [] [ Html.text "Generated" ]
        , Maze.view (Maze.generate 1 9 threeByThree seed)
        ]
        |> Html.toUnstyled
