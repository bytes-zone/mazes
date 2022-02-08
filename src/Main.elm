module Main exposing (..)

import Html as RootHtml
import Html.Styled as Html exposing (Html)
import Maze exposing (Maze)
import Random
import Svg.Styled as Svg


tenByTen : Maze { coords : ( Int, Int ) } { wall : Bool }
tenByTen =
    Maze.squares
        (\coords -> { coords = coords })
        { wall = True }
        { width = 10, height = 10 }


main : RootHtml.Html msg
main =
    Html.main_ []
        [ Html.h1 [] [ Html.text "Mazes!" ]
        , Html.h2 [] [ Html.text "Ungenerated" ]
        , Maze.view tenByTen
        , Html.h2 [] [ Html.text "Generated" ]
        , Maze.view (Maze.generate 0 99 tenByTen (Random.initialSeed 0))
        , Maze.view (Maze.generate 0 99 tenByTen (Random.initialSeed 1))
        , Maze.view (Maze.generate 0 99 tenByTen (Random.initialSeed 2))
        ]
        |> Html.toUnstyled
