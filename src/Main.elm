module Main exposing (..)

import Css
import Html as RootHtml
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Maze exposing (Maze)
import Random
import Svg.Styled as Svg


tenByTen : Maze { row : Int, column : Int } { wall : Bool }
tenByTen =
    Maze.hexes
        { row = -1, column = -1 }
        { wall = True }
        { width = 10, height = 12 }


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
