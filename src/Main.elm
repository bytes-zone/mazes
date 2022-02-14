module Main exposing (..)

import Css
import Html as RootHtml
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Maze exposing (Maze)
import Random
import Svg.Styled as Svg


tenByTen : Maze
tenByTen =
    Maze.hexes
        { width = 10
        , height = 12
        , entrance = { row = 0, column = 0 }
        , exit = { row = 11, column = 9 }
        }


main : RootHtml.Html msg
main =
    Html.main_ []
        [ Html.h1 [] [ Html.text "Mazes!" ]
        , Html.h2 [] [ Html.text "Ungenerated" ]
        , Maze.view { cell = cellAttrs, wall = wallAttrs } tenByTen
        , Html.h2 [] [ Html.text "Generated" ]
        , Maze.view { cell = cellAttrs, wall = wallAttrs } (Maze.generate 0 99 tenByTen (Random.initialSeed 0))
        , Maze.view { cell = cellAttrs, wall = wallAttrs } (Maze.generate 0 99 tenByTen (Random.initialSeed 1))
        , Maze.view { cell = cellAttrs, wall = wallAttrs } (Maze.generate 0 99 tenByTen (Random.initialSeed 2))
        ]
        |> Html.toUnstyled


wallAttrs : List (Svg.Attribute msg)
wallAttrs =
    [ Attrs.stroke "#37474F"
    , Attrs.strokeWidth "2"
    ]


cellAttrs : Maze.Cell -> List (Svg.Attribute msg)
cellAttrs { role } =
    case role of
        Nothing ->
            [ Attrs.fill "#ECEFF1" ]

        Just Maze.Entrance ->
            [ Attrs.fill "#B2FF59" ]

        Just Maze.Exit ->
            [ Attrs.fill "#64FFDA" ]
