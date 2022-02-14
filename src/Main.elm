module Main exposing (..)

import Css
import Html as RootHtml
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HAttrs exposing (css)
import Maze exposing (Maze)
import Random
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attrs


tenByTen : Maze
tenByTen =
    Maze.squares
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
        , Maze.view { cell = cellAttrs, wall = wallAttrs, container = containerAttrs } tenByTen
        , Html.h2 [] [ Html.text "Generated" ]
        , Maze.view { cell = cellAttrs, wall = wallAttrs, container = containerAttrs } (Maze.generate 0 99 tenByTen (Random.initialSeed 0))
        , Maze.view { cell = cellAttrs, wall = wallAttrs, container = containerAttrs } (Maze.generate 0 99 tenByTen (Random.initialSeed 1))
        , Maze.view { cell = cellAttrs, wall = wallAttrs, container = containerAttrs } (Maze.generate 0 99 tenByTen (Random.initialSeed 2))
        ]
        |> Html.toUnstyled


containerAttrs : List (Html.Attribute msg)
containerAttrs =
    [ HAttrs.style "max-width" "calc(33% - 5px)"
    , HAttrs.style "margin-right" "5px"
    ]


wallAttrs : List (Svg.Attribute msg)
wallAttrs =
    [ Attrs.stroke "#546E7A"
    , Attrs.strokeWidth "5"
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
