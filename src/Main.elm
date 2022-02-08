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
    Maze.squares
        { row = -1, column = -1 }
        { wall = True }
        { width = 3, height = 3 }


main : RootHtml.Html msg
main =
    Html.main_ []
        [ Html.h1 [] [ Html.text "Mazes!" ]
        , Html.h2 [] [ Html.text "Ungenerated" ]
        , viewWithDebug tenByTen
        , Html.h2 [] [ Html.text "Generated" ]
        , viewWithDebug (Maze.generate 0 8 tenByTen (Random.initialSeed 0))
        , viewWithDebug (Maze.generate 0 8 tenByTen (Random.initialSeed 1))
        , viewWithDebug (Maze.generate 0 8 tenByTen (Random.initialSeed 2))
        ]
        |> Html.toUnstyled


viewWithDebug : Maze { row : Int, column : Int } { wall : Bool } -> Html msg
viewWithDebug maze =
    Html.section
        [ css
            [ Css.displayFlex
            , Css.justifyContent Css.spaceAround
            , Css.width (Css.px 800)
            ]
        ]
        [ Maze.view maze
        , Maze.debugView maze
        ]
