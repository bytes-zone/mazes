module Maze exposing (Graph, Maze, build)

import Random exposing (Generator)


type alias Graph =
    { start : Int
    , end : Int
    , edges : List ( Int, Int )
    }


type Maze
    = Built Graph


build : Graph -> Generator Maze
build graph =
    Debug.todo "build"
