module Maze exposing (Maze, build)

import Graph exposing (Graph)
import Random exposing (Generator)
import Set exposing (Set)


type Maze node
    = Built (Graph node Bool)


build : Int -> Int -> Graph node Bool -> Generator (Maze node)
build start end graph =
    Debug.todo "build"
