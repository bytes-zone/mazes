module Maze exposing (Maze, build)

import Graph exposing (Graph)
import Random exposing (Generator)
import Set exposing (Set)


type Maze node
    = Built (Graph node Bool)


build : Graph node Bool -> Generator (Maze node)
build graph =
    Debug.todo "build"
