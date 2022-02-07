module Maze exposing (Maze, build)

import Graph exposing (Graph)
import Random exposing (Generator)
import Set exposing (Set)


type Maze node
    = Built (Graph node Bool)


build : Int -> Int -> Graph node Bool -> Generator (Maze node)
build =
    buildHelp []


buildHelp : List Int -> Int -> Int -> Graph node Bool -> Generator (Maze node)
buildHelp stack start end graph =
    Debug.todo "buildHelp"
