module Maze exposing (Maze, build)

import Graph exposing (Graph)
import Random exposing (Generator)
import Set exposing (Set)


type Maze node
    = Built (Graph node Bool)


build : Int -> Int -> Graph node Bool -> Generator (Maze node)
build start end graph =
    buildHelp [ start ] end graph


buildHelp : List Int -> Int -> Graph node Bool -> Generator (Maze node)
buildHelp stack end graph =
    case stack of
        [] ->
            Debug.todo "we failed to generate a maze, but it's not reflected in the types"

        here :: _ ->
            Debug.todo "buildHelp"
