module Maze exposing (Maze, build)

import Dict
import Graph exposing (Graph)
import Random exposing (Generator)


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

        whereWeAre :: whereWeWere ->
            if whereWeAre == end then
                buildHelp whereWeWere end graph

            else
                let
                    possibilities =
                        Graph.neighbors whereWeAre graph
                            |> Maybe.withDefault Dict.empty
                            |> Dict.toList
                            |> List.filter Tuple.second
                            |> List.map Tuple.first
                in
                case possibilities of
                    [] ->
                        buildHelp whereWeWere end graph

                    first :: rest ->
                        -- TODO: make this stack-safe if it turns out to be a problem
                        Random.uniform first rest
                            |> Random.andThen
                                (\next ->
                                    buildHelp
                                        (next :: stack)
                                        end
                                        (Graph.setEdge whereWeAre next False graph)
                                )
