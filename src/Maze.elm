module Maze exposing (Maze, build)

import Dict
import Graph exposing (Graph)
import Random exposing (Generator)


type Maze node
    = Built (Graph node Bool)


build : Int -> Int -> Graph node Bool -> Random.Seed -> Maze node
build start end graph seed =
    buildHelp [ start ] end graph seed


buildHelp : List Int -> Int -> Graph node Bool -> Random.Seed -> Maze node
buildHelp stack end graph seed =
    case stack of
        [] ->
            Built graph

        whereWeAre :: whereWeWere ->
            if whereWeAre == end then
                buildHelp whereWeWere end graph seed

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
                        buildHelp whereWeWere end graph seed

                    first :: rest ->
                        let
                            ( whereWeAreGoing, nextSeed ) =
                                Random.step (Random.uniform first rest) seed
                        in
                        buildHelp
                            (whereWeAreGoing :: stack)
                            end
                            (Graph.setEdge whereWeAre whereWeAreGoing False graph)
                            nextSeed
