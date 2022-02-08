module Maze exposing (Maze, custom, generate)

import Dict
import Graph exposing (Graph)
import Random exposing (Generator)





custom : Graph node { edge | present : Bool } -> Maze node edge
custom =
    Maze


type Maze node edge
    = Maze (Graph node { edge | present : Bool })


generate : Int -> Int -> Maze node edge -> Random.Seed -> Maze node edge
generate start end (Maze graph) seed =
    generateHelp [ start ] end graph seed


generateHelp : List Int -> Int -> Graph node { edge | present : Bool } -> Random.Seed -> Maze node edge
generateHelp stack end graph seed =
    case stack of
        [] ->
            Maze graph

        whereWeAre :: whereWeWere ->
            if whereWeAre == end then
                generateHelp whereWeWere end graph seed

            else
                let
                    possibilities =
                        Graph.neighbors whereWeAre graph
                            |> Maybe.withDefault Dict.empty
                            |> Dict.toList
                            |> List.filter (Tuple.second >> .present)
                            |> List.map Tuple.first
                in
                case possibilities of
                    [] ->
                        generateHelp whereWeWere end graph seed

                    first :: rest ->
                        let
                            ( whereWeAreGoing, nextSeed ) =
                                Random.step (Random.uniform first rest) seed
                        in
                        generateHelp
                            (whereWeAreGoing :: stack)
                            end
                            (Graph.updateEdge
                                whereWeAre
                                whereWeAreGoing
                                (Maybe.map (\edge -> { edge | present = False }))
                                graph
                            )
                            nextSeed
