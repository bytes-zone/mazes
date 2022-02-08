module Maze exposing (Maze, custom, generate, squares)

import Dict
import Graph exposing (Graph)
import Random exposing (Generator)


squares : (( Int, Int ) -> node) -> { edge | present : Bool } -> { width : Int, height : Int } -> Maze node { edge | present : Bool }
squares makeNode initEdge bounds =
    let
        width =
            bounds.width - 1

        height =
            bounds.height - 1
    in
    -- make the coords and their IDs
    List.range 0 height
        |> List.concatMap
            (\row ->
                List.range 0 width
                    |> List.map (\col -> ( col, row ))
            )
        |> List.indexedMap Tuple.pair
        -- create a graph out of 'em!
        |> List.foldl
            (\( id, ( col, row ) ) graph ->
                graph
                    |> Graph.setNode id (makeNode ( col, row ))
                    |> (if row + 1 <= height then
                            Graph.setEdge id (id + width + 1) initEdge

                        else
                            identity
                       )
                    |> (if col + 1 <= width then
                            Graph.setEdge id (id + 1) initEdge

                        else
                            identity
                       )
            )
            Graph.empty
        |> Squares


custom : Graph node { edge | present : Bool } -> Maze node { edge | present : Bool }
custom =
    Maze


type Maze node edge
    = Maze (Graph node edge)
    | Squares (Graph node edge)


generate : Int -> Int -> Maze node { edge | present : Bool } -> Random.Seed -> Maze node { edge | present : Bool }
generate start end maze seed =
    case maze of
        Maze custom_ ->
            Maze (generateHelp [ start ] end custom_ seed)

        Squares squares_ ->
            Squares (generateHelp [ start ] end squares_ seed)


generateHelp : List Int -> Int -> Graph node { edge | present : Bool } -> Random.Seed -> Graph node { edge | present : Bool }
generateHelp stack end graph seed =
    case stack of
        [] ->
            graph

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
