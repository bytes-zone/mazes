module Maze exposing (Maze, generate, squares, view)

import Dict
import Graph exposing (Graph)
import Html.Styled as Html exposing (Html)
import Random exposing (Generator)
import Set exposing (Set)
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as Attrs


squares : (( Int, Int ) -> node) -> { edge | wall : Bool } -> { width : Int, height : Int } -> Maze node { edge | wall : Bool }
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


type Maze node edge
    = Squares (Graph node edge)


generate : Int -> Int -> Maze node { edge | wall : Bool } -> Random.Seed -> Maze node { edge | wall : Bool }
generate start end maze seed =
    case maze of
        Squares squares_ ->
            Squares (generateHelp [ start ] Set.empty end squares_ seed)


generateHelp : List Int -> Set Int -> Int -> Graph node { edge | wall : Bool } -> Random.Seed -> Graph node { edge | wall : Bool }
generateHelp stack visited end graph seed =
    case stack of
        [] ->
            graph

        whereWeAre :: whereWeWere ->
            if whereWeAre == end then
                generateHelp
                    whereWeWere
                    (Set.insert end visited)
                    end
                    graph
                    seed

            else
                let
                    possibilities =
                        Graph.neighbors whereWeAre graph
                            |> Maybe.withDefault Dict.empty
                            |> Dict.toList
                            |> List.filter (\( id, _ ) -> not (Set.member id visited))
                            |> List.map Tuple.first
                in
                case possibilities of
                    [] ->
                        -- if there are no possibilities, we need to pop from
                        -- the stack until we get to a node with possibilities
                        -- or return to the start
                        generateHelp whereWeWere visited end graph seed

                    first :: rest ->
                        let
                            ( whereWeAreGoing, nextSeed ) =
                                Random.step (Random.uniform first rest) seed
                        in
                        generateHelp
                            (whereWeAreGoing :: stack)
                            (Set.insert whereWeAreGoing visited)
                            end
                            (Graph.updateEdge
                                whereWeAre
                                whereWeAreGoing
                                (Maybe.map (\edge -> { edge | wall = False }))
                                graph
                            )
                            nextSeed


view : Maze { node | coords : ( Int, Int ) } { edge | wall : Bool } -> Html msg
view _ =
    Svg.svg
        [ Attrs.width "250"
        , Attrs.height "250"
        , Attrs.viewBox "0 0 1000 1000"
        , Attrs.style "border: 1px solid black"
        ]
        [ Svg.rect
            [ Attrs.x "0"
            , Attrs.y "0"
            , Attrs.width "100"
            , Attrs.height "100"
            , Attrs.fill "black"
            ]
            []
        ]
