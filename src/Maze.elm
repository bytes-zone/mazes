module Maze exposing (Maze, debugView, generate, hexes, squares, view)

import Dict
import Graph exposing (Graph)
import Html.Styled as Html exposing (Html)
import Random exposing (Generator)
import Set exposing (Set)
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as Attrs


squares :
    { node | row : Int, column : Int }
    -> { edge | wall : Bool }
    -> { width : Int, height : Int }
    -> Maze { node | row : Int, column : Int } { edge | wall : Bool }
squares initNode initEdge bounds =
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
                    |> List.map (\column -> { row = row, column = column })
            )
        |> List.indexedMap Tuple.pair
        -- create a graph out of 'em!
        |> List.foldl
            (\( id, { row, column } ) graph ->
                graph
                    |> Graph.insertNode id { initNode | row = row, column = column }
                    |> (if row + 1 <= height then
                            Graph.insertEdge id (id + width + 1) initEdge

                        else
                            identity
                       )
                    |> (if column + 1 <= width then
                            Graph.insertEdge id (id + 1) initEdge

                        else
                            identity
                       )
            )
            Graph.empty
        |> Squares bounds


hexes :
    { node | row : Int, column : Int }
    -> { edge | wall : Bool }
    -> { width : Int, height : Int }
    -> Maze { node | row : Int, column : Int } { edge | wall : Bool }
hexes initNode initEdge bounds =
    List.range 0 (bounds.width * bounds.height - 1)
        |> List.foldl
            (\id graph ->
                let
                    row =
                        id // bounds.width

                    column =
                        modBy bounds.width id
                in
                graph
                    |> Graph.insertNode id { initNode | row = row, column = column }
            )
            Graph.empty
        |> Debug.log "ids"
        |> Hexes bounds


type Maze node edge
    = Squares { width : Int, height : Int } (Graph node edge)
    | Hexes { width : Int, height : Int } (Graph node edge)


generate : Int -> Int -> Maze node { edge | wall : Bool } -> Random.Seed -> Maze node { edge | wall : Bool }
generate start end maze seed =
    case maze of
        Squares bounds squares_ ->
            Squares bounds (generateHelp [ start ] (Set.singleton start) end squares_ seed)

        Hexes bounds hexes_ ->
            Hexes bounds (generateHelp [ start ] (Set.singleton start) end hexes_ seed)


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


view : Maze { node | row : Int, column : Int } { edge | wall : Bool } -> Html msg
view maze =
    case maze of
        Squares bounds graph ->
            let
                squareSize =
                    50
            in
            Graph.nodes graph
                |> Dict.toList
                |> List.concatMap
                    (\( id, node ) ->
                        let
                            box =
                                Svg.rect
                                    [ Attrs.fill "#FCFCFC"

                                    -- attrs above here should eventually come in from a parameter
                                    , Attrs.x (String.fromInt (node.column * squareSize))
                                    , Attrs.y (String.fromInt (node.row * squareSize))
                                    , Attrs.width (String.fromInt squareSize)
                                    , Attrs.height (String.fromInt squareSize)
                                    , Attrs.class ("id-" ++ String.fromInt id)
                                    ]
                                    []

                            walls =
                                Graph.edgesFrom id graph
                                    |> Maybe.map Dict.toList
                                    |> Maybe.withDefault []
                                    |> List.filter (Tuple.second >> .wall)
                                    |> List.filterMap (\( toId, edge ) -> Maybe.map (Tuple.pair edge) (Graph.node toId graph))
                                    |> List.filter (\( _, other ) -> other.column > node.column || other.row > node.row)
                                    |> List.map
                                        (\( edge, other ) ->
                                            Svg.line
                                                (Attrs.stroke "black"
                                                    :: Attrs.strokeWidth "4"
                                                    -- attrs above here should eventually come in from a parameter
                                                    :: Attrs.class ("edge-" ++ String.fromInt id)
                                                    :: (if other.column > node.column then
                                                            -- line goes to the right of the node
                                                            [ Attrs.x1 (String.fromInt (node.column * squareSize + squareSize))
                                                            , Attrs.y1 (String.fromInt (node.row * squareSize))
                                                            , Attrs.x2 (String.fromInt (node.column * squareSize + squareSize))
                                                            , Attrs.y2 (String.fromInt (node.row * squareSize + squareSize))
                                                            , Attrs.class "right"
                                                            ]

                                                        else if other.row > node.row then
                                                            -- line goes below node
                                                            [ Attrs.x1 (String.fromInt (node.column * squareSize))
                                                            , Attrs.y1 (String.fromInt (node.row * squareSize + squareSize))
                                                            , Attrs.x2 (String.fromInt (node.column * squareSize + squareSize))
                                                            , Attrs.y2 (String.fromInt (node.row * squareSize + squareSize))
                                                            , Attrs.class "bottom"
                                                            ]

                                                        else
                                                            -- invalid but we should have
                                                            -- already removed any nodes
                                                            -- with any other conditions
                                                            Debug.todo "another condition"
                                                       )
                                                )
                                                []
                                        )
                        in
                        box :: walls
                    )
                |> Svg.svg
                    [ Attrs.width "250"
                    , Attrs.height "250"
                    , Attrs.style "border: 1px solid black"

                    -- things above this should eventually end up in a passed-in attribute.
                    , Attrs.viewBox <|
                        "0 0 "
                            ++ String.fromInt (bounds.width * squareSize)
                            ++ " "
                            ++ String.fromInt (bounds.height * squareSize)
                    ]

        Hexes _ _ ->
            Html.text "TODO"


debugView : Maze { node | row : Int, column : Int } { edge | wall : Bool } -> Html msg
debugView maze =
    case maze of
        Squares bounds graph ->
            Graph.nodes graph
                |> Dict.toList
                |> List.concatMap
                    (\( id, node ) ->
                        Graph.neighbors id graph
                            |> Maybe.map Dict.toList
                            |> Maybe.withDefault []
                            |> List.filterMap (\( otherId, edge ) -> Maybe.map (Tuple.pair ( otherId, edge )) (Graph.node otherId graph))
                            -- |> List.filter (\( _, other ) -> other.column > node.column || other.row > node.row)
                            |> List.map (\stuff -> Html.dd [] [ Html.text (Debug.toString stuff) ])
                            |> (::) (Html.dt [] [ Html.text (String.fromInt id ++ " (" ++ Debug.toString node ++ ")") ])
                    )
                |> Html.dl []

        Hexes _ _ ->
            Html.text "TODO"
