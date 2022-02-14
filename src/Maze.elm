module Maze exposing (Cell, Maze, Role(..), Wall, debugView, generate, hexes, squares, view)

import Dict
import Graph exposing (Graph)
import Html.Styled as Html exposing (Html)
import Random exposing (Generator)
import Set exposing (Set)
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as Attrs


squares :
    { width : Int
    , height : Int
    , entrance : { row : Int, column : Int }
    , exit : { row : Int, column : Int }
    }
    -> Maze
squares params =
    List.range 0 (params.width * params.height - 1)
        |> List.foldl
            (\id graph ->
                let
                    row =
                        id // params.width

                    column =
                        modBy params.width id
                in
                graph
                    |> Graph.insertNode id
                        { row = row
                        , column = column
                        , role =
                            if params.entrance.row == row && params.entrance.column == column then
                                Just Entrance

                            else if params.exit.row == row && params.exit.column == column then
                                Just Exit

                            else
                                Nothing
                        }
                    |> (if row + 1 < params.height then
                            Graph.insertEdge id (id + params.width) { wall = True }

                        else
                            identity
                       )
                    |> (if column + 1 < params.width then
                            Graph.insertEdge id (id + 1) { wall = True }

                        else
                            identity
                       )
            )
            Graph.empty
        |> Squares { width = params.width, height = params.height }


{-|

     1 2 3
      4 5 6
     7 8 9

-}
hexes :
    { width : Int
    , height : Int
    , entrance : { row : Int, column : Int }
    , exit : { row : Int, column : Int }
    }
    -> Maze
hexes params =
    List.range 0 (params.width * params.height - 1)
        |> List.foldl
            (\id graph ->
                let
                    row =
                        id // params.width

                    column =
                        modBy params.width id

                    toBottomLeft =
                        id + params.width - modBy 2 (row + 1)

                    toBottomRight =
                        id + params.width + modBy 2 row
                in
                graph
                    |> Graph.insertNode id
                        { row = row
                        , column = column
                        , role =
                            if params.entrance.row == row && params.entrance.column == column then
                                Just Entrance

                            else if params.exit.row == row && params.exit.column == column then
                                Just Exit

                            else
                                Nothing
                        }
                    |> (if column + 1 < params.width then
                            Graph.insertEdge id (id + 1) { wall = True }

                        else
                            identity
                       )
                    |> (if row + 1 < params.height && (column > 0 || modBy 2 row == 1) then
                            Graph.insertEdge id toBottomLeft { wall = True }

                        else
                            identity
                       )
                    |> (if row + 1 < params.height && (column + 1 < params.width || modBy 2 row == 0) then
                            Graph.insertEdge id toBottomRight { wall = True }

                        else
                            identity
                       )
            )
            Graph.empty
        |> Hexes { width = params.width, height = params.height }


type Role
    = Entrance
    | Exit


type alias Cell =
    { row : Int
    , column : Int
    , role : Maybe Role
    }


type alias Wall =
    { wall : Bool }


type Maze
    = Squares { width : Int, height : Int } (Graph Cell Wall)
    | Hexes { width : Int, height : Int } (Graph Cell Wall)


generate : Int -> Int -> Maze -> Random.Seed -> Maze
generate start end maze seed =
    case maze of
        Squares bounds squares_ ->
            Squares bounds (generateHelp [ start ] (Set.singleton start) end squares_ seed)

        Hexes bounds hexes_ ->
            Hexes bounds (generateHelp [ start ] (Set.singleton start) end hexes_ seed)


generateHelp : List Int -> Set Int -> Int -> Graph node Wall -> Random.Seed -> Graph node Wall
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


view : { cell : Cell -> List (Svg.Attribute msg), wall : List (Svg.Attribute msg) } -> Maze -> Html msg
view attrs maze =
    case maze of
        Squares bounds graph ->
            viewSquares attrs bounds graph

        Hexes bounds graph ->
            viewHexes attrs bounds graph


viewSquares : { cell : Cell -> List (Svg.Attribute msg), wall : List (Svg.Attribute msg) } -> { width : Int, height : Int } -> Graph Cell Wall -> Html msg
viewSquares attrs bounds graph =
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
                            (attrs.cell node
                                ++ [ Attrs.x (String.fromInt (node.column * squareSize))
                                   , Attrs.y (String.fromInt (node.row * squareSize))
                                   , Attrs.width (String.fromInt squareSize)
                                   , Attrs.height (String.fromInt squareSize)
                                   , Attrs.class ("id-" ++ String.fromInt id)
                                   ]
                            )
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
                                        (Attrs.class ("edge-" ++ String.fromInt id)
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


viewHexes : { cell : Cell -> List (Svg.Attribute msg), wall : List (Svg.Attribute msg) } -> { width : Int, height : Int } -> Graph Cell Wall -> Html msg
viewHexes attrs bounds graph =
    let
        hexRadius =
            25

        hexPoints =
            List.range 1 6
                |> List.map
                    (\i ->
                        ( hexRadius * sin (toFloat i * 2 * pi / 6)
                        , hexRadius * cos (toFloat i * 2 * pi / 6)
                        )
                    )

        ( hexWidth, hexHeight ) =
            hexPoints
                |> List.foldl
                    (\( x, y ) ( ( minX, maxX ), ( minY, maxY ) ) ->
                        ( ( min x minX
                          , max x maxX
                          )
                        , ( min y minY
                          , max y maxY
                          )
                        )
                    )
                    ( ( 0, 0 ), ( 0, 0 ) )
                |> (\( ( minX, maxX ), ( minY, maxY ) ) ->
                        ( maxX - minX, maxY - minY )
                   )

        hatHeight =
            case hexPoints of
                _ :: ( _, bl ) :: ( _, bot ) :: _ ->
                    abs bot - abs bl

                _ ->
                    0

        lines =
            case hexPoints of
                [ ( brX, brY ), ( trX, trY ), ( tX, tY ), ( tlX, tlY ), ( blX, blY ), ( bX, bY ) ] ->
                    { topRight =
                        Svg.line
                            (attrs.wall
                                ++ [ Attrs.x1 (String.fromFloat tX)
                                   , Attrs.y1 (String.fromFloat tY)
                                   , Attrs.x2 (String.fromFloat trX)
                                   , Attrs.y2 (String.fromFloat trY)
                                   ]
                            )
                            []
                    , right =
                        Svg.line
                            (attrs.wall
                                ++ [ Attrs.x1 (String.fromFloat trX)
                                   , Attrs.y1 (String.fromFloat trY)
                                   , Attrs.x2 (String.fromFloat brX)
                                   , Attrs.y2 (String.fromFloat brY)
                                   ]
                            )
                            []
                    , botRight =
                        Svg.line
                            (attrs.wall
                                ++ [ Attrs.x1 (String.fromFloat brX)
                                   , Attrs.y1 (String.fromFloat brY)
                                   , Attrs.x2 (String.fromFloat bX)
                                   , Attrs.y2 (String.fromFloat bY)
                                   ]
                            )
                            []
                    , botLeft =
                        Svg.line
                            (attrs.wall
                                ++ [ Attrs.x1 (String.fromFloat bX)
                                   , Attrs.y1 (String.fromFloat bY)
                                   , Attrs.x2 (String.fromFloat blX)
                                   , Attrs.y2 (String.fromFloat blY)
                                   ]
                            )
                            []
                    , left =
                        Svg.line
                            (attrs.wall
                                ++ [ Attrs.x1 (String.fromFloat blX)
                                   , Attrs.y1 (String.fromFloat blY)
                                   , Attrs.x2 (String.fromFloat tlX)
                                   , Attrs.y2 (String.fromFloat tlY)
                                   ]
                            )
                            []
                    , topLeft =
                        Svg.line
                            (attrs.wall
                                ++ [ Attrs.x1 (String.fromFloat tlX)
                                   , Attrs.y1 (String.fromFloat tlY)
                                   , Attrs.x2 (String.fromFloat tX)
                                   , Attrs.y2 (String.fromFloat tY)
                                   ]
                            )
                            []
                    }

                _ ->
                    { topRight = Svg.text "topRight"
                    , right = Svg.text "right"
                    , botRight = Svg.text "botRight"
                    , botLeft = Svg.text "botLeft"
                    , left = Svg.text "left"
                    , topLeft = Svg.text "topLeft"
                    }

        hexPointsAttr =
            hexPoints
                |> List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat y)
                |> String.join " "
                |> Attrs.points
    in
    Graph.nodes graph
        |> Dict.toList
        |> List.map
            (\( id, { row, column } as cell ) ->
                let
                    offsetX =
                        hexWidth * toFloat column + hexWidth / 2 + ((hexWidth / 2) * toFloat (modBy 2 row))

                    offsetY =
                        (hexHeight - hatHeight) * toFloat row + hexHeight / 2

                    walls =
                        Graph.neighbors id graph
                            |> Maybe.map Dict.toList
                            |> Maybe.withDefault []
                            |> List.filterMap
                                (\( otherId, { wall } ) ->
                                    if wall then
                                        Just otherId

                                    else
                                        Nothing
                                )
                            |> List.filterMap (\otherId -> Graph.node otherId graph)
                            |> List.foldl
                                {-
                                   1 2 3
                                    4 5 6
                                   7 8 9
                                -}
                                (\other acc ->
                                    { right = acc.right || (other.row == row && other.column > column)
                                    , botRight = acc.botRight || (other.row > row && other.column == column + modBy 2 row)
                                    , botLeft = acc.botLeft || (other.row > row && other.column == column - modBy 2 (row + 1))
                                    }
                                )
                                { right = False
                                , botRight = False
                                , botLeft = False
                                }
                in
                [ Just <| Svg.polygon (hexPointsAttr :: attrs.cell cell) []
                , if column == 0 then
                    Just lines.left

                  else
                    Nothing
                , if walls.botLeft || (column == 0 && modBy 2 row == 0) || row + 1 == bounds.height then
                    Just lines.botLeft

                  else
                    Nothing
                , if walls.botRight || (modBy 2 row == 1 && column + 1 == bounds.width) || row + 1 == bounds.height then
                    Just lines.botRight

                  else
                    Nothing
                , if walls.right || column + 1 == bounds.width then
                    Just lines.right

                  else
                    Nothing
                , if row == 0 || (column + 1 == bounds.width && modBy 2 row == 1) then
                    Just lines.topRight

                  else
                    Nothing
                , if row == 0 || (column == 0 && modBy 2 row == 0) then
                    Just lines.topLeft

                  else
                    Nothing
                ]
                    |> List.filterMap identity
                    |> Svg.g [ Attrs.transform ("translate(" ++ String.fromFloat offsetX ++ "," ++ String.fromFloat offsetY ++ ")") ]
            )
        |> Svg.svg
            [ Attrs.width "250"
            , Attrs.height "250"
            , Attrs.style "border: 1px solid black"
            , Attrs.viewBox <|
                "0 0 "
                    ++ String.fromFloat (toFloat bounds.width * hexWidth + hexWidth / 2)
                    ++ " "
                    ++ String.fromFloat (toFloat bounds.height * (hexHeight - hatHeight) + hatHeight)
            ]


debugView : Maze -> Html msg
debugView maze =
    let
        graph =
            case maze of
                Squares _ g ->
                    g

                Hexes _ g ->
                    g
    in
    Graph.nodes graph
        |> Dict.toList
        |> List.concatMap
            (\( id, node ) ->
                Graph.neighbors id graph
                    |> Maybe.map Dict.toList
                    |> Maybe.withDefault []
                    |> List.filterMap (\( otherId, edge ) -> Maybe.map (Tuple.pair ( otherId, edge )) (Graph.node otherId graph))
                    |> List.map (\stuff -> Html.dd [] [ Html.text (Debug.toString stuff) ])
                    |> (::) (Html.dt [] [ Html.text (String.fromInt id ++ " (" ++ Debug.toString node ++ ")") ])
            )
        |> Html.dl []
