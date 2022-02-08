module Graph exposing (Graph, empty, neighbors, node, setEdge, setNode, updateEdge)

import Dict exposing (Dict)


{-| An undirected graph
-}
type Graph node edge
    = Graph
        { nodes : Dict Int node
        , edges : Dict Int (Dict Int edge)
        }


empty : Graph node edge
empty =
    Graph { nodes = Dict.empty, edges = Dict.empty }


setNode : Int -> node -> Graph node edge -> Graph node edge
setNode id data (Graph guts) =
    Graph { guts | nodes = Dict.insert id data guts.nodes }


setEdge : Int -> Int -> edge -> Graph node edge -> Graph node edge
setEdge a b data graph =
    updateEdge a b (\_ -> Just data) graph


updateEdge : Int -> Int -> (Maybe edge -> Maybe edge) -> Graph node edge -> Graph node edge
updateEdge a b updater (Graph guts) =
    Graph
        { guts
            | edges =
                guts.edges
                    |> updateEdgeHelp a b updater
                    |> updateEdgeHelp b a updater
        }


updateEdgeHelp : Int -> Int -> (Maybe edge -> Maybe edge) -> Dict Int (Dict Int edge) -> Dict Int (Dict Int edge)
updateEdgeHelp a b updater =
    Dict.update a
        (\maybeInner ->
            case maybeInner of
                Nothing ->
                    case updater Nothing of
                        Just data ->
                            Just (Dict.singleton b data)

                        Nothing ->
                            Nothing

                Just inner ->
                    Just (Dict.update b updater inner)
        )


node : Int -> Graph node edge -> Maybe node
node id (Graph { nodes }) =
    Dict.get id nodes


neighbors : Int -> Graph node edge -> Maybe (Dict Int edge)
neighbors id (Graph { edges }) =
    Dict.get id edges
