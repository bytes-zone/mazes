module Graph exposing (Graph, empty, neighbors, node, setEdge, setNode)

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
setEdge a b data (Graph guts) =
    Graph
        { guts
            | edges =
                guts.edges
                    |> setEdgeHelp a b data
                    |> setEdgeHelp b a data
        }


setEdgeHelp : Int -> Int -> edge -> Dict Int (Dict Int edge) -> Dict Int (Dict Int edge)
setEdgeHelp a b data =
    Dict.update a
        (\maybeInner ->
            Just <|
                case maybeInner of
                    Nothing ->
                        Dict.singleton b data

                    Just inner ->
                        Dict.insert b data inner
        )


node : Int -> Graph node edge -> Maybe node
node id (Graph { nodes }) =
    Dict.get id nodes


neighbors : Int -> Graph node edge -> Maybe (Dict Int edge)
neighbors id (Graph { edges }) =
    Dict.get id edges
