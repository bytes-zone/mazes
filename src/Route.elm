module Route exposing (MazeShape(..), Route(..), parse, toAbsolutePath)

import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser, int, map, oneOf, s, top)
import Url.Parser.Query as Query


type Route
    = Home
    | Maze { shape : MazeShape, seed : Int, width : Maybe Int, height : Maybe Int }
    | NotFound


type MazeShape
    = Squares
    | Hexes


parse : Url -> Route
parse =
    Parser.parse parser >> Maybe.withDefault NotFound


parser : Parser (Route -> b) b
parser =
    oneOf
        [ map
            (\shape seed width height ->
                Maze { shape = shape, seed = seed, width = width, height = height }
            )
            (top </> s "maze" </> mazeShapeParser </> int <?> Query.int "width" <?> Query.int "height")
        , map Home top
        ]


mazeShapeParser : Parser (MazeShape -> b) b
mazeShapeParser =
    Parser.custom "SHAPE"
        (\segment ->
            case segment of
                "squares" ->
                    Just Squares

                "hexes" ->
                    Just Hexes

                _ ->
                    Nothing
        )


toAbsolutePath : Route -> String
toAbsolutePath route =
    case route of
        Home ->
            Builder.absolute [] []

        Maze { shape, seed, width, height } ->
            Builder.absolute
                [ "maze"
                , shapeToSegment shape
                , String.fromInt seed
                ]
                (List.filterMap identity
                    [ Maybe.map (Builder.int "width") width
                    , Maybe.map (Builder.int "height") height
                    ]
                )

        NotFound ->
            Builder.absolute [ "404" ] []


shapeToSegment : MazeShape -> String
shapeToSegment shape =
    case shape of
        Squares ->
            "squares"

        Hexes ->
            "hexes"
