module Route exposing (Route(..), parse, toAbsolutePath)

import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser, int, map, oneOf, s, top)
import Url.Parser.Query as Query


type Route
    = Home
    | Maze { seed : Int, width : Maybe Int, height : Maybe Int }
    | NotFound


parse : Url -> Route
parse =
    Parser.parse parser >> Maybe.withDefault NotFound


parser : Parser (Route -> b) b
parser =
    oneOf
        [ map
            (\seed width height ->
                Maze { seed = seed, width = width, height = height }
            )
            (top </> s "maze" </> int <?> Query.int "width" <?> Query.int "height")
        , map Home top
        ]


toAbsolutePath : Route -> String
toAbsolutePath route =
    case route of
        Home ->
            Builder.absolute [] []

        Maze { seed, width, height } ->
            Builder.absolute [ "maze", String.fromInt seed ]
                (List.filterMap identity
                    [ Maybe.map (Builder.int "width") width
                    , Maybe.map (Builder.int "height") height
                    ]
                )

        NotFound ->
            Builder.absolute [ "404" ] []
