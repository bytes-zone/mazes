module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Css
import Css.Global as Global
import Html as RootHtml
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HAttrs exposing (css)
import Html.Styled.Events as Events
import Maze exposing (Maze)
import Random
import Route exposing (Route)
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attrs
import Url exposing (Url)


type alias Flags =
    ()


type alias Model =
    { key : Navigation.Key
    , route : Route
    , nextSeed : Int
    , newMazeShape : Route.MazeShape
    , newMazeDifficulty : Int
    }


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | SetNewMazeShape Route.MazeShape
    | SetNewMazeDifficulty Int
    | StartSolvingNewMaze


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init () url key =
    ( { key = key
      , route = Route.parse url
      , nextSeed = 0 -- TODO: get from flags
      , newMazeShape = Route.Hexes
      , newMazeDifficulty = 10
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlRequest (Browser.Internal url) ->
            ( model
            , Navigation.pushUrl model.key (Url.toString url)
            )

        OnUrlRequest (Browser.External url) ->
            ( model
            , Navigation.load url
            )

        OnUrlChange url ->
            ( { model | route = Route.parse url }
            , Cmd.none
            )

        SetNewMazeShape shape ->
            ( { model | newMazeShape = shape }
            , Cmd.none
            )

        SetNewMazeDifficulty difficulty ->
            ( { model | newMazeDifficulty = difficulty }
            , Cmd.none
            )

        StartSolvingNewMaze ->
            let
                { width, height, shape } =
                    baseParams model
            in
            ( { model | nextSeed = model.nextSeed + 1 }
            , Route.Maze
                { shape = shape
                , seed = model.nextSeed
                , width = Just width
                , height = Just height
                }
                |> Route.toAbsolutePath
                |> Navigation.pushUrl model.key
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Nate's Mazes"
    , body =
        [ Global.global
            [ Global.everything [ Css.boxSizing Css.borderBox ]
            , Global.body [ Css.padding (Css.px 20) ]
            ]
        , Html.main_ []
            [ case model.route of
                Route.New ->
                    -- just dumping all the controls in here for now. Will make them look nice later.
                    Html.div
                        []
                        [ Html.label []
                            [ Html.text "Difficulty"
                            , Html.input
                                [ HAttrs.type_ "range"
                                , HAttrs.min "1"
                                , HAttrs.max "30"
                                , HAttrs.value (String.fromInt model.newMazeDifficulty)
                                , Events.onInput (String.toInt >> Maybe.withDefault 10 >> SetNewMazeDifficulty)
                                ]
                                []
                            ]
                        , Html.label []
                            [ Html.text "Shape"
                            , case model.newMazeShape of
                                Route.Hexes ->
                                    Html.button
                                        [ Events.onClick (SetNewMazeShape Route.Squares) ]
                                        [ Html.text "Change to Squares" ]

                                Route.Squares ->
                                    Html.button
                                        [ Events.onClick (SetNewMazeShape Route.Hexes) ]
                                        [ Html.text "Change to Hexes" ]
                            ]
                        , Html.button
                            [ Events.onClick StartSolvingNewMaze ]
                            [ Html.text "Carve!" ]
                        , model
                            |> baseParams
                            |> baseMaze
                            |> Maze.view
                                { cell = cellAttrs
                                , wall = wallAttrs
                                , container = [ css [ Css.width (Css.vw 95), Css.height (Css.vh 80) ] ]
                                }
                        ]

                Route.Maze info ->
                    baseMaze
                        { width = Maybe.withDefault 10 info.width
                        , height = Maybe.withDefault 10 info.height
                        , shape = info.shape
                        }
                        |> Maze.generate (Random.initialSeed info.seed)
                        |> Maze.view
                            { cell = cellAttrs
                            , wall = wallAttrs
                            , container = [ css [ Css.width (Css.vw 95), Css.height (Css.vh 80) ] ]
                            }

                Route.NotFound ->
                    Html.text "not found"
            ]
        ]
            |> List.map Html.toUnstyled
    }


baseParams : Model -> { width : Int, height : Int, shape : Route.MazeShape }
baseParams model =
    { shape = model.newMazeShape
    , width = round (toFloat model.newMazeDifficulty * 1.4)
    , height = model.newMazeDifficulty
    }


baseMaze : { otherStuff | width : Int, height : Int, shape : Route.MazeShape } -> Maze
baseMaze { width, height, shape } =
    let
        dimensions =
            { width = width
            , height = height
            , entrance = { row = 0, column = 0 }
            , exit = { row = height - 1, column = width - 1 }
            }
    in
    case shape of
        Route.Hexes ->
            Maze.hexes dimensions

        Route.Squares ->
            Maze.squares dimensions


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


containerAttrs : List (Html.Attribute msg)
containerAttrs =
    []


wallAttrs : List (Svg.Attribute msg)
wallAttrs =
    [ Attrs.stroke "#546E7A"
    , Attrs.strokeWidth "3"
    , Attrs.strokeLinecap "round"
    ]


cellAttrs : Maze.Cell -> List (Svg.Attribute msg)
cellAttrs { role } =
    case role of
        Nothing ->
            [ Attrs.fill "#ECEFF1" ]

        Just Maze.Entrance ->
            [ Attrs.fill "#B2FF59" ]

        Just Maze.Exit ->
            [ Attrs.fill "#64FFDA" ]
