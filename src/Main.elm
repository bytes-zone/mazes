module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Css
import Css.Global as Global
import Css.Media as Media
import Html as RootHtml
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HAttrs exposing (css)
import Html.Styled.Events as Events
import Maze exposing (Maze)
import Random
import Route exposing (Route)
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attrs
import Task
import Time
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
    | SetNextSeed Int
    | SetNewMazeShape Route.MazeShape
    | SetNewMazeDifficulty Int
    | StartSolvingNewMaze


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init () url key =
    ( { key = key
      , route = Route.parse url
      , nextSeed = 0
      , newMazeShape = Route.Hexes
      , newMazeDifficulty = 10
      }
    , Time.now
        |> Task.map Time.posixToMillis
        |> Task.perform SetNextSeed
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

        SetNextSeed seed ->
            ( { model | nextSeed = seed }
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
                , width = width
                , height = height
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
            , Global.body
                [ Css.fontFamily Css.sansSerif
                , Css.backgroundColor (Css.hex "FFFFFF")
                , darkMode
                    [ Css.backgroundColor (Css.hex "212121")
                    ]
                ]
            ]
        , Html.main_ []
            (case model.route of
                Route.New ->
                    [ viewNewFormControls model
                    , model
                        |> baseParams
                        |> baseMaze
                        |> viewMaze
                    ]

                Route.Maze info ->
                    [ baseMaze info
                        |> Maze.generate (Random.initialSeed info.seed)
                        |> viewMaze
                    ]

                Route.NotFound ->
                    [ Html.text "not found" ]
            )
        ]
            |> List.map Html.toUnstyled
    }


viewNewFormControls : Model -> Html Msg
viewNewFormControls model =
    controlsBar
        [ Html.label []
            [ Html.text "Difficulty:"
            , Html.input
                [ HAttrs.type_ "range"
                , HAttrs.min "1"
                , HAttrs.max "30"
                , HAttrs.value (String.fromInt model.newMazeDifficulty)
                , Events.onInput (String.toInt >> Maybe.withDefault 10 >> SetNewMazeDifficulty)
                , let
                    track =
                        [ Css.height (Css.em 1)
                        , Css.backgroundColor (Css.hex "CFD8DC")
                        , darkMode [ Css.backgroundColor (Css.hex "263238") ]
                        ]

                    thumb =
                        [ Css.property "-webkit-appearance" "none"
                        , Css.height (Css.em 1.5)
                        , Css.width (Css.em 1.5)
                        , Css.marginTop (Css.em -0.25)
                        , Css.borderRadius (Css.pct 25)
                        , Css.backgroundColor (Css.hex "263238")
                        , darkMode [ Css.backgroundColor (Css.hex "CFD8DC") ]
                        ]
                  in
                  css
                    [ Css.margin (Css.px 10)
                    , Css.position Css.relative
                    , Css.property "-webkit-appearance" "none"
                    , Css.pseudoElement "-webkit-slider-runnable-track" track
                    , Css.pseudoElement "-webkit-slider-thumb" thumb
                    , Css.pseudoElement "-moz-range-track" track
                    , Css.pseudoElement "-moz-range-thumb" thumb
                    , Css.pseudoElement "-ms-track" track
                    , Css.pseudoElement "-ms-thumb" thumb
                    ]
                ]
                []
            ]
        , case model.newMazeShape of
            Route.Hexes ->
                button
                    [ Events.onClick (SetNewMazeShape Route.Squares) ]
                    [ Html.text "Change to Squares" ]

            Route.Squares ->
                button
                    [ Events.onClick (SetNewMazeShape Route.Hexes) ]
                    [ Html.text "Change to Hexes" ]
        , button
            [ Events.onClick StartSolvingNewMaze ]
            [ Html.text "Carve!" ]
        ]


controlsBar : List (Html msg) -> Html msg
controlsBar =
    Html.div
        [ css
            [ Css.displayFlex
            , Css.alignItems Css.baseline
            , Css.justifyContent Css.center
            , Css.height (Css.px 50)

            -- color scheme
            , Css.backgroundColor (Css.hex "ECEFF1")
            , Css.color (Css.hex "37474F")
            , darkMode
                [ Css.backgroundColor (Css.hex "37474F")
                , Css.color (Css.hex "ECEFF1")
                ]
            ]
        ]


baseParams : Model -> { width : Int, height : Int, shape : Route.MazeShape }
baseParams model =
    { shape = model.newMazeShape
    , width = round (toFloat model.newMazeDifficulty * 1.4)
    , height = model.newMazeDifficulty
    }


viewMaze : Maze -> Html msg
viewMaze maze =
    Maze.view
        { cell =
            \{ role } ->
                let
                    ( light, dark ) =
                        case role of
                            Nothing ->
                                ( Css.hex "ECEFF1", Css.hex "546E7A" )

                            Just Maze.Entrance ->
                                ( Css.hex "B2FF59", Css.hex "64DD17" )

                            Just Maze.Exit ->
                                ( Css.hex "64FFDA", Css.hex "00BFA5" )
                in
                [ Attrs.css
                    [ Css.fill light
                    , darkMode [ Css.fill dark ]
                    ]
                ]
        , wall =
            [ Attrs.css
                [ Css.property "stroke" "#546E7A"
                , darkMode [ Css.property "stroke" "#37474F" ]
                ]
            , Attrs.strokeWidth "3"
            , Attrs.strokeLinecap "round"
            ]
        , container =
            [ css
                [ Css.position Css.absolute
                , Css.top (Css.pct 7)
                , Css.left (Css.pct 2.5)
                , Css.width (Css.vw 95)
                , Css.height (Css.vh 90.5)
                ]
            ]
        }
        maze


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



--- UTILS


darkMode : List Css.Style -> Css.Style
darkMode =
    Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]


button : List (Html.Attribute msg) -> List (Html msg) -> Html msg
button attrs children =
    Html.button
        (css
            [ Css.fontSize (Css.em 0.9)
            , Css.margin (Css.px 10)
            , Css.padding2 (Css.px 7) (Css.px 10)
            , Css.borderRadius (Css.px 2)
            , Css.border Css.zero
            , Css.backgroundColor (Css.hex "CFD8DC")
            , Css.color (Css.hex "263238")
            , darkMode
                [ Css.backgroundColor (Css.hex "263238")
                , Css.color (Css.hex "CFD8DC")
                ]
            ]
            :: attrs
        )
        children
