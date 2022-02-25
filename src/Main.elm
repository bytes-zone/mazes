module Main exposing (..)

import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Navigation as Navigation
import Css
import Css.Global as Global
import Css.Media as Media
import Dict exposing (Dict)
import Html as RootHtml
import Html.Events.Extra.Touch as Touch exposing (Touch)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HAttrs exposing (css)
import Html.Styled.Events as Events
import Html.Styled.Lazy as Lazy
import Json.Decode as Decode
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
    , screenRatio : Float
    , drawing : Dict Int ( ( Float, Float ), List ( Float, Float ) )
    , mouseDraw : Bool
    }


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | SetNextSeed Int
    | SetNewMazeShape Route.MazeShape
    | SetNewMazeDifficulty Int
    | NextMaze
    | Draw (List Touch)
    | ResetLines
    | BackToGenerator
    | StartMouseDraw
    | MouseDraw ( Float, Float )
    | StopMouseDraw
    | GotViewport Viewport


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init () url key =
    ( { key = key
      , route = Route.parse url
      , nextSeed = 0
      , newMazeShape = Route.Hexes
      , newMazeDifficulty = 10
      , screenRatio = 1024.0 / 768.0
      , drawing = Dict.empty
      , mouseDraw = False
      }
    , Cmd.batch
        [ Time.now
            |> Task.map Time.posixToMillis
            |> Task.perform SetNextSeed
        , Dom.getViewport
            |> Task.perform GotViewport
        ]
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

        NextMaze ->
            let
                { width, height, shape } =
                    baseParams model
            in
            ( { model
                | nextSeed = model.nextSeed + 1
                , drawing = Dict.empty
              }
            , Route.Maze
                { shape = shape
                , seed = model.nextSeed
                , width = width
                , height = height
                }
                |> Route.toAbsolutePath
                |> Navigation.pushUrl model.key
            )

        Draw newTouches ->
            ( { model
                | drawing =
                    List.foldl
                        (\{ identifier, clientPos } ->
                            Dict.update
                                identifier
                                (\v ->
                                    case v of
                                        Just ( first, rest ) ->
                                            Just ( clientPos, first :: rest )

                                        Nothing ->
                                            Just ( clientPos, [] )
                                )
                        )
                        model.drawing
                        newTouches
              }
            , Cmd.none
            )

        StartMouseDraw ->
            ( { model | mouseDraw = True }
            , Cmd.none
            )

        MouseDraw coord ->
            ( if model.mouseDraw then
                { model
                    | drawing =
                        Dict.update
                            0
                            (\v ->
                                case v of
                                    Just ( first, rest ) ->
                                        Just ( coord, first :: rest )

                                    Nothing ->
                                        Just ( coord, [] )
                            )
                            model.drawing
                }

              else
                model
            , Cmd.none
            )

        StopMouseDraw ->
            ( { model | mouseDraw = False }
            , Cmd.none
            )

        ResetLines ->
            ( { model | drawing = Dict.empty }
            , Cmd.none
            )

        BackToGenerator ->
            ( model
            , Navigation.pushUrl model.key (Route.toAbsolutePath Route.New)
            )

        GotViewport { viewport } ->
            ( { model | screenRatio = viewport.width / viewport.height }
            , Cmd.none
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
                    [ viewMazeControls
                    , viewCanvas model
                    , Lazy.lazy carveAndView info
                    ]

                Route.NotFound ->
                    [ Html.text "not found" ]
            )
        ]
            |> List.map Html.toUnstyled
    }


carveAndView : { width : Int, height : Int, shape : Route.MazeShape, seed : Int } -> Html msg
carveAndView params =
    baseMaze params
        |> Maze.generate (Random.initialSeed params.seed)
        |> viewMaze


viewNewFormControls : Model -> Html Msg
viewNewFormControls model =
    controlsBar
        [ Html.label []
            [ Html.text "Difficulty:"
            , Html.input
                [ HAttrs.type_ "range"
                , HAttrs.min "2"
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
            [ Events.onClick NextMaze ]
            [ Html.text "Carve!" ]
        ]


viewMazeControls : Html Msg
viewMazeControls =
    controlsBar
        [ button [ Events.onClick BackToGenerator ] [ Html.text "Back to Generator" ]
        , button [ Events.onClick ResetLines ] [ Html.text "Reset Lines" ]
        , button [ Events.onClick NextMaze ] [ Html.text "Next Maze" ]
        ]


controlsBar : List (Html msg) -> Html msg
controlsBar =
    Html.div
        [ css
            [ Css.displayFlex
            , Css.alignItems Css.baseline
            , Css.justifyContent Css.center
            , Css.height (Css.px 50)
            , Css.zIndex (Css.int 2)
            , Css.position Css.relative

            -- color scheme
            , Css.backgroundColor (Css.hex "ECEFF1")
            , Css.color (Css.hex "37474F")
            , darkMode
                [ Css.backgroundColor (Css.hex "37474F")
                , Css.color (Css.hex "ECEFF1")
                ]
            , Media.withMedia [ Media.only Media.print [] ]
                [ Css.display Css.none ]
            ]
        ]


viewCanvas : Model -> Html Msg
viewCanvas model =
    model.drawing
        |> Dict.values
        |> List.map
            (\( first, rest ) ->
                let
                    toCoord ( x, y ) =
                        String.fromFloat x ++ "," ++ String.fromFloat y
                in
                Svg.path
                    [ Attrs.d
                        ("M "
                            ++ toCoord first
                            ++ " L "
                            ++ String.join " L " (List.map toCoord rest)
                        )
                    , Attrs.fill "none"
                    , Attrs.strokeWidth "5"
                    , Attrs.css
                        [ Css.property "stroke" "#76FF03"
                        , darkMode [ Css.property "stroke" "#CCFF90" ]
                        ]
                    ]
                    []
            )
        |> Svg.svg
            [ Attrs.css
                [ Css.position Css.absolute
                , Css.zIndex (Css.int 1)
                , Css.top (Css.px 0)
                , Css.left (Css.px 0)
                , Css.width (Css.vw 100)
                , Css.height (Css.vh 100)
                ]
            , Events.preventDefaultOn "touchmove"
                (Touch.eventDecoder
                    |> Decode.map .touches
                    |> Decode.map (\touches -> ( Draw touches, True ))
                )
            , Events.preventDefaultOn "mousedown"
                (Decode.succeed ( StartMouseDraw, True ))
            , Events.preventDefaultOn "mouseup"
                (Decode.succeed ( StopMouseDraw, True ))
            , Events.preventDefaultOn "mousemove"
                (Decode.map2 Tuple.pair
                    (Decode.field "clientX" Decode.float)
                    (Decode.field "clientY" Decode.float)
                    |> Decode.map (\coords -> ( MouseDraw coords, True ))
                )
            ]


baseParams : Model -> { width : Int, height : Int, shape : Route.MazeShape }
baseParams model =
    { shape = model.newMazeShape
    , width = round (toFloat model.newMazeDifficulty * model.screenRatio)
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
                                ( "#ECEFF1", "#546E7A" )

                            Just Maze.Entrance ->
                                ( "#B2FF59", "#64DD17" )

                            Just Maze.Exit ->
                                ( "#64FFDA", "#00BFA5" )
                in
                [ Attrs.strokeWidth "1"
                , Attrs.css
                    [ Css.property "stroke" light
                    , Css.fill (Css.hex light)
                    , darkMode
                        [ Css.property "stroke" dark
                        , Css.fill (Css.hex dark)
                        ]
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
