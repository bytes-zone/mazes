module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Css
import Html as RootHtml
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HAttrs exposing (css)
import Maze exposing (Maze)
import Random
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attrs
import Url exposing (Url)


type alias Flags =
    ()


type alias Model =
    { key : Navigation.Key }


type Msg
    = OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init () _ key =
    ( { key = key }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model
    , Cmd.none
    )


view : Model -> Browser.Document msg
view model =
    { title = "Nate's Mazes"
    , body = []
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }


containerAttrs : List (Html.Attribute msg)
containerAttrs =
    [ HAttrs.style "max-width" "calc(33% - 5px)"
    , HAttrs.style "margin-right" "5px"
    ]


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
