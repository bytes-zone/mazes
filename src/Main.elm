module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Css
import Html as RootHtml
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HAttrs exposing (css)
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
    }


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init () url key =
    enforceRedirection
        { key = key
        , route = Route.parse url
        , nextSeed = 0 -- TODO: get from flags
        }


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
            enforceRedirection { model | route = Route.parse url }


enforceRedirection : Model -> ( Model, Cmd Msg )
enforceRedirection model =
    if model.route == Route.Home then
        ( { model | nextSeed = model.nextSeed + 1 }
        , Route.Maze { seed = model.nextSeed, width = Just 10, height = Just 10 }
            |> Route.toAbsolutePath
            |> Navigation.replaceUrl model.key
        )

    else
        ( model
        , Cmd.none
        )


view : Model -> Browser.Document msg
view model =
    { title = "Nate's Mazes"
    , body =
        [ Html.main_ []
            [ case model.route of
                Route.Home ->
                    Html.text "home"

                Route.Maze info ->
                    Html.text (Debug.toString info)

                Route.NotFound ->
                    Html.text "not found"
            ]
            |> Html.toUnstyled
        ]
    }


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
