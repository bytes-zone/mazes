module RouteTests exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Route exposing (Route)
import Test exposing (..)
import Url


roundTrip : Test
roundTrip =
    fuzz routeFuzzer "toAbsolutePath -> parse is a round trip" <|
        \route ->
            case Url.fromString ("https://example.com" ++ Route.toAbsolutePath route) of
                Just url ->
                    url
                        |> Route.parse
                        |> Expect.equal route

                Nothing ->
                    Expect.fail "could not generate a URL from the given route"


routeFuzzer : Fuzzer Route
routeFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Route.Home
        , Fuzz.map3 (\seed width height -> Route.Maze { seed = seed, width = width, height = height })
            (Fuzz.intRange 0 1)
            maybeDimensionFuzzer
            maybeDimensionFuzzer
        , Fuzz.constant Route.NotFound
        ]


maybeDimensionFuzzer : Fuzzer (Maybe Int)
maybeDimensionFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Nothing
        , Fuzz.map Just (Fuzz.intRange 0 1)
        ]
