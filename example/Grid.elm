module Example.Grid exposing (..)

import Draw exposing (..)
import Generative exposing (..)
import Html exposing (Html, div, text)
import Random
import Svg exposing (Svg)
import Svg.Attributes exposing (transform)


type Model
    = Setup Configuration
    | Model Configuration Grid


type Configuration
    = Configuration Int Int Float


type Grid
    = Grid (List Float)


type alias Line =
    ( ( Float, Float ), ( Float, Float ) )


type Msg
    = Generate
    | Draw Grid


init : ( Model, Cmd Msg )
init =
    update Generate (Setup <| Configuration 16 16 10)


setup : Configuration -> List Line
setup (Configuration m n _) =
    List.map
        (\_ -> ( ( -0.5, -0.5 ), ( 0.5, 0.5 ) ))
        (List.range 0 (m * n - 1))


draw : Model -> List Line -> List (Svg Msg)
draw model start =
    let
        flip r =
            if r > 0 then
                0
            else
                90
    in
    case model of
        Model (Configuration m n size) (Grid x) ->
            List.map3
                (\( ( x1, y1 ), ( x2, y2 ) ) r ( dx, dy ) ->
                    g
                        [ transform <|
                            Draw.scale size
                                ++ Draw.translate dx dy
                                ++ Draw.rotate (flip r)
                        , Svg.Attributes.style <| strokeWidth (0.5 / size)
                        ]
                        [ line x1 y1 x2 y2 ]
                )
                start
                x
                (makeGrid m n)

        _ ->
            []


view : Model -> Html Msg
view model =
    case model of
        Model config (Grid d) ->
            a4Landscape
                []
                [ g
                    [ transform <| Draw.translate 60 25 ]
                    (config |> setup |> draw model)
                ]

        _ ->
            text ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Generate, Setup (Configuration m n _) ) ->
            ( model
            , Random.generate Draw <|
                Random.map Grid (randomList (m * n))
            )

        ( Draw data, Setup config ) ->
            ( Model config data, Cmd.none )

        _ ->
            ( model, Cmd.none )
