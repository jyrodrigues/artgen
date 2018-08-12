module Example.Interactive exposing (..)

import Draw exposing (..)
import Html exposing (Html, text, div, button, h1)
import Html.Events exposing (onClick)
import LSystem
import LSystem.Turtle exposing (State(..), turtle)
import Svg exposing (Svg)
import Svg.Attributes exposing (transform)
import Svg.PathD as PathD exposing (d_)


type alias Model =
    { state: List State
    , l: Int
    , r: Int
    , d: Int
    , s: Int
    , depth: Int
    }

initialState : List State
initialState = [ D, R, D, L, D, L, D, D, L, D, D, L, D ]
-- initialState = [ D, R, D, R, D, R, D ]

initialModel : Model
initialModel =
    Model initialState 0 0 0 0 3


model : Model
model = initialModel



type Configuration
    = Configuration ( Float, Float ) Float


rule : Model -> State -> List State
rule model state =
    case state of
        D ->
            case model.d of
                1 -> [ D, D, D, L, D, L, D, L, D, L, D ]
                2 -> [ D, L, D, R ]
                _ -> [ D ]
            -- [ D, D, L, D, L, D, R, D, R, D, D, R, D, L ]
            -- [ D, D, L, D, R, D ]
            -- [ D, D, L, D, R, D, D ]
            -- [ D, R, D, L, D, R, D, R, D, D, D ]
            -- [ D, D, D, L, D, L, D, D, L, D, D, D, D ]
            -- [ D, D, D, L, D, L, D, L, D, L, D ]
            -- [ R, D, R, D, L, L, D, D, D ]
            -- [ D, L, D, R ]
            -- [ D, L ]
            -- [ D ]

        L ->
            -- [ L, D ]
            -- [ L, D, L, D, D, D, L, D ]
            -- [ R, D, R, D, D, D, R, D ]
            -- [ D, D, R, R, D, D, R ]
            -- [ D, D, R, R, D, D, R ]
            [ L ]

        R ->
            -- [ D, L ]
            [ R ]
            -- [ D, D, D, D, D, D, D, R, R, D, D, R ]

        s ->
            [ s ]


type Msg
    = Iterate
    | ChooseDRule Int
    | Reset

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

draw : Model -> Configuration -> Svg Msg
draw model (Configuration p0 a0) =
    Svg.path
        [ d_ <| [ PathD.M p0 ] ++ turtle model.state 90
        , Svg.Attributes.strokeWidth "0.2"
        ]
        []


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "H!" ]
        , h1 [] [ text "H!" ]
        , button [ onClick <| ChooseDRule 1 ] [ text "1.1" ]
        , button [ onClick <| ChooseDRule 2 ] [ text "2.2" ]
        , button [ onClick <| Iterate ] [ text "Iterate" ]
        , button [ onClick <| Reset ] [ text "Reset" ]
        , a4Landscape
            []
            [ g
                [ transform <| Draw.translate 150 100 ++ Draw.scale 2 ]
                [ draw model (Configuration ( 0, 0 ) 0) ]
            ]
        ]



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Iterate ->
            ( { model | state = LSystem.apply (rule model) model.state }, Cmd.none )
        ChooseDRule newD ->
            ( { model | d = newD }, Cmd.none )
        Reset ->
            ( initialModel, Cmd.none )
