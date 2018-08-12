module Example.Rectangles2 exposing (..)

import Draw exposing (..)
import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)
import LSystem
import LSystem.Turtle exposing (State(..), turtle)
import Svg exposing (Svg)
import Svg.Attributes exposing (transform)
import Svg.PathD as PathD exposing (d_)


type Model =
    Model Int (List State) Int


type Configuration
    = Configuration ( Float, Float ) Float


rule : Int -> State -> List State
rule x state =
    case state of
        D ->
            case x of
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
    | ChooseRule Int Int
    | Reset

initialState : List State
initialState = [ D, R, D, R, D, R, D ]

init : ( Model, Cmd Msg )
init =
    update Iterate (Model 3 initialState 1)


draw : Model -> Configuration -> Svg Msg
draw (Model _ states _) (Configuration p0 a0) =
    Svg.path
        [ d_ <| [ PathD.M p0 ] ++ turtle states 90
        , Svg.Attributes.strokeWidth "0.2"
        ]
        []


view : Model -> Html Msg
view model =
    case model of
        Model n state x ->
            div []
                [ button [ onClick <| ChooseRule n 1 ] [ text "1" ]
                , button [ onClick <| ChooseRule n 2 ] [ text "2" ]
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
    case ( msg, model ) of
        ( Iterate, Model n state x ) ->
            case n > 0 of
                True ->
                    update Iterate <|
                        Model (n - 1) (LSystem.apply (rule x) state) x

                False ->
                    ( model, Cmd.none )
        ( ChooseRule n1 x1, Model n state x ) ->
            update Iterate <|
                Model n1 (LSystem.apply (rule x1) state) x1
        ( Reset, Model n state x ) ->
            ( Model 1 initialState 0, Cmd.none )
