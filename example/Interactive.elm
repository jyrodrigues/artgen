module Example.Interactive exposing (..)

import Draw exposing (..)
import Html exposing (Html, text, div, button, h1, h3, h4, span, ul, input)
import Html.Events exposing (onClick, onInput)
import Html.Styled.Attributes exposing (css)
import Css exposing (backgroundColor, rgba)
import LSystem
import LSystem.Turtle exposing (State(..), turtle)
import Svg exposing (Svg)
import Svg.Attributes exposing (transform)
import Svg.PathD as PathD exposing (d_)
import Keyboard
import Char


type alias Model =
    { state: List State
    , l: Int
    , r: Int
    , d: Int
    , s: Int
    , base: List State
    , rbase: String
    , history: List (List State)
    , length: Int
    , hk: List Char
    , hc: List Keyboard.KeyCode
    , recOn: Bool
    }

type Configuration
    = Configuration ( Float, Float ) Float



initialState : List State
initialState = [ D, R, D, L, D, L, D, D, L, D, D, L, D ]
-- initialState = [ D, R, D, R, D, R, D ]

initialModel : List State -> Model
initialModel state =
    Model state 0 0 0 0 state "" [] (List.length state) [] [] False

model : Model
model = initialModel initialState

rule : Model -> State -> List State
rule model state =
    case state of
        D ->
            mapD model.d

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

mapD : Int -> List State
mapD i =
    case i of
        1 -> [ D, D, D, L, D, L, D, L, D, L, D ]
        2 -> [ D, L, D, R ]
        3 -> [ D, D, L, D, L, D, R, D, R, D, D, R, D, L ]
        4 -> [ D, D, L, D, R, D ]
        5 -> [ D, D, L, D, R, D, D ]
        6 -> [ D, R, D, L, D, R, D, R, D, D, D ]
        7 -> [ D, D, D, L, D, L, D, D, L, D, D, D, D ]
        8 -> [ D, D, D, L, D, L, D, L, D, L, D ]
        9 -> [ R, D, R, D, L, L, D, D, D ]
        10 -> [ D, L, D, R ]
        11 -> [ D, L ]
        _ -> [ D ]

type Msg
    = Iterate
    | ChooseDRule Int
    | Reset
    | SetBase
    | RegisterBase String
    | KeyDown Keyboard.KeyCode
    | RecOnOff

init : ( Model, Cmd Msg )
init =
    ( initialModel initialState, Cmd.none )

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
        [ h1 [] [ text "LSystem Interactive" ]
        , h3 [] [ text <| toString model.length ]
        , ul [] (List.map ruleButtonView (List.range 1 11))
        , h3 [] [ text <| toString model.base ]
        , h3 [] [ text <| toString model.hk ]
        , h3 [] [ text <| toString model.hc ]
        , ul []
            [ button [ onClick <| RecOnOff ] [ text <| "Rec " ++ (if model.recOn then "On" else "Off") ]
            , button [ onClick <| Iterate ] [ text "Iterate" ]
            , button [ onClick <| Reset ] [ text "Reset" ]
            , button [ onClick <| SetBase ] [ text "Set Base" ]
            , input [ onInput RegisterBase ] []
            ]
        -- , ul []
        --     [ button [ onClick <| Next ] [ text ">" ]
        --     , button [ onClick <| Previous ] [ text "<" ]
        --     ]
        , a4Landscape
            []
            [ g
                [ transform <| Draw.translate 150 100 ++ Draw.scale 2 ]
                [ draw model (Configuration ( 0, 0 ) 0) ]
            ]
        ]


ruleButtonView : Int -> Html Msg
ruleButtonView n =
    div []
        [ button [ onClick <| ChooseDRule n ] [ text <| toString n ]
        , span [] [ text <| toString <| mapD n ]
        ]
    

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newState = LSystem.apply (rule model) model.state
        resetModel = initialModel model.base
    in
        case msg of
            Iterate ->
                ( { model
                | state = newState
                , history = model.state :: model.history
                , length = List.length newState
                }, Cmd.none )
            ChooseDRule newD ->
                ( { model | d = newD }, Cmd.none )
            Reset ->
                ( { resetModel | base = model.base }, Cmd.none )
            RegisterBase s ->
                ( { model | rbase = s }, Cmd.none )
            SetBase ->
                ( { model | base = stringToState model.rbase }, Cmd.none )
            KeyDown keycode ->
                ( { model
                    | hk = (Char.fromCode keycode) :: model.hk
                    , hc = keycode :: model.hc
                    }
                , Cmd.none )
            RecOnOff ->
                { model | recOn = not model.recOn } ! []


stringToState s =
    let
        charList = String.toList <| String.toUpper s
    in
        List.map charToState charList

charToState c =
    case c of
        'D' -> D
        'R' -> R
        'L' -> L
        'S' -> S
        _ -> D


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <| subs model

subs : Model -> List (Sub Msg)
subs model =
    if model.recOn then
        [ Keyboard.downs KeyDown ]
    else
        []


{-| Program Entry.
-}
main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
