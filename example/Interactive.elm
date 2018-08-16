module Example.Interactive exposing (..)

import Draw exposing (..)
import Html exposing (Html, text, div, button, h1, h3, h4, span, ul, input, p)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import LSystem
import LSystem.Turtle exposing (State(..), turtle)
import Svg exposing (Svg)
import Svg.Attributes exposing (transform)
import Svg.PathD as PathD exposing (d_)
import Keyboard
import Char


type alias Model =
    { state: List State
    , l: List State
    , r: List State
    , d: List State
    , s: List State
    , base: List State
    , history: List (List State)
    , length: Int
    , recordedState: List State
    , recOn: Bool
    , savedPatterns: List (List State)
    }

type Configuration
    = Configuration ( Float, Float ) Float



initialState : List State
initialState = [ D, R, D, L, D, L, D, D, L, D, D, L, D ]
-- initialState = [ D, R, D, R, D, R, D ]

initialModel : List State -> Model
initialModel state =
    Model state [L] [R] [D] [S] state [state] (List.length state) [] False []

model : Model
model = initialModel initialState

rule : Model -> State -> List State
rule model state =
    case state of
        D ->
            model.d

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

-- mapD : Int -> List State
-- mapD i =
--     case i of
--         1 -> [ D, D, D, L, D, L, D, L, D, L, D ]
--         2 -> [ D, L, D, R ]
--         3 -> [ D, D, L, D, L, D, R, D, R, D, D, R, D, L ]
--         4 -> [ D, D, L, D, R, D ]
--         5 -> [ D, D, L, D, R, D, D ]
--         6 -> [ D, R, D, L, D, R, D, R, D, D, D ]
--         7 -> [ D, D, D, L, D, L, D, D, L, D, D, D, D ]
--         8 -> [ D, D, D, L, D, L, D, L, D, L, D ]
--         9 -> [ R, D, R, D, L, L, D, D, D ]
--         10 -> [ D, L, D, R ]
--         11 -> [ D, L ]
--         _ -> [ D ]

type Msg
    = RecOnOff
    | RecReset
    | KeyDown Keyboard.KeyCode
    | SetRecordedAsBase
    | SetRecordedAsStep
    | Iterate
    | ResetSvg
    | SavePattern
    | SelectPattern (List State)


init : ( Model, Cmd Msg )
init =
    ( initialModel initialState, Cmd.none )

draw : List State -> Configuration -> Svg Msg
draw pattern (Configuration p0 a0) =
    Svg.path
        [ d_ <| [ PathD.M p0 ] ++ turtle pattern 90
        , Svg.Attributes.strokeWidth "0.2"
        ]
        []

w3 = [("width", "30%")]
dib = [("display", "inline-block")]
dbw = style (dib ++ w3)

view : Model -> Html Msg
view model =
    div []
        [ h1 [style [("margin-top", "0")]] [ text "LSystem Interactive" ]
        , div [ style [("display", "block"), ("padding", "10px"), ("border-bottom", "1px solid black")]]
            [ div [ dbw ]
                [ span [ style [("margin", "10px")]] [ text <| toString model.length ]
                , span [ style [("margin", "10px")]] [ text <| toString model.base ]
                , p [] [ text <| toString model.recordedState ]
                ]
            , ul [ dbw ]
                [ button [ onClick <| RecOnOff ] [ text <| "Rec " ++ (if model.recOn then "On" else "Off") ]
                , button [ onClick <| RecReset ] [ text "Reset recording" ]
                , button [ onClick <| SavePattern ] [ text "Save pattern" ]
                , button [ onClick <| SetRecordedAsBase ] [ text "Set base" ]
                , button [ onClick <| SetRecordedAsStep ] [ text "Set step" ]
                , button [ onClick <| ResetSvg ] [ text "Reset svg" ]
                , button [ onClick <| Iterate ] [ text "Iterate" ]
                ]
            , a6
                [ style <| ("border", "1px solid black") :: dib ]
                [ g
                    [ transform <| Draw.translate 15 10 ++ Draw.scale 2 ]
                    [ draw model.recordedState (Configuration ( 0, 0 ) 0) ]
                ]
            ]
        , a4Landscape
            [ style [("height", "700px"), ("width", "50%"), ("display", "inline-blick")]]
            [ g
                [ transform <| Draw.translate 150 100 ++ Draw.scale 2 ]
                [ draw model.state (Configuration ( 0, 0 ) 0) ]
            ]
        , div
            [ style <| dib ++ w3 ++ [ ("vertical-align", "top"), ("padding", "20px") ]]
            (List.map viewSavedPattern model.savedPatterns)
        ]

-- ruleButtonView : Int -> Html Msg
-- ruleButtonView n =
--     div []
--         [ button [ onClick <| ChooseDRule n ] [ text <| toString n ]
--         , span [] [ text <| toString <| mapD n ]
--         ]

viewSavedPattern pattern =
    div
        []
        [ a6
            [ style <| [ ("border", "1px solid black"), ("margin", "10px") ] ++ dib ]
            [ g
                [ transform <| Draw.translate 15 10 ++ Draw.scale 2 ]
                [ draw pattern (Configuration (0, 0) 0) ]
            ]
        , button [ onClick <| SelectPattern pattern ] [ text <| "Select" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newState = LSystem.apply (rule model) model.state
    in
        case msg of
            RecOnOff ->
                { model | recOn = not model.recOn } ! []
            RecReset ->
                { model | recordedState = [] } ! []
            KeyDown keycode ->
                { model | recordedState = updateRecordedState keycode model.recordedState } ! []
            SetRecordedAsBase ->
                { model | base = model.recordedState } ! []
            SetRecordedAsStep ->
                { model | d = model.recordedState } ! []
            Iterate ->
                ( { model
                    | state = newState
                    , history = model.state :: model.history
                    , length = List.length newState
                    }
                , Cmd.none
                )
            ResetSvg ->
                ( { model | state = model.base }, Cmd.none )
            SavePattern ->
                { model | savedPatterns = model.recordedState :: model.savedPatterns } ! []
            SelectPattern pattern ->
                { model | recordedState = pattern } ! []

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

updateRecordedState keycode recordedState =
    case keycode of
        37 -> recordedState ++ [L]
        38 -> recordedState ++ [D]
        39 -> recordedState ++ [R]
        32 -> recordedState ++ [S]
        _ -> recordedState



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
