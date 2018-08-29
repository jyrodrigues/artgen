module Example.Interactive exposing (..)

import Draw exposing (..)
import Html exposing (Html, text, div, button, h1, h3, h4, span, ul, input, p)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (onClick, onInput)
import LSystem
import LSystem.Turtle exposing (State(..), turtle)
import Svg exposing (Svg)
import Svg.Attributes exposing (transform)
import Svg.PathD as PathD exposing (d_)
import Keyboard
import Debug


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
    , deg: Float
    , rdeg: String
    }

type Configuration
    = Configuration ( Float, Float ) Float



initialState : List State
initialState = [ D, R, D, R, D, R, D ]

initialSavedPatterns =
    [ [ D ]
    , [D,R,D,R,D,R,D,R,D,R,D]
    , [D,R,D,R,D,R,R,D,R,R,D,D,L,D,L,D,L,L,D,L,D,D]
    , [ D, R, D, R, D, R, D ]
    , [ D, D, R, D, L, D, R, D, R, D, D ]
    , [ D, R, D, L, D, L, D, R ]
    , [ D, D, D, L, D, L, D, L, D, L, D ]
    , [ D, L, D, R ]
    , [ D, D, L, D, L, D, R, D, R, D, D, R, D, L ]
    , [ D, D, L, D, R, D ]
    , [ D, D, L, D, R, D, D ]
    , [ D, R, D, L, D, R, D, R, D, D, D ]
    , [ D, D, D, L, D, L, D, D, L, D, D, D, D ]
    , [ D, D, D, L, D, L, D, L, D, L, D ]
    , [ R, D, R, D, L, L, D, D, D ]
    , [ D, L, D, R ]
    , [ L, D, L, D, D, D, L, D ]
    , [ R, D, R, D, D, D, R, D ]
    , [ D, D, R, R, D, D, R ]
    , [ D, D, R, R, D, D, R ]
    ]

initialModel : List State -> Model
initialModel state =
    Model state [L] [R] [D] [S] state [state] (List.length state) [] False initialSavedPatterns 90 "90"

model : Model
model = initialModel initialState

rule : List State -> State -> List State
rule pattern state =
    case state of
        D ->
            pattern

        L ->
            [ L ]

        R ->
            [ R ]

        s ->
            [ s ]

type Msg
    = RecOnOff
    | RecReset
    | KeyDown Keyboard.KeyCode
    | SetBase (List State)
    | SetStep (List State)
    | Iterate (List State)
    | ResetSvg
    | SavePattern
    | SelectPattern (List State)
    | RegisterDeg String
    | SetDeg


init : ( Model, Cmd Msg )
init =
    ( initialModel initialState, Cmd.none )

draw : List State -> Configuration -> Float -> Svg Msg
draw pattern (Configuration p0 a0) deg =
    Svg.path
        [ d_ <| [ PathD.M p0 ] ++ (turtle pattern deg)
        , Svg.Attributes.strokeWidth "0.2"
        ]
        []

dib = [("display", "inline-block")]
dbw = ("width", "30%") :: dib

view : Model -> Html Msg
view model =
    div []
        [ div
            [ style
                [ ("display", "block")
                , ("padding", "10px")
                , ("border-bottom", "1px solid black")
                ]
            ]
            [ div [ style dbw ]
                [ h1 [ style [("margin-top", "0")] ] [ text "LSystem Interactive" ]
                , span [ style [("margin", "10px")] ] [ text <| toString model.length ]
                , span [ style [("margin", "10px")] ] [ text <| toString model.base ]
                , p [] [ text <| toString model.recordedState ]
                ]
            , ul [ style dbw ]
                [ button [ onClick <| RecOnOff ] [ text <| "Rec " ++ (if model.recOn then "On" else "Off") ]
                , button [ onClick <| RecReset ] [ text "Reset recording" ]
                , button [ onClick <| SavePattern ] [ text "Save pattern" ]
                , button [ onClick <| SetBase model.recordedState ] [ text "Set base" ]
                , button [ onClick <| SetStep model.recordedState ] [ text "Set step" ]
                , button [ onClick <| ResetSvg ] [ text "Reset svg" ]
                , button [ onClick <| Iterate model.d ] [ text "Iterate" ]
                , input [ type_ "text", onInput RegisterDeg ] []
                , button [ onClick <| SetDeg ] [ text "Set degrees" ]
                ]
            , div
                [ style
                    [ ("display", "inline-block")
                    , ("width", "30%")
                    , ("height", "150px")
                    , ("vertical-align", "top")
                    , ("overflow", "scroll")
                    , ("border", "1px solid black")
                    , ("padding", "2px")
                    ]
                ]
                {-- ((++) [ span [] [ text <| "HERE" ++ toString (turtle model.recordedState 90)] ] --}
                [ a4Landscape
                    [ style <| ("border", "1px dashed black") :: dib ]
                    [ g
                        [ transform <| Draw.translate 15 10 ++ Draw.scale 2 ]
                        [ draw model.recordedState (Configuration ( 0, 0 ) 0) model.deg ]
                    ]
                ]--)
            ]
        , div
            [ style <|
                [ ("height", "700px")
                , ("width", "50%")
                , ("display", "inline-block")
                , ("overflow", "scroll")
                ]
            ]
            [ a3Landscape
                []
                [ g
                    [ transform <| Draw.translate 150 100 ++ Draw.scale 1 ]
                    [ draw model.state (Configuration ( 0, 0 ) 0) model.deg ]
                ]
            ]
        , div
            [ style <|
                dib ++
                [ ("width", "45%")
                , ("vertical-align", "top")
                , ("padding", "20px")
                , ("height", "700px")
                , ("overflow-y", "scroll")
                ]
            ]
            (List.map (viewSavedPattern model.deg) model.savedPatterns)
        ]

viewSavedPattern deg pattern =
    div
        [ style <|
            dib ++
            [ ("border", "1px solid black")
            , ("margin", "4px")
            ]
        ]
        [ a6
            [ style <|
                dib ++
                [ ("border", "1px dashed black")
                , ("margin", "10px")
                ]
            ]
            [ g
                [ transform <| Draw.translate 15 10 ++ Draw.scale 2 ]
                [ draw pattern (Configuration (0, 0) 0) deg ]
            ]
        , div
            [ style
                [ ("vertical-align", "top")
                , ("padding", "20px")
                , ("display", "inline-flex")
                , ("flex-direction", "column")
                , ("justify-content", "space-between")
                ]
            ]
            [ button [ onClick <| SelectPattern pattern ] [ text <| "Select" ]
            , button [ onClick <| SetBase pattern ] [ text <| "Base" ]
            , button [ onClick <| Iterate pattern ] [ text <| "Iterate" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RecOnOff ->
            { model | recOn = not model.recOn } ! []
        RecReset ->
            { model | recordedState = [] } ! []
        KeyDown keycode ->
            { model | recordedState = updateRecordedState keycode model.recordedState } ! []
        SetBase pattern ->
            { model | base = pattern, state = pattern } ! []
        SetStep pattern ->
            { model | d = pattern } ! []
        Iterate pattern ->
            let
                newState = apply (rule pattern) model.state
            in
                { model
                    | state = newState
                    , history = model.state :: model.history
                    , length = List.length newState
                } ! []
        ResetSvg ->
            ( { model | state = model.base }, Cmd.none )
        SavePattern ->
            { model | savedPatterns = model.recordedState :: model.savedPatterns } ! []
        SelectPattern pattern ->
            { model | recordedState = pattern } ! []
        RegisterDeg val ->
            { model | rdeg = val } ! []
        SetDeg ->
            { model | deg = Result.withDefault 90 (String.toFloat model.rdeg) } ! []

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


-- TO BE EXTRACTED

apply : (a -> List a) -> List a -> List a
apply rule states =
    List.concatMap (\s -> rule s) states