port module Main exposing (main)

{-| Single Gallery Application to display various examples of generative art.

@docs main

-}

import Example.Rectangles2 as Rectangles2
import Example.Interactive as Interactive
import Generative exposing (..)
import Html
    exposing
        ( Html
        , a
        , article
        , button
        , div
        , input
        , main_
        , nav
        , node
        , p
        , text
        )
import Html.Attributes exposing (class, href, id, style, type_)
import Navigation exposing (Location)
import Tuple exposing (first, mapFirst, mapSecond, second)
import UrlParser
    exposing
        ( Parser
        , oneOf
        , parseHash
        , s
        , top
        )


-- MODEL --


type Route
    = Rectangles2 (Maybe Rectangles2.Model)
    | Interactive (Maybe Interactive.Model)


type alias Model =
    { route : Route
    }


type Msg
    = NavigateTo Location
    | Rectangles2Msg Rectangles2.Msg
    | InteractiveMsg Interactive.Msg


init : Location -> ( Model, Cmd Msg )
init location =
    let
        routeMsg =
            case parseLocation location of
                Rectangles2 _ ->
                    Rectangles2.init
                        |> mapTuple2 (Rectangles2 << Just) (Cmd.map Rectangles2Msg)
                Interactive _ ->
                    Interactive.init
                        |> mapTuple2 (Interactive << Just) (Cmd.map InteractiveMsg)
    in
    ( { route = first routeMsg
      }
    , second routeMsg
    )



-- VIEW --


view : Model -> Html Msg
view model =
    main_
        []
        [ nav
            []
            [ a [ href "#rectangles2" ] [ text "Rectangles2" ]
            , a [ href "#interactive" ] [ text "Interactive LSystem" ]
            ]
        , article
            []
            [ div
                [ class "main" ]
                [ render model.route ]
            ]
        ]


render : Route -> Html Msg
render route =
    case route of
        Rectangles2 (Just pageModel) ->
            Rectangles2.view pageModel
                |> Html.map Rectangles2Msg

        Interactive (Just pageModel) ->
            Interactive.view pageModel
                |> Html.map InteractiveMsg

        _ ->
            text "404 Not Found"



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.route ) of
        ( NavigateTo location, _ ) ->
            init location

        ( msg, route ) ->
            let
                routeMsg =
                    case ( msg, route ) of
                        ( Rectangles2Msg pageMsg, Rectangles2 (Just pageModel) ) ->
                            Rectangles2.update pageMsg pageModel
                                |> mapTuple2 (Rectangles2 << Just) (Cmd.map Rectangles2Msg)

                        _ ->
                            ( route, Cmd.none )
            in
            ( { model | route = first routeMsg }, Cmd.none )



-- PORTS --


port download : String -> Cmd msg



-- SUBSCRIPTIONS --


{--}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
    -- Sub.batch
    --     [ getPlotterStatus PlotterStatus
    --     ]
--}



-- ROUTING --


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ UrlParser.map (Rectangles2 Nothing) (s "rectangles2")
        , UrlParser.map (Interactive Nothing) (s "interactive")
        ]


parseLocation : Location -> Route
parseLocation location =
    case parseHash matchers location of
        Just route ->
            route

        Nothing ->
            Interactive Nothing


{-| Program Entry.
-}
main : Program Never Model Msg
main =
    Navigation.program NavigateTo
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
