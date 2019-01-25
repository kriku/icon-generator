module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (get, initialize, set)
import Browser
import Dict exposing (..)
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import List exposing (foldr, map, range)
import Round exposing (round)
import Svg exposing (polyline, svg)
import Svg.Attributes exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { polygons : Dict Int Polygon }


type alias Polygon =
    { n : Int
    , vector : Point
    , position : Point
    }


type alias Point =
    ( Float, Float )


init : Model
init =
    { polygons =
        Dict.fromList
            [ ( 0
              , { n = 6
                , vector = ( 40, 30 )
                , position = ( 50, 50 )
                }
              )
            , ( 1
              , { n = 6
                , vector = ( 40, 0 )
                , position = ( 50, 50 )
                }
              )
            ]
    }



-- vector means ( radius, alpha )


polygon : Int -> Point -> Point -> List Point
polygon n ( radius, alpha ) ( x, y ) =
    map
        (\i ->
            ( x + radius * sin (degrees alpha + turns i / toFloat n)
            , y + radius * cos (degrees alpha + turns i / toFloat n)
            )
        )
        (map toFloat (range 0 n))



-- UPDATE


type Msg
    = ChangeN Int Int
    | ChangeRadius Int String
    | ChangeAlpha Int String
    | AddPolygon
    | RemovePolygon Int


addPolygon : Model -> Model
addPolygon model =
    { model
        | polygons =
            Dict.insert
                (Dict.size model.polygons)
                { n = 6
                , vector = ( 40, 0 )
                , position = ( 50, 50 )
                }
                model.polygons
    }


removePolygon : Int -> Model -> Model
removePolygon key model =
    { model
        | polygons =
            Dict.remove
                key
                model.polygons
    }


changeN : Int -> Int -> Model -> Model
changeN index value model =
    let
        updatePolygon =
            Maybe.map (\polygonData -> { polygonData | n = value })

        polygonsUpdated =
            Dict.update index
                updatePolygon
                model.polygons
    in
    { model | polygons = polygonsUpdated }


sanitize : String -> Float
sanitize input =
    input |> String.toFloat |> Maybe.withDefault 0


changeRadius : Int -> String -> Model -> Model
changeRadius index value model =
    let
        updatePolygon =
            Maybe.map
                (\p ->
                    { p
                        | vector =
                            ( sanitize value
                            , Tuple.second p.vector
                            )
                    }
                )

        polygonsUpdated =
            Dict.update index
                updatePolygon
                model.polygons
    in
    { model | polygons = polygonsUpdated }


changeAlpha : Int -> String -> Model -> Model
changeAlpha index value model =
    let
        updatePolygon =
            Maybe.map
                (\p ->
                    { p
                        | vector =
                            ( Tuple.first p.vector
                            , sanitize value
                            )
                    }
                )

        polygonsUpdated =
            Dict.update index
                updatePolygon
                model.polygons
    in
    { model | polygons = polygonsUpdated }

update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeN index n ->
            changeN index n model

        AddPolygon ->
            addPolygon model

        RemovePolygon key ->
            removePolygon key model

        ChangeRadius index radius ->
            changeRadius index radius model

        ChangeAlpha index alpha ->
            changeAlpha index alpha model



-- VIEW


toPolygonString : List Point -> String
toPolygonString polygons =
    String.join " "
        (map
            (\p ->
                round 2 (Tuple.first p)
                    ++ ","
                    ++ round 2 (Tuple.second p)
            )
            -- initial data in view - bad sign or it's view?
            polygons
        )


polygonsChanger : Model -> Html Msg
polygonsChanger model =
    let
        selectedPolygon ( key, data ) =
            div []
                [ button [ onClick (ChangeN key (data.n - 1)) ] [ text "-" ]
                , span [] [ text (String.fromInt data.n) ]
                , button [ onClick (ChangeN key (data.n + 1)) ] [ text "+" ]
                , input
                    [ placeholder "radius"
                    , value
                        (data.vector |> Tuple.first |> round 2)
                    , onInput (ChangeRadius key)
                    ]
                    []
                , input
                    [ placeholder "alpha"
                    , value
                        (data.vector |> Tuple.second |> round 2)
                    , onInput (ChangeAlpha key)
                    ]
                    []
                , button [ onClick (RemovePolygon key) ] [ text "delete" ]
                ]
    in
    div []
        (model.polygons
            |> Dict.toList
            |> List.map selectedPolygon
        )


producedSvg : Model -> Html.Html msg
producedSvg model =
    let
        renderedPolygon ( key, p ) =
            polyline
                [ fill "none"
                , stroke "gray"
                , points
                    (toPolygonString
                        (polygon
                            p.n
                            p.vector
                            p.position
                        )
                    )
                ]
                []
    in
    svg
        [ width "400", height "400", viewBox "0 0 100 100" ]
        (model.polygons
            |> Dict.toList
            |> List.map renderedPolygon
        )


view : Model -> Html Msg
view model =
    div []
        [ polygonsChanger model
        , producedSvg model
        , button [ onClick AddPolygon ] [ text "+" ]
        ]
