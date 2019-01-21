module Main exposing (Model, Msg(..), coordinates, coordinatesString, init, main, pointsInString, roundRect, update, view)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)
import List exposing (foldr, map, range)
import Round exposing (round)
import Svg exposing (polyline, svg)
import Svg.Attributes exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { n : Int }


init : Model
init =
    { n = 6 }


coordinates : Int -> Float -> Float -> Float -> List ( Float, Float )
coordinates n radius x y =
    map
        (\i ->
            ( x + radius * sin (turns i / toFloat n)
            , y + radius * cos (turns i / toFloat n)
            )
        )
        (map toFloat (range 0 n))



-- UPDATE


type Msg
    = IncrementN
    | DecrementN


update : Msg -> Model -> Model
update msg model =
    case msg of
        IncrementN ->
            { model | n = model.n + 1 }

        DecrementN ->
            { model | n = model.n - 1 }



-- VIEW


coordinatesString : Int -> String
coordinatesString n =
    String.join " "
        (map
            (\p ->
                round 2 (Tuple.first p)
                    ++ ","
                    ++ round 2 (Tuple.second p)
            )
            -- initial data in view - bad sign or it's view?
            (coordinates n 50 50 50)
        )


pointsInString : Model -> String
pointsInString model =
    foldr (++) "" (map String.fromInt (range 0 model.n))


roundRect : Model -> Html.Html msg
roundRect model =
    svg
        [ width "400", height "400", viewBox "0 0 100 100" ]
        [ polyline [ fill "none", stroke "black", points (coordinatesString model.n) ] [] ]


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick DecrementN ] [ text "-" ]
            , span [] [ text (String.fromInt model.n) ]
            , button [ onClick IncrementN ] [ text "+" ]
            ]
        , roundRect model
        ]
