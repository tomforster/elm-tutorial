module Main exposing (main)

import Browser
import Html exposing (Html, Attribute, div, input, text)
import List exposing (map)
import Svg exposing (Svg, svg, circle)
import Svg.Attributes exposing (..)
import Random

main = Browser.element {
    init = init,
    update = update,
    view = view,
    subscriptions = subscriptions }

type alias Model
    = {circle: Maybe CircleDef}

-- MODEL

init : () -> (Model, Cmd Msg)
init _ = (
        {circle = Maybe.Nothing},
        Random.generate CircleMsg (Random.map4 genCircleDef (Random.float 15 100) (Random.float 15 90) (Random.float 5 15) (Random.int 1 255))
    )


-- UPDATE

type Msg = CircleMsg CircleDef

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of CircleMsg circleDef -> ({model | circle = Maybe.Just circleDef}, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

genCircleDef: Float -> Float -> Float -> Int -> CircleDef
genCircleDef x y radius color = {x = x, y = y, radius = radius, color = color}

type alias CircleDef =
    {
    x : Float,
    y : Float,
    radius: Float,
    color: Int
    }

buildCircle : CircleDef -> Svg element
buildCircle circleDef =
    circle [cx (String.fromFloat circleDef.x), cy (String.fromFloat circleDef.y), r (String.fromFloat circleDef.radius), fill ("#" ++ String.fromInt(circleDef.color))] []

-- VIEW

view : Model -> Html Msg
view model =
    svg [style "margin: 1em"] [(buildCircle (Maybe.withDefault {x = 1, y =1, radius = 1, color = 5} model.circle))]