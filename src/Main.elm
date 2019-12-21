module Main exposing (main)

import Browser exposing (Document)
import Svg exposing (Svg, svg, circle)
import Html exposing (Html)
import Svg.Attributes exposing (..)
import Random
import Time

main = Browser.document
    {
        init = init,
        update = update,
        view = view,
        subscriptions = subscriptions
    }

type alias Model =
    {
        circles: List CircleDef
    }

type alias CircleDef =
    {
        x : Float,
        y : Float,
        radius: Float,
        color: Int
    }

type alias Document msg =
    {
        title : String,
        body : List (Html msg)
    }

type Msg = CircleMsg CircleDef | Tick Time.Posix

-- MODEL

init : () -> (Model, Cmd Msg)
init _ = (
        {circles = []},
        makeCircleMsg
    )

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CircleMsg circleDef ->
            if List.length model.circles < 100
                then ({model | circles = model.circles ++ [circleDef]}, Cmd.none)
            else ({model | circles = Maybe.withDefault [] (List.tail model.circles) ++ [circleDef]}, Cmd.none)

        Tick _ -> (model, makeCircleMsg)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Time.every 50 Tick

-- VIEW

view : Model -> Document Msg
view model =
    {body = [
        svg [style "width:100%; height:100%"] (List.map buildCircle model.circles)
    ], title = "Demo"}

genCircleDef: Float -> Float -> Float -> Int -> CircleDef
genCircleDef x y radius color = {x = x, y = y, radius = radius, color = color}

makeCircleMsg : Cmd Msg
makeCircleMsg = Random.generate CircleMsg (Random.map4 genCircleDef (Random.float 0 100) (Random.float 0 100) (Random.float 15 160) (Random.int 0 255))

buildCircle : CircleDef -> Svg element
buildCircle circleDef =
    circle [cx (String.fromFloat circleDef.x ++ "%"), cy (String.fromFloat circleDef.y ++ "%"), r (String.fromFloat circleDef.radius), fill ("hsl(" ++ String.fromInt(circleDef.color) ++ ", 100%, 50%)")] []