import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Mouse

player = {x=0.0, y=0.0}

clearGrey : Color
clearGrey =
  rgba 111 111 111 0.6

draw : { x : Float, y : Float } -> Form
draw spaceship =
  ngon 3 75
  |> filled clearGrey
  |> move(spaceship.x,spaceship.y)

scene : (Int, Int) -> Element
scene (left,top) =
  collage left top
    [ draw player ]

main : Signal Element
main =
  Signal.map scene Mouse.position
