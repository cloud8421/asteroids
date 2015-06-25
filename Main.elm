import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Mouse
import Window

type alias Model =
  { x : Float
  , y : Float
  }
  
spaceship : Model
spaceship =
  { x = 0
  , y = 0
  }

spaceshipSprite : Shape
spaceshipSprite =
  ngon 3 20

clearGrey : Color
clearGrey =
  rgba 111 111 111 0.6

draw : Model -> (Int, Int) -> Form
draw spaceship (x,y) =
  spaceshipSprite
  |> filled clearGrey
  |> move(toFloat(x),toFloat(-y))

scene : (Int, Int) -> (Int, Int) -> Element
scene (left,top) shipPosition =
  collage left top
    [ draw spaceship shipPosition]

main : Signal Element
main =
  Signal.map2 scene Window.dimensions Mouse.position
