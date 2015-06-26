import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Time exposing (..)
import Color exposing (..)
import Mouse
import Keyboard
import Window
import Debug

type alias Keys = { x:Int, y:Int }

type alias Model =
  { angle : Float
  , velocity : Float
  }

spaceship : Model
spaceship =
  { angle = 0.0
  , velocity = 0.0
  }

spaceSprite : Int -> Int -> Form
spaceSprite w' h' =
  let
    w = toFloat(w')
    h = toFloat(h')
  in
    filled black (rect w h)

spaceshipSprite : Form
spaceshipSprite =
  outlined (solid white) (ngon 3 20)

updateSpaceship : (Float, Keys) -> Model -> Model
updateSpaceship (time, keys) ship =
  let
    newAngle = ship.angle + toFloat(keys.x)
    newVelocity = ship.velocity + 0.1
  in
    { ship | angle <- newAngle, velocity <- newVelocity }

drawScene : (Int, Int) -> Model -> Element
drawScene (w, h) ship =
  let
    dbg = Debug.watch "ship" ship
  in
    collage w h
      [ spaceSprite w h
      , spaceshipSprite
        |> rotate (degrees -ship.angle * 3)
      ]

keyboardInput : Signal (Float, Keys)
keyboardInput =
  let
    delta = Signal.map (\t -> t/2) (fps 60)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)

spaceshipInput : Signal Model
spaceshipInput =
  Signal.foldp updateSpaceship spaceship keyboardInput

main : Signal Element
main =
  Signal.map2 drawScene Window.dimensions spaceshipInput