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
  { rotation : Float
  , thrust : Float
  , x : Float
  , y : Float
  }
  
spaceship : Model
spaceship =
  { rotation = 0.0
  , thrust = 0.0
  , x = 1.0
  , y = 1.0
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
           
updateThrust : Int -> Float -> Float
updateThrust y old =
  case y of
    1    -> 1 + old * 1.005
    (-1) -> old * 0.5
    0    -> old * 0.9
            
updateX : Float -> Float -> Float -> Float
updateX rotation thrust x =
  x + (cos rotation) * thrust 
    
updateY : Float -> Float -> Float -> Float
updateY rotation thrust y =
  y + (sin rotation) * thrust
    
updateRotation rotation x =
  case x of
    1    -> rotation - (degrees 6)
    (-1) -> rotation + (degrees 6)
    0    -> rotation

updateSpaceship : (Float, Keys) -> Model -> Model
updateSpaceship (time, keys) ship =
  let
    newThrust   = updateThrust keys.y ship.thrust
    newX        = updateX ship.rotation newThrust ship.x
    newY        = updateY ship.rotation newThrust ship.y
    newRotation = updateRotation ship.rotation keys.x
  in
    { ship | rotation <- newRotation
           , thrust <- newThrust
           , x <- newX
           , y <- newY
    }

drawScene : (Int, Int) -> Model -> Element
drawScene (w, h) ship =
  let
    dbg = Debug.watch "ship" ship
  in
    collage w h
      [ spaceSprite w h
      , spaceshipSprite
        |> rotate ship.rotation
        |> move (ship.x,ship.y)
      ]

keyboardInput : Signal (Float, Keys)
keyboardInput =
  let
    delta = Signal.map (\t -> t/2) (fps 10)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)

spaceshipInput : Signal Model
spaceshipInput =
  Signal.foldp updateSpaceship spaceship keyboardInput

main : Signal Element
main =
  Signal.map2 drawScene Window.dimensions spaceshipInput