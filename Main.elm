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
  , x : Float
  , y : Float
  }
  
spaceship : Model
spaceship =
  { angle = 0.0
  , velocity = 0.0
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
           
updateVelocity : Float -> Int -> Float
updateVelocity old y =
  if | y > 0     -> old + 1
     | y < 0     -> old - 1
     | otherwise -> old

updateX : Float -> Float -> Float -> Float
updateX x angle velocity =
  x + velocity / 10 * (angle |> degrees |> cos)
       
updateY : Float -> Float -> Float -> Float
updateY y angle velocity =
  y + velocity / -10 * (angle |> degrees |> sin)

updatePosition : Model -> Float -> (Float, Float)
updatePosition ship velocity =
  let
    newX = (updateX ship.x ship.angle velocity)
    newY = (updateY ship.y ship.angle velocity)
  in
    (newX, newY)

updateSpaceship : (Float, Keys) -> Model -> Model
updateSpaceship (time, keys) ship =
  let
    dbg = Debug.watch "keys" keys
    newAngle = ship.angle + toFloat(keys.x)
    newVelocity = updateVelocity ship.velocity keys.y
    (newX, newY) = updatePosition ship newVelocity
  in
    { ship | angle <- newAngle
    , velocity <- newVelocity
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
        |> rotate (degrees -ship.angle * 3)
        |> move (ship.x,ship.y)
      ]
    
keyboardInput : Signal (Float, Keys)
keyboardInput =
  let
    delta = Signal.map (\t -> t/2) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)

spaceshipInput : Signal Model
spaceshipInput =
  Signal.foldp updateSpaceship spaceship keyboardInput

main : Signal Element
main =
  Signal.map2 drawScene Window.dimensions spaceshipInput