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

spaceshipSprite : Shape
spaceshipSprite =
  ngon 3 20

clearGrey : Color
clearGrey =
  rgba 111 111 111 0.6
       
toDrawableAngle : Float -> Float
toDrawableAngle shipAngle =
  degrees -shipAngle * 3

keyboardInput : Signal (Float, Keys)
keyboardInput =
  let
    delta = Signal.map (\t -> t/2) (fps 60)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
          
updateSpaceship : (Float, Keys) -> Model -> Model
updateSpaceship (time, keys) ship =
  let
    newAngle = ship.angle + toFloat(keys.x)
    newVelocity = ship.velocity + 0.1
  in
    { ship | angle <- newAngle, velocity <- newVelocity }
    
drawShip : Model -> Element
drawShip ship =
  let
    dbg = Debug.watch "ship" ship
  in
    collage 300 300
        [ spaceshipSprite
        |> filled clearGrey
        |> rotate (toDrawableAngle ship.angle) ]
          
spaceshipInput : Signal Model
spaceshipInput =
  Signal.foldp updateSpaceship spaceship keyboardInput

main : Signal Element
main =
  Signal.map drawShip spaceshipInput