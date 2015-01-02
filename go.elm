module ElmGo where

-- Imports
import Array (Array, indexedMap, fromList, toList, get, set,repeat)
import Color (..)
import Debug
import Graphics.Collage (..)
import Graphics.Element (..)
import List (map,(::))
import Mouse
import Signal as S
import Signal (Signal, (<~), constant, sampleOn, keepIf, foldp)
import Text (asText)
import Transform2D (matrix)


-- Game Parameters
size : Int
size = 9

scale : Float
scale = 25

border : Int
border = 15


---- MODEL ----
type alias State =
  { board  : Board
  , toPlay : Player
  }

type alias Board = Array Intersection

type Intersection = Empty
                  | Stone Player

type Player = Black | White

initialState : State
initialState =
  { board = repeat (size * size) Empty
  , toPlay = Black }


---- UPDATE ----
type Update
    = PlayStone Point

type alias Point = (Int,Int)

updateState : Update -> State -> State
updateState u {board, toPlay} = let
    valid : Point -> Bool
    valid p = free p && notSuicide p && notKo p
    free (i,j) = get (i*size+j) board == Just Empty
    notSuicide p = True --TODO
    notKo p = True --TODO
  in
    case u of
      PlayStone p -> 
        { board  = addStone toPlay p board
        , toPlay = turn toPlay}

addStone : Player -> Point -> Board -> Board
addStone p (i,j) board = case get (i*size+j) board of
                           Just Empty -> set (i*size+j) (Stone p) board
                           _          -> board

turn : Player -> Player
turn p = case p of
  Black -> White
  White -> Black


---- VIEV ----
draw : State -> Element
draw {board, toPlay} = let
    bSize = scale * toFloat (size-1)
    eSize = 2 * round scale * size + 2 * border +1
    drawInter idx inter =
      move
      (2 * scale * toFloat (idx % size) - bSize,
       bSize - 2 * scale * toFloat (idx // size))
      (case inter of
          Empty       -> group []
          Stone Black -> filled black (circle <| scale-1)
          Stone White -> filled white (circle <| scale-1))
    stones = group <| toList <| indexedMap drawInter board
    hLines = map (\i -> segment (0, 2*scale*toFloat i)
                  (2*scale*toFloat (size-1), 2*scale*toFloat i))
                 [0..size-1]
             |> map (traced (solid charcoal))
    vLines = groupTransform (matrix 0 1 1 0 0 0) hLines
    grid = group (vLines :: hLines)
           |> move (-scale*toFloat (size-1),-scale*toFloat (size-1))
  in
    color (rgb 245 222 179)
    <| collage eSize eSize [grid, stones]


---- INPUT ----
main : Signal Element
main = draw <~ state

state : Signal State
state = foldp updateState initialState mousePlay

mousePlay : Signal Update
mousePlay = let pps = 2 * round scale in
  sampleOn Mouse.clicks Mouse.position
  |> S.map (\ (x,y) -> (x - border, y - border))     
  |> keepIf (\ (x,y) -> x >= 0 && x <= pps * size &&
                        y >= 0 && y <= pps * size) (0,0)
  |> S.map (\ (x,y) -> PlayStone (y // pps,x // pps)) -- y -> i (row), x -> j (col)
