module ElmGo where

-- Imports
import Array exposing (Array, indexedMap, fromList, toList, get, set, repeat)
import Color exposing (..)
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (map,(::), foldl, any)
import Mouse
import Set exposing (Set, insert, member)
import Signal as S
import Signal exposing (Signal, constant, sampleOn, filter, foldp)
import Transform2D exposing (matrix)


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

-- get all four adjacent points (potentially off-board)
neighbours : Point -> List Point
neighbours (i,j) = [(i+1,j), (i,j+1), (i-1,j), (i,j-1)]

-- convert from point to intdex into the arry
idx : Point -> Int
idx (i,j) = i*size+j

-- convert from an index back to point
xdi : Int -> Point
xdi a = (a // size, a % size)

-- checks whether a point is within the boundaries of the board
onBoard : Point -> Bool
onBoard (i,j) = i >= 0 && i < size && j >= 0 && j < size

-- return a set of all the points in the same group as p
getGroup : Board -> Point -> Set Int
getGroup board p =
  case get (idx p) board of
    Just (Stone c) ->
      let
        go : Point -> Set Int -> Set Int
        go q visited =
          if not <| onBoard q ||
             get (idx q) board /= Just (Stone c) ||
             member (idx q) visited
          then visited
          else foldl go (insert (idx q) visited) (neighbours q)
      in go p Set.empty
    _ -> Set.empty

-- checks whether a given point has any libertis left
hasLiberties : Board -> Point -> Bool
hasLiberties board p =
  let
    group = getGroup board (i,j)
    directLiberties : Point -> Bool
    directLiberties q = neighbours q |> any free
    free : Point -> Bool
    free q = onBoard q && get (idx q) board == Just Empty
  in List.any (dxi >> directLiberties) (Set.toList group)

updateState : Update -> State -> State
updateState update {board, toPlay} =
  case update of
    PlayStone (i,j) ->
      let
        next = addStone toPlay (i,j) board

        valid = True
      in
        if valid
        then { board  = next
             , toPlay = turn toPlay}
        else {board = board, toPlay = toPlay}

addStone : Player -> Point -> Board -> Board
addStone p (i,j) board = case get (i*size+j) board of
                           Just Empty -> set (i*size+j) (Stone p) board
                           _          -> board

turn : Player -> Player
turn p = case p of
  Black -> White
  White -> Black


---- VIEV ----
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

hLines = map (\i -> traced (solid charcoal) (segment (0, 2*scale*toFloat i)
                    (2*bSize, 2*scale*toFloat i)))
         [0..size-1]

vLines = groupTransform (matrix 0 1 1 0 0 0) hLines

grid = group (vLines :: hLines)
     |> move (-bSize,-bSize)

drawBoard : State -> Element
drawBoard {board, toPlay} = let
    stones = group <| toList <| indexedMap drawInter board
  in
    color (rgb 245 222 179)
    <| collage eSize eSize [grid, stones]


---- INPUT ----
main : Signal Element
main = S.map drawBoard state

state : Signal State
state = foldp updateState initialState mousePlay

mousePlay : Signal Update
mousePlay = let pps = 2 * round scale in
  sampleOn Mouse.clicks Mouse.position
  |> S.map (\ (x,y) -> (x - border, y - border))
  |> filter (\ (x,y) -> x >= 0 && x <= pps * size &&
                        y >= 0 && y <= pps * size) (0,0)
  |> S.map (\ (x,y) -> PlayStone (y // pps,x // pps)) -- y -> i (row), x -> j (col)
