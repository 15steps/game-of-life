module Game where

import Data.Array
import Control.Applicative(liftA2)

-- ar = listArray (0,399) [(listArray (0,399) [if x `mod` 3 == 0 then Alive else Dead | x <- [0..400]]) | x <- [0..400]]
-- bd = Board { bGrid = ar , bHeight = 400, bWidth = 400 }

-- Data model
type Row = Array Int State
type Grid = Array Int Row

data State = Alive | Dead deriving(Eq)
data Board = Board { bGrid :: Grid, bHeight :: Int, bWidth :: Int } deriving (Show)
data Coord = Coord Int Int deriving (Show, Eq)

-- Not needed since I'll be implementing using Graphics.Gloss
instance Show State where
    show Alive = "■"
    show Dead  = " "

(<+>) :: Coord -> Coord -> Coord
Coord a b <+> Coord c d = Coord (a + c) (b + d)

{-
@Given The current board
@Given A coordinate
@Outputs Number of neighbours of cell in the given coordinate
-}
neighbours :: Board -> Coord -> Int
neighbours board coord = length $ filter ((== Alive) . state board) wrappedCoords
    where
        -- validCoords = filter (validCoord board) coordsAround
        wrappedCoords = map (wrapCoord board) coordsAround
        coordsAround = map (<+> coord) directions
        directions = filter (/= Coord 0 0) $ liftA2 Coord [-1..1] [-1..1]

{-
@Given The current board
@Given A coordinate
@Outputs New coordinate with x and y wrapped around the edges of the board
-}
wrapCoord :: Board -> Coord -> Coord
wrapCoord board (Coord x y) = (Coord (x `mod` bHeight board) (y `mod` bWidth board)) 

{-
@Given The current board
@Given A coordinate
@Outputs Whether the given coord is within the board' height & width
-}
validCoord :: Board -> Coord -> Bool
validCoord board (Coord x y) = xValid && yValid
    where
        xValid = x >= 0 && x < bWidth board
        yValid = y >= 0 && y < bHeight board

{-
@Given The current board
@Given A coordinate
@Outputs The state of the cell in the given coord
-}
state :: Board -> Coord -> State
state board (Coord x y) = bGrid board ! x ! y

{-
@Given The current board
@Given A coordinate
@Outputs The state of the cell in the given coord
-}
nextState :: Board -> Coord -> State
nextState board coord = stepCell currentState bours
    where
        currentState = state board coord
        bours = neighbours board coord

{-
@Given Current state of a cell
@Given Number of neighbours
@Outputs Cell's next state
-}
stepCell :: State -> Int -> State
stepCell Dead 3 = Alive
stepCell Dead _ = Dead
stepCell Alive 3 = Alive
stepCell Alive _ = Dead

{-
@Given The current board
@Outputs New board
-}
step :: Board -> Board
step board = board { bGrid = mapIndices (uncurry updateRow) $ bGrid board }
    where
        updateRow y = mapIndices (nextState board . Coord y . fst)
        mapIndices :: ((Int, a) -> b) -> Array Int a -> Array Int b
        mapIndices f = listToArray . fmap f . assocs 

{-
@Given A list
@Outputs An array form of the list
-}
listToArray :: [a] -> Array Int a
listToArray list = listArray (0, length list - 1) list