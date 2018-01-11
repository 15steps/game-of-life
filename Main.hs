module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Picture

import Data.Array
import Control.Applicative(liftA2)

import System.Random
import Game

main =
    do
    let width = 400
    let height = 400
    let fps = 1
    simulate
        (InWindow "Conway's Game of Life" (width, height) (10, 10)) -- Window settings
        (greyN 0.5) -- Background color
        fps -- Frames per second
        (genesis width height) -- Initial model
        render -- Renderer
        stepModel -- Step function (run every frame)

genesis :: Int -> Int -> Board 
genesis width height = 
    Board { bGrid = grid, bWidth = width, bHeight = height }
    where
        grid = listArray (0,height - 1) [(listArray (0,width - 1) [if x `mod` 3 == 0 then Alive else Dead | x <- [0..width]]) | y <- [0..height]]

stepModel :: ViewPort -> Float -> Board -> Board
stepModel _ _ = step

render :: Board -> Picture
render board 
    = Translate 0 0
    $ Pictures
    $ map (renderCell board) coords
    where
        coords = liftA2 Coord [0..(bWidth board - 1)] [0..(bHeight board - 1)]

renderCell :: Board -> Coord -> Picture
renderCell board coord@(Coord x y) 
    = Color (makeColor 1.0 green 1.0 1.0)
    $ Translate (-200.0 + fromIntegral(x)) (-200.0 + fromIntegral(y))
    $ rectangleSolid 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     0 10
    where
        green = if (bGrid board ! x ! y) == Alive then 0.0 else 1.0
        width = bWidth board
        height = bHeight board