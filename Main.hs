module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Data.Array

--player
data Player = PlayerRed | PlayerBlue | PlayerYellow | PlayerGreen deriving (Eq, Show)

--gamestate
data State = Running | GameOver (Maybe Player)

-- if a cell is filled with a character or not
data Cell = Empty | Full Player deriving (Eq, Show)

-- board
type Board = Array (Int, Int) Cell

-- array?
data Game = Game { gameBoard :: Board,
                   gamePlayer :: Player,
                   gameState :: State
                 } 
-- Window
window = InWindow "Ludo" (600, 600) (100,100)

--background color
backgroundColor = white

screenWidth :: Int
screenWidth = 600

screenHeight :: Int 
screenHeight = 600

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

--How large the board is, 15x15
n :: Int
n = 15
{- this is the startboard-}
emptyBoard = Game { gameBoard = (array indexRange $ zip (range indexRange) (cycle [Empty])) // [((0,0), Full PlayerRed),((14,14), Full PlayerBlue)],
                    gamePlayer = PlayerRed,
                    gameState = Running
                  }
            where indexRange = ((0,0), (n-1, n-1))

-- if the game is still running
boardAsRunningPicture board = 
    pictures [ color red $ redCellsOfBoard board,
               color blue $ blueCellsOfBoard board,
               color yellow $ yellowCellsOfBoard board,
               color green $ greenCellsOfBoard board,
               color black $ boardGrid]

-- colors for gameover
outcomeColor (Just PlayerRed) = red
outcomeColor (Just PlayerBlue) = blue
outcomeColor (Just PlayerYellow) = yellow
outcomeColor (Just PlayerGreen) = green
outcomeColor Nothing = white

-- put the pieces in the cell
snapPictureToCell picture (row, column) = translate x y picture 
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5

redCell :: Picture
redCell = thickCircle 1.0 radius
    where radius = min cellWidth cellHeight * 0.75

blueCell :: Picture
blueCell = thickCircle 1.0 radius
    where radius = min cellWidth cellHeight * 0.75

yellowCell :: Picture
yellowCell = thickCircle 1.0 radius
    where radius = min cellWidth cellHeight * 0.75

greenCell :: Picture
greenCell = thickCircle 1.0 radius
    where radius = min cellWidth cellHeight * 0.75

-- the cells of the board
cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture = pictures 
    $ map (snapPictureToCell cellPicture . fst) 
    $ filter (\(_, e) -> e == cell) 
    $ assocs board

--makes the cells the color of the player
redCellsOfBoard :: Board -> Picture
redCellsOfBoard board = cellsOfBoard board (Full PlayerRed) redCell

blueCellsOfBoard :: Board -> Picture
blueCellsOfBoard board = cellsOfBoard board (Full PlayerBlue) blueCell

yellowCellsOfBoard :: Board -> Picture
yellowCellsOfBoard board = cellsOfBoard board (Full PlayerYellow) yellowCell

greenCellsOfBoard :: Board -> Picture
greenCellsOfBoard board = cellsOfBoard board (Full PlayerGreen) greenCell

--Board grid
boardGrid :: Picture
boardGrid = pictures $ concatMap (\i -> [ line [ (i * cellWidth, 0.0),
                                                 (i * cellWidth, fromIntegral screenHeight)],
                                          line [ (0.0, i * cellHeight),
                                                 (fromIntegral screenWidth, i * cellHeight)]])
                                                 [0.0 .. fromIntegral n]

-- layers for each player and the grid
boardAsPicture board = 
    pictures [ redCellsOfBoard board,
               blueCellsOfBoard board,
               yellowCellsOfBoard board,
               greenCellsOfBoard board,
               boardGrid
             ]

-- if the game is over
boardAsGameOverPicture winner board = color (outcomeColor winner) (boardAsPicture board)


--Translate the board
gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
   where frame = case gameState game of 
            Running -> boardAsRunningPicture (gameBoard game)
            GameOver winner -> boardAsGameOverPicture winner (gameBoard game)

transformGame _ game = game

main :: IO ()
main = play window backgroundColor 30 emptyBoard gameAsPicture transformGame (const id)