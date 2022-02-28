module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Data.Array
import Graphics.Gloss.Interface.IO.Game (Event(EventKey), MouseButton (LeftButton))
import Graphics.Gloss.Interface.Pure.Game


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

{-
validPositions = [(0,8),(0,7),(0,6),(1,6),(2,6),(3,6),(4,6),(5,6),(6,6),(6,5),(6,4),(6,3),(6,2),(6,1),(6,0),(7,0),(8,0),(8,1),(8,2),(8,3),(8,4),(8,5),(8,6),
                  (9,6),(10,6),(11,6),(12,6),(13,6),(14,6),(14,7),(14,8),(13,8),(12,8),(11,8),(10,8),(9,8),(8,8),(8,9),(8,10),(8,11),(8,12),(8,13),(8,14),
                  (7,14),(6,14),(6,13),(6,12),(6,11),(6,10),(6,9--nice),(6,8),(5,8),(4,8),(3,8),(2,8),(1,8)]
goalSquare = [(7,7)]                  

-}
emptyBoard = Game { gameBoard = (array indexRange $ zip (range indexRange) (cycle [Empty])) // [((0,0), Full PlayerRed),((14,14), Full PlayerBlue)],
                    gamePlayer = PlayerRed,
                    gameState = Running
                  }
            where indexRange = ((0,0), (n-1, n-1))

-- if the game is still running
boardAsRunningPicture board = 
    pictures [ color red $ redCellsOfBoard board,
               color blue $ blueCellsOfBoard board,
               color yellow $ yellowCellsOfBoard board,
               color green $ greenCellsOfBoard board,
               color black $  boardGrid]

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

isCoordCorrect = inRange ((0, 0), (n-1,n-1))

playerSwitch game =
    case gamePlayer game of
        PlayerRed -> game {gamePlayer = PlayerGreen}
        PlayerGreen -> game {gamePlayer = PlayerYellow}
        PlayerYellow -> game {gamePlayer = PlayerBlue}
        PlayerBlue -> game {gamePlayer = PlayerRed}

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    | isCoordCorrect cellCoord && board ! cellCoord == Empty = 
        playerSwitch $ game { gameBoard = board // [(cellCoord, Full $ player)]} 
    | otherwise = game 
    where board = gameBoard game
          player = gamePlayer game 

mousePosCell :: (Float,Float) -> (Int,Int)
mousePosCell (x,y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                     , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth )
                     )

transformGame (EventKey(MouseButton LeftButton) Up _ mousePos) game = 
    case gameState game of 
        Running -> playerTurn game $ mousePosCell mousePos 
        GameOver _ -> emptyBoard
transformGame _ game = game

main :: IO ()
main = play window backgroundColor 30 emptyBoard gameAsPicture transformGame (const id)