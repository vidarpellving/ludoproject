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

-- this is a "Record" and creates variables for the type Game 
-- line 21 is equal, (almost) to data Game = Game Board Player State
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
{- 
    this is the startboard
    Array takes and a range named which is defined as indexRange on line 58 and a list [((0,0),Empty),((0,1),Empty)..((14,14),Empty)] 
    and makes it an array.
    range indexRange makes a list filled with tuples, [(0,0),(0,1)..(14,14)].
    cycle [Empty] makes an infinetly long list of the type Empty.
    zip takes two lists and combines the first second .. and last elemnts from both lists to create tuples. [(x1,y1),(x2,y2)..(xn,yn)].
    (//) takes an array and a new list to add to it, first it takes a position and then the value, in this case [((0,0), Full PlayerRed)].

-}
emptyBoard = Game { gameBoard = (array indexRange $ zip (range indexRange) (cycle [Empty])) // [((3,3), Full PlayerRed), 
                                                                                                ((3,2), Full PlayerRed),
                                                                                                ((2,2), Full PlayerRed),
                                                                                                ((2,3), Full PlayerRed),
                                                                                                ((3,12), Full PlayerBlue),
                                                                                                ((2,12), Full PlayerBlue),
                                                                                                ((3,11), Full PlayerBlue),
                                                                                                ((2,11), Full PlayerBlue),
                                                                                                ((11,11), Full PlayerYellow),
                                                                                                ((11,12), Full PlayerYellow),
                                                                                                ((12,11), Full PlayerYellow),
                                                                                                ((12,12), Full PlayerYellow),
                                                                                                ((12,2), Full PlayerGreen),
                                                                                                ((12,3), Full PlayerGreen),
                                                                                                ((11,2), Full PlayerGreen),
                                                                                                ((11,3), Full PlayerGreen),
                                                                                                ((1,6), Full PlayerRed)],
                    gamePlayer = PlayerRed,
                    gameState = Running
                  }
            -- This is used to define how large the array created will be
            where indexRange = ((0,0), (n-1, n-1))
-- if the game is still running
{-
    This function displays the different layers when the game is running
    Determines what layers are on the bottom and on the top.
-}
boardAsRunningPicture board = 
    pictures [ color red $ redCorner,
               color blue $ blueCorner,
               color yellow $ yellowCorner,
               color green $ greenCorner,
               color white $ whiteSquare,
               color red $ redCellsOfBoard board,
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

{-
    This function makes all the lines for the grid
    pictures makes takes a list of picture and creates a single picture
    concatMap is a combination of map and concat
    in this case it useses the function (\i -> [ line [ (i * cellWidth, 0.0),
                                                 (i * cellWidth, fromIntegral screenHeight)],
                                          line [ (0.0, i * cellHeight),
                                                 (fromIntegral screenWidth, i * cellHeight)]])
                                                 on the list [0.0 .. fromIntegral n] which is a list of float [0.0,1.0 .. 15.0]
-}
boardGrid :: Picture
boardGrid = pictures $ concatMap (\i -> [ line [ (i * cellWidth, 0.0),
                                                 (i * cellWidth, fromIntegral screenHeight)],
                                          line [ (0.0, i * cellHeight),
                                                 (fromIntegral screenWidth, i * cellHeight)]])
                                                 [0.0 .. fromIntegral n]
-- Only works when the gameState = GameOver
redCorner :: Picture
redCorner = pictures [translate (120) (120) (rectangleSolid 240 240),
                      translate (260) (60) (rectangleSolid 40 40),
                      translate (300) (160) (rectangleSolid 40 240)]

blueCorner :: Picture
blueCorner = pictures [translate (480) (120) (rectangleSolid 240 240),
                       translate (540) (260) (rectangleSolid 40 40),
                       translate (440) (300) (rectangleSolid 240 40)]

yellowCorner :: Picture
yellowCorner = pictures [translate (480) (480) (rectangleSolid 240 240),
                         translate (340) (540) (rectangleSolid 40 40),
                         translate (300) (440) (rectangleSolid 40 240)]

greenCorner :: Picture
greenCorner = pictures [translate (120) (480) (rectangleSolid 240 240),
                        translate (60) (340) (rectangleSolid 40 40),
                        translate (160) (300) (rectangleSolid 240 40)]

whiteSquare :: Picture
whiteSquare = pictures [translate (120) (120) (rectangleSolid 160 160),
                        translate (480) (120) (rectangleSolid 160 160),
                        translate (120) (480) (rectangleSolid 160 160),
                        translate (480) (480) (rectangleSolid 160 160)]

{- 
    layers for each player and the grid.
    each color has a function for displaying itself
    boardGrid draws the lines for the grid
-}
boardAsPicture board = 
    pictures [ redCellsOfBoard board,
               blueCellsOfBoard board,
               yellowCellsOfBoard board,
               greenCellsOfBoard board,
               blueCorner,
               yellowCorner,
               greenCorner,
               redCorner,
               whiteSquare,
               boardGrid
             ]

{-
    Color takes two arguments Color and a board.
    outcomeColor winner gets the color of the winner.
    boardAsPicture board draws the board
-}
boardAsGameOverPicture winner board = color (outcomeColor winner) (boardAsPicture board)


--Translate the board, this is the way to handle events.
gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
   where frame = case gameState game of 
        -- gameBoard game is a way to get the value of gameBoard from the datatype Game
            Running -> boardAsRunningPicture (gameBoard game)
            GameOver winner -> boardAsGameOverPicture winner (gameBoard game)

transformGame _ game = game

main :: IO ()
main = play window backgroundColor 30 emptyBoard gameAsPicture transformGame (const id)