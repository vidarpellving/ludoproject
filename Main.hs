module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Data.Array
import Graphics.Gloss.Interface.IO.Game (Event(EventKey), MouseButton (LeftButton))
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.IO
import Data.List
import Data.Time

--player
data Player = PlayerRed | PlayerBlue | PlayerYellow | PlayerGreen deriving (Eq, Show)

--gamestate
data State = Running | GameOver (Maybe Player)

-- if a cell is filled with a character or not
data Cell = Empty | Full Player deriving (Eq, Show)

data Dice = Void | Dice Int deriving (Eq, Show)

data Move = None | Piece (Int, Int) deriving (Eq, Show)

-- board
type Board = Array (Int, Int) Cell

-- this is a "Record" and creates variables for the type Game 
-- line 21 is equal, (almost) to data Game = Game Board Player State

data Game = Game { gameBoard :: Board,
                   gamePlayer :: Player,
                   gameState :: State,
                   rnd :: [Float],
                   dice :: Dice,
                   diceUpdate :: Bool,
                   pieceMove :: Move
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

--Coordinates and curcial points; validPositions, goalSquare, winColor, entryPoint
--Total 56 possible positions before winColorRow(which contains 7 positions.)
validPositions :: [(Int,Int)]
validPositions = [(6,2),(6,1),(6,0),(7,0),(8,0),(8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(9,6),(10,6),(11,6),
                  (12,6),(13,6),(14,6),(14,7),(14,8),(13,8),(12,8),(11,8),(10,8),(9,8),(8,8),(8,9),(8,10),(8,11),
                  (8,12),(8,13),(8,14),(7,14),(6,14),(6,13),(6,12),(6,11),(6,10),(6,9),(6,8),(5,8),(4,8),(3,8),
                  (2,8),(1,8),(0,8),(0,7),(0,6),(1,6),(2,6),(3,6),(4,6),(5,6),(6,6),(6,5),(6,4),(6,3)]

--dice position
dicePos = [(9,4),(9,5),(10,4),(10,5)]


goalSquare = [(7,7)]      
-- spawnpoints for all the colors
redSpawn = [(2,2),(3,2),(3,3),(2,3)]
blueSpawn = [(2,11),(3,11),(3,12),(2,12)]
greenSpawn = [(11,2),(12,2),(12,3),(11,3)]
yellowSpawn = [(11,11),(12,11),(12,12),(11,12)]


-- the squares that are limited to their colors
winColorGreen = [(8,1),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6)]
winColorYellow = [(13,8),(13,7),(12,7),(11,7),(10,7),(9,7),(8,7)]
winColorBlue = [(6,13),(7,13),(7,12),(7,11),(7,10),(7,9),(7,8)]
winColorRed = [(1,6),(1,7),(2,7),(3,7),(4,7),(5,7),(6,7)]

--entrypoints for all colors
entryPoints :: [(Cell, (Int,Int))]
entryPoints = [(Full PlayerRed,(6,2)),(Full PlayerGreen,(12,6)),(Full PlayerYellow,(8,12)),(Full PlayerBlue,(2,8))]

getPlayerStart :: Player -> [(Cell, (Int,Int))] -> (Int,Int)
getPlayerStart _ [] = (0,0)
getPlayerStart player ((x,y):xs) | Full player == x = y
                                 | otherwise = getPlayerStart player xs


{- 
    this is the startboard
    Array takes and a range named which is defined as indexRange on line 58 and a list [((0,0),Empty),((0,1),Empty)..((14,14),Empty)] 
    and makes it an array.
    range indexRange makes a list filled with tuples, [(0,0),(0,1)..(14,14)].
    cycle [Empty] makes an infinetly long list of the type Empty.
    zip takes two lists and combines the first second .. and last elemnts from both lists to create tuples. [(x1,y1),(x2,y2)..(xn,yn)].
    (//) takes an array and a new list to add to it, first it takes a position and then the value, in this case [((0,0), Full PlayerRed)].

-}
emptyBoard = Game { gameBoard = array indexRange (zip (range indexRange) (repeat Empty)) // [((3,3), Full PlayerRed),
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
                                                                                                ((11,3), Full PlayerGreen)],


                    gamePlayer = PlayerRed,
                    gameState = Running,
                    rnd = randoms (mkStdGen 42),
                    dice = Void,
                    diceUpdate = False,
                    pieceMove = None
                  }
            -- This is used to define how large the array created will be
            where indexRange = ((0,0), (n-1, n-1))
-- if the game is still running
{-
    This function displays the different layers when the game is running
    Determines what layers are on the bottom and on the top.
-}

diceGen :: Dice -> Picture
diceGen Void = color black visualDiceBG
diceGen dice | dice == Dice 1 = color white diceValue1
             | dice == Dice 2 = color white diceValue2
             | dice == Dice 3 = color white diceValue3
             | dice == Dice 4 = color white diceValue4
             | dice == Dice 5 = color white diceValue5
             | dice == Dice 6 = color white diceValue6
             | otherwise = color black visualDiceBG

boardAsRunningPicture :: Board -> Dice -> Picture
boardAsRunningPicture board dice =
    pictures [ color red redCorner,
               color blue blueCorner,
               color yellow yellowCorner,
               color green greenCorner,
               color white whiteSquare,
               redBoarder board,
               blueBoarder board,
               yellowBoarder board,
               greenBoarder board,
               redCellsOfBoard board,
               blueCellsOfBoard board,
               yellowCellsOfBoard board,
               greenCellsOfBoard board,
               color black visualDiceBG,
               color black boardGrid,
               diceGen dice]

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
redCell = color red $ thickCircle 1.0 radius
    where radius = min cellWidth cellHeight * 0.75

blueCell :: Picture
blueCell = color blue $ thickCircle 1 radius
    where radius = min cellWidth cellHeight * 0.75

yellowCell :: Picture
yellowCell = color yellow $ thickCircle 1.0 radius
    where radius = min cellWidth cellHeight * 0.75

greenCell :: Picture
greenCell = color green $ thickCircle 1.0 radius
    where radius = min cellWidth cellHeight * 0.75

boarderCell :: Picture
boarderCell = color black $ thickCircle 1.0 radius
    where radius = min cellWidth cellHeight * 0.81

-- the cells of the board
cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture = pictures
    $ map (snapPictureToCell cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

--makes the cells the color of the player
redCellsOfBoard :: Board -> Picture
redCellsOfBoard board = cellsOfBoard board (Full PlayerRed) redCell
redBoarder :: Board -> Picture
redBoarder board = cellsOfBoard board (Full PlayerRed) boarderCell

blueCellsOfBoard :: Board -> Picture
blueCellsOfBoard board = cellsOfBoard board (Full PlayerBlue) blueCell
blueBoarder :: Board -> Picture
blueBoarder board = cellsOfBoard board (Full PlayerBlue) boarderCell

yellowCellsOfBoard :: Board -> Picture
yellowCellsOfBoard board = cellsOfBoard board (Full PlayerYellow) yellowCell
yellowBoarder :: Board -> Picture
yellowBoarder board = cellsOfBoard board (Full PlayerYellow) boarderCell

greenCellsOfBoard :: Board -> Picture
greenCellsOfBoard board = cellsOfBoard board (Full PlayerGreen) greenCell
greenBoarder :: Board -> Picture
greenBoarder board = cellsOfBoard board (Full PlayerGreen) boarderCell

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
redCorner = pictures [translate 120 120 (rectangleSolid 240 240),
                      translate 260 60 (rectangleSolid 40 40),
                      translate 300 160 (rectangleSolid 40 240),
                      translate 100 260 (rectangleSolid 40 40)]

blueCorner :: Picture
blueCorner = pictures [translate 480 120 (rectangleSolid 240 240),
                       translate 540 260 (rectangleSolid 40 40),
                       translate 440 300 (rectangleSolid 240 40),
                       translate 340 100 (rectangleSolid 40 40)]

yellowCorner :: Picture
yellowCorner = pictures [translate 480 480 (rectangleSolid 240 240),
                         translate 340 540 (rectangleSolid 40 40),
                         translate 300 440 (rectangleSolid 40 240),
                         translate 500 340 (rectangleSolid 40 40)]

greenCorner :: Picture
greenCorner = pictures [translate 120 480 (rectangleSolid 240 240),
                        translate 60 340 (rectangleSolid 40 40),
                        translate 160 300 (rectangleSolid 240 40),
                        translate 260 500 (rectangleSolid 40 40)]

whiteSquare :: Picture
whiteSquare = pictures [translate 120 120 (rectangleSolid 160 160),
                        translate 480 120 (rectangleSolid 160 160),
                        translate 120 480 (rectangleSolid 160 160),
                        translate 480 480 (rectangleSolid 160 160)]

------------ DICE --------
visualDiceBG :: Picture
visualDiceBG = pictures [translate 200 400 (rectangleSolid 80 80)]

diceValue1 :: Picture
diceValue1 = pictures [translate 200 400 (thickCircle 5 10)]
diceValue2 :: Picture
diceValue2 = pictures [translate 180 380 (thickCircle 5 10),
                       translate 220 420 (thickCircle 5 10)
                      ]
diceValue3 :: Picture
diceValue3 = pictures [translate 200 400 (thickCircle 5 10),
                       translate 180 380 (thickCircle 5 10),
                       translate 220 420 (thickCircle 5 10)]
diceValue4 :: Picture
diceValue4 = pictures [translate 180 380 (thickCircle 5 10),
                       translate 220 420 (thickCircle 5 10),
                       translate 180 420 (thickCircle 5 10),
                       translate 220 380 (thickCircle 5 10)]


diceValue5 :: Picture
diceValue5 = pictures [translate 200 400 (thickCircle 5 10),
                       translate 180 380 (thickCircle 5 10),
                       translate 220 420 (thickCircle 5 10),
                       translate 180 420 (thickCircle 5 10),
                       translate 220 380 (thickCircle 5 10)]
diceValue6 :: Picture
diceValue6 = pictures [translate 180 380 (thickCircle 5 10),
                       translate 220 420 (thickCircle 5 10),
                       translate 180 420 (thickCircle 5 10),
                       translate 220 380 (thickCircle 5 10),
                       translate 180 400 (thickCircle 5 10),
                       translate 220 400 (thickCircle 5 10)
                       ]
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
            Running -> boardAsRunningPicture (gameBoard game) (dice game)
            GameOver winner -> boardAsGameOverPicture winner (gameBoard game)


rndNumGen :: [Float] -> Int
rndNumGen rnd = truncate (head rnd*6+1)

isCoordCorrect :: (Int, Int) -> Bool
isCoordCorrect = inRange ((0,0),(n-1,n-1))



{- findPlayersPos (assocs(gameBoard emptyBoard))

    RETURNS : list of players of desired color
    EXAMPLE : findPlayersPos (assocs(gameBoard emptyBoard)) PlayerRed == [((1,6),Full PlayerRed),((2,2),Full PlayerRed),((2,3),Full PlayerRed),((3,2),Full PlayerRed),((3,3),Full PlayerRed)]
-}
    --findPlayersPos (assocs(gameBoard emptyBoard))
    {-EXAMPLE: findPlayersPos (assocs(gameBoard emptyBoard)) == [((1,6),Full PlayerRed),((2,2),Full PlayerRed),((2,3),Full PlayerRed),
                                                                ((2,11),Full PlayerBlue),((2,12),Full PlayerBlue),((3,2),Full PlayerRed),
                                                                ((3,3),Full PlayerRed),((3,11),Full PlayerBlue),((3,12),Full PlayerBlue),
                                                                ((6,2),Full PlayerBlue),((11,2),Full PlayerGreen),((11,3),Full PlayerGreen),
                                                                ((11,11),Full PlayerYellow),((11,12),Full PlayerYellow),((12,2),Full PlayerGreen),
                                                                ((12,3),Full PlayerGreen),((12,11),Full PlayerYellow),((12,12),Full PlayerYellow)]
    -}
findPlayersPos :: [((Int,Int), Cell)] -> Player ->[((Int,Int), Cell)]
findPlayersPos [] _ = []
findPlayersPos (((_,_), Empty):xs) player = findPlayersPos xs player
findPlayersPos array@((x,y):xs) player
    | y == Full player = (x,Full player) : findPlayersPos xs player
    | otherwise = findPlayersPos xs player

{- movePlayer validPts point
    Moves a player(of selected color) from its current position to a position 1-6 cells forward.
    PRE      :
    RETURNS  : 
    EXAMPLES :
-}
--movePlayer

{- getNextPos lst point
    Gives the next possible position from a given point on the board
    RETURNS  :
    EXAMPLES :  movePlayer validPositions (12,6) == (13,6)
                movePlayer validPositions (14,6) == (14,7)
-}
getNexPos :: [a] -> (Int, Int) -> (Int, Int)
getNexPos validPts@(x:xs) point = validPositions !! (fromJust (elemIndex point validPositions) +1)

{- fromJust a
    Translates and integer of type Maybe to only a Integer
    RETURNS  : a single number
    EXAMPLES : fromJust (Just 7) == 7
-}
fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

spawnPoint :: [((Int,Int), Cell)] -> Player -> ((Int,Int),Cell)
spawnPoint [] _ = ((7,7),Empty)
spawnPoint ((x,y):xs) player =  ((spawnPointCords (findPlayersPos ((x,y):xs) player) player), Empty)

spawnPointCords :: [((Int,Int), Cell)] -> Player -> (Int,Int)
spawnPointCords [] _ = (7,7)
spawnPointCords ((x,y):xs) player | player == PlayerRed && elem x redSpawn = x
                                  | player == PlayerBlue && elem x blueSpawn = x 
                                  | player == PlayerGreen && elem x greenSpawn = x
                                  | player == PlayerYellow && elem x yellowSpawn = x
                                  | otherwise = spawnPointCords xs player

checkSpawnPoints :: Board -> Player -> [Cell]
checkSpawnPoints board player | player == PlayerRed = map (\i -> (!) board i) redSpawn
                              | player == PlayerBlue = map (\i -> (!) board i) blueSpawn
                              | player == PlayerGreen = map (\i -> (!) board i) greenSpawn
                              | player == PlayerYellow = map (\i -> (!) board i) yellowSpawn
                              | otherwise = [Empty,Empty,Empty,Empty]

isSpawnEmpty :: Board -> Player -> Bool 
isSpawnEmpty board player | player == PlayerRed && checkSpawnPoints board player == [Empty,Empty,Empty,Empty] = True
                         | player == PlayerBlue && checkSpawnPoints board player == [Empty,Empty,Empty,Empty] = True
                         | player == PlayerGreen && checkSpawnPoints board player == [Empty,Empty,Empty,Empty] = True
                         | player == PlayerYellow && checkSpawnPoints board player == [Empty,Empty,Empty,Empty] = True
                         | otherwise = False

movePlayer :: Board -> Int -> Game
movePlayer = undefined


playerSwitch game =
    case gamePlayer game of
        PlayerRed -> game {gamePlayer = PlayerGreen}
        PlayerGreen -> game {gamePlayer = PlayerYellow}
        PlayerYellow -> game {gamePlayer = PlayerBlue}
        PlayerBlue -> game {gamePlayer = PlayerRed}

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    -- places pieces with mouseclick
    | isCoordCorrect cellCoord && board ! cellCoord == Empty && elem cellCoord validPositions && 
    (length (findPlayersPos (assocs board) player) <4 || (isSpawnEmpty board player == False)) == True && diceU == True
       = playerSwitch $ game { gameBoard = board // [(cellCoord, Full player), (spawnPoint (assocs(board)) player)],
               diceUpdate = False,
               dice = Void}
    -- removes spawn piece and puts it in the entrypoint
    | isCoordCorrect cellCoord && board ! cellCoord == Full player && dic == Dice 6 && diceU == True
        = playerSwitch $ game { gameBoard = board // [(cellCoord, Empty),((getPlayerStart player entryPoints), Full player)],
                                diceUpdate = False,
                                dice = Void}
    -- remove piece to and after you can click to move it
    | isCoordCorrect cellCoord && board ! cellCoord == Full player && diceU == True 
    = game {pieceMove = Piece cellCoord}
    -- make it so that if all your pieces are in spawn and you do not get 6 you switch player
    -- dice transformer
    | isCoordCorrect cellCoord && board ! cellCoord == Empty && elem cellCoord dicePos && diceU == False = 
        if dic /= Dice 6 
        then playerSwitch $ game{rnd = drop 1 (rnd game),
                                 dice = Dice (rndNumGen (rnd game)),
                                 diceUpdate = False }
        else
        game {diceUpdate = True}
    | otherwise = game
    where board = gameBoard game
          player = gamePlayer game
          diceU = diceUpdate game
          dic = dice game
          pieceM = pieceMove game

mousePosCell :: (Float,Float) -> (Int,Int)
mousePosCell (x,y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                     , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth )
                     )

transformGame :: Event -> Game -> Game
transformGame (EventKey(MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
        Running -> playerTurn game $ mousePosCell mousePos
        GameOver _ -> emptyBoard
transformGame _ game = game

timeGen :: IO Int
timeGen = do
   currTime <- getCurrentTime
   let time = floor $ utctDayTime currTime :: Int
   return time

main :: IO ()
main = do
    time <- timeGen
    play window backgroundColor 30 (emptyBoard {rnd = randoms (mkStdGen time)}) gameAsPicture transformGame (const id)
