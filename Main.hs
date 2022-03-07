module Main where

import Test.HUnit
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Data.Array
import Graphics.Gloss.Interface.IO.Game (Event(EventKey), MouseButton (LeftButton))
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.IO
import Data.List
import Data.Time

-- Some functions examples have been omitted since Gloss-package changes the output so that it gets very tedious to both read and understand.
-- where interesting examples with Gloss interaction are viable, they have been exaplained with text.




{- it represents the color of the player
    a player can either be of colour red, blue green or yellow 
    HAAAAAAAAAAAAAAAINVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
data Player = PlayerRed | PlayerBlue | PlayerYellow | PlayerGreen deriving (Eq, Show)


{- the state of the game
    represented by being Running or GameOver, and is the typeclass of Maybe
    HAAAAAAAAAAAAAAAINVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}

--gamestate
data StateGame = Running | GameOver (Maybe Player) deriving (Eq, Show)


{- all of the cells on the gameboard
    if the cell is empty its displayed as Empty and if full displayed as Full
    HAAAAAAAAAAAAAAAINVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
data Cell = Empty | Full Player deriving (Eq, Show)

{- representing a dice with random values from one to six
    randomices an Int when called or a Void when the dice value is wiped from the screen
    HAAAAAAAAAAAAAAAINVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
data Dice = Void | Dice Int deriving (Eq, Show)


{- the workable board of the game
    the board contains an array where an element is a tuple of a tuple of two Ints which is a coordinate and a Cell which can be Empty or Player
    HAAAAAAAAAAAAAAAINVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
type Board = Array (Int, Int) Cell

-- this is a "Record" and creates variables for the type Game 
-- line 21 is equal, (almost) to data Game = Game Board Player State

{- represents the game
    components: board, player, state of game, two dice-types, and an update for dice
    HAAAAAAAAAAAAAAAINVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
data Game = Game { gameBoard :: Board,
                   gamePlayer :: Player,
                   gameState :: StateGame,
                   rnd :: [Float],
                   dice :: Dice,
                   diceUpdate :: Bool
                 } deriving (Eq, Show)

-- Window specifications
window = InWindow "Ludo" (600, 600) (100,100)

backgroundColor = white

screenWidth :: Int
screenWidth = 600

screenHeight :: Int
screenHeight = 600

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

-- How large the board is, 15x15
n :: Int
n = 15

-- Coordinates and crucial points
-- Total 55 possible position to the goal in (7,7)
validPositionsRed :: [(Int,Int)]
validPositionsRed = [(6,2),(6,1),(6,0),(7,0),(8,0),(8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(9,6),(10,6),(11,6),
                  (12,6),(13,6),(14,6),(14,7),(14,8),(13,8),(12,8),(11,8),(10,8),(9,8),(8,8),(8,9),(8,10),(8,11),
                  (8,12),(8,13),(8,14),(7,14),(6,14),(6,13),(6,12),(6,11),(6,10),(6,9),(6,8),(5,8),(4,8),(3,8),
                  (2,8),(1,8),(0,8),(0,7),(0,6),(1,6),(1,7),(2,7),(3,7),(4,7),(5,7),(6,7),(7,7)]
validPositionsGreen :: [(Int,Int)]
validPositionsGreen = [(12,6),(13,6),(14,6),(14,7),(14,8),(13,8),(12,8),(11,8),(10,8),(9,8),(8,8),(8,9),(8,10),
                (8,11),(8,12),(8,13),(8,14),(7,14),(6,14),(6,13),(6,12),(6,11),(6,10),(6,9),(6,8),(5,8),(4,8),(3,8),
                (2,8),(1,8),(0,8),(0,7),(0,6),(1,6),(2,6),(3,6),(4,6),(5,6),(6,6),(6,5),(6,4),(6,3),(6,2),
                (6,1),(6,0),(7,0),(8,0),(8,1),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7)]
validPositionsYellow :: [(Int,Int)]
validPositionsYellow = [(8,12),(8,13),(8,14),(7,14),(6,14),(6,13),(6,12),(6,11),(6,10),(6,9),(6,8),(5,8),(4,8),(3,8),
                (2,8),(1,8),(0,8),(0,7),(0,6),(1,6),(2,6),(3,6),(4,6),(5,6),(6,6),(6,5),(6,4),(6,3),(6,2),
                (6,1),(6,0),(7,0),(8,0),(8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(9,6),(10,6),(11,6),
                (12,6),(13,6),(14,6),(14,7),(14,8),(13,8),(13,7),(12,7),(11,7),(10,7),(9,7),(8,7),(7,7)]
validPositionsBlue :: [(Int,Int)]
validPositionsBlue = [(2,8),(1,8),(0,8),(0,7),(0,6),(1,6),(2,6),(3,6),(4,6),(5,6),(6,6),(6,5),(6,4),(6,3),(6,2),
                (6,1),(6,0),(7,0),(8,0),(8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(9,6),(10,6),(11,6),
                (12,6),(13,6),(14,6),(14,7),(14,8),(13,8),(12,8),(11,8),(10,8),(9,8),(8,8),(8,9),(8,10),
                (8,11),(8,12),(8,13),(8,14),(7,14),(6,14),(6,13),(7,13),(7,12),(7,11),(7,10),(7,9),(7,8),(7,7)]

--dice position
dicePos = [(9,4),(9,5),(10,4),(10,5)]

goalSquare = [(7,7)]
-- spawnpoints for all the colors
redSpawn = [(2,2),(3,2),(3,3),(2,3)]
blueSpawn = [(2,11),(3,11),(3,12),(2,12)]
greenSpawn = [(11,2),(12,2),(12,3),(11,3)]
yellowSpawn = [(11,11),(12,11),(12,12),(11,12)]

--entrypoints for all colors
entryPoints :: [(Cell, (Int,Int))]
entryPoints = [(Full PlayerRed,(6,2)),(Full PlayerGreen,(12,6)),(Full PlayerYellow,(8,12)),(Full PlayerBlue,(2,8))]

-- returns the first position that has that player in it, it has been paired with entryPoints so it just returnsthe coords for that player
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
                    diceUpdate = False
                  }
            -- This is used to define how large the array created will be
            where indexRange = ((0,0), (n-1, n-1))

{- diceGen dice 
    draws number of the dice that is rolled on the board
    PRE: Int must be between 1 and 6
    RETURNS: a picture corresponding the precise output of dice(input 1-6)
    EXAMPLES: diceGen (Dice 1) == Color (RGBA 1.0 1.0 1.0 1.0) (Pictures [Translate 200.0 400.0 (ThickCircle 5.0 10.0)])
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

{- boardAsRunningPicture board dice 
    Draws everything on the board, all the player pieces, the dice and all the colored cells
    PRE: Int must be between 1 and 6
    RETURNS: a picture of the board with every player on the board drawn
    EXAMPLES: when ran with only 5 different player pieces on the board it produces a picture with gloss´s function translate. An example is very tedious.
-}       
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

{- outComeColor player
    translates the playercolour to a RGBA-value
    RETURNS: the color of the player
    EXAMPLES: outComeColor (Just PlayerRed) == RGBA 1.0 0.0 0.0 1.0
-}  
outcomeColor :: Maybe Player -> Color 
outcomeColor (Just PlayerRed) = red
outcomeColor (Just PlayerBlue) = blue
outcomeColor (Just PlayerYellow) = yellow
outcomeColor (Just PlayerGreen) = green
outcomeColor Nothing = white

{- snapPictureToCell picture point
    translates the picture to a specific coordinate on the board
    RETURNS: the color of the player
    EXAMPLES: snapPictureToCell redCell (6,2) == Translate 100.0 260.0 (Color (RGBA 1.0 0.0 0.0 1.0) (ThickCircle 1.0 30.0))
-}  
snapPictureToCell :: Picture -> (Int, Int) -> Picture
snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5

-- variables to paint the cells in specific colours
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

{- cellsOfBoard board cell cellPicture
    produces a picture with cells with points drawn
    RETURNS: a picture with points on
-}
cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture = pictures
    $ map (snapPictureToCell cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

--makes the cells the color of the player
{- redCellsOfBoard board
    draws multiple circles on given coordiantes of the colour declared as argument
    this specification is the same for every following function that goes from Board -> Picture.
    RETURNS: a picture of a multiple red circles on the board and their position
    EXAMPLES: redCellsOfBoard (gameBoard emptyBoard) == Pictures [Translate 100.0 100.0 (Color (RGBA 1.0 0.0 0.0 1.0) (ThickCircle 1.0 30.0)),
                                                                  Translate 140.0 100.0 (Color (RGBA 1.0 0.0 0.0 1.0) (ThickCircle 1.0 30.0)),
                                                                  Translate 100.0 140.0 (Color (RGBA 1.0 0.0 0.0 1.0) (ThickCircle 1.0 30.0)),
                                                                  Translate 140.0 140.0 (Color (RGBA 1.0 0.0 0.0 1.0) (ThickCircle 1.0 30.0))]
-}
redCellsOfBoard :: Board -> Picture
redCellsOfBoard board = cellsOfBoard board (Full PlayerRed) redCell

{- redBoarder board
    this specification is the same for every following function that goes from Board -> Picture.
    draws four borders of declared colour on coordinates on the board
    RETURNS: a picture with colored lines forming a rectangle
    EXAMPLES: redBoarder (gameBoard emptyBoard) == Pictures [Translate 100.0 100.0 (Color (RGBA 0.0 0.0 0.0 1.0) (ThickCircle 1.0 32.4)),
                                                             Translate 140.0 100.0 (Color (RGBA 0.0 0.0 0.0 1.0) (ThickCircle 1.0 32.4)),
                                                             Translate 100.0 140.0 (Color (RGBA 0.0 0.0 0.0 1.0) (ThickCircle 1.0 32.4)),
                                                             Translate 140.0 140.0 (Color (RGBA 0.0 0.0 0.0 1.0) (ThickCircle 1.0 32.4))]
-}
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
{- boardGrid
    helper-variable that fills the use of showing where lines are drawn on the picture
-}
boardGrid :: Picture
boardGrid = pictures $ concatMap (\i -> [ line [ (i * cellWidth, 0.0),
                                                 (i * cellWidth, fromIntegral screenHeight)],
                                          line [ (0.0, i * cellHeight),
                                                 (fromIntegral screenWidth, i * cellHeight)]])
                                                 [0.0 .. fromIntegral n]

-- variable for all the bases of the colours
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

-- dice
visualDiceBG :: Picture
visualDiceBG = pictures [translate 200 400 (rectangleSolid 80 80)]

diceValue1 :: Picture
diceValue1 = pictures [translate 200 400 (thickCircle 5 10)]

diceValue2 :: Picture
diceValue2 = pictures [translate 180 380 (thickCircle 5 10),
                       translate 220 420 (thickCircle 5 10)]
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

{- boardAsPciture board
    makes all the lines, circles and players drawn on the board
    RETURNS: the whole gameboard animated in Gloss
    EXAMPLES: boardAsPciture (gameBoard emptyBoard), can be ran and will print the whole gameboard with all its colored paths and bases for the different colours, including their coordinates
-}
boardAsPicture :: Board -> Picture
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

boardAsGameOverPicture :: Maybe Player -> Board -> Picture
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

-- generates a random integer between 1 to 6
rndNumGen :: [Float] -> Int
rndNumGen rnd = truncate (head rnd*6+1)
-- returns True if the tuple is inside the board and False if it is outside
isCoordCorrect :: (Int, Int) -> Bool
isCoordCorrect = inRange ((0,0),(n-1,n-1))



{- findPlayersPos (assocs(gameBoard emptyBoard)) player

    RETURNS : list of players of desired color
    EXAMPLE : findPlayersPos (assocs(gameBoard emptyBoard)) PlayerRed == [((1,6),Full PlayerRed),((2,2),Full PlayerRed),((2,3),Full PlayerRed),((3,2),Full PlayerRed),((3,3),Full PlayerRed)]
-}
findPlayersPos :: [((Int,Int), Cell)] -> Player ->[((Int,Int), Cell)]
findPlayersPos [] _ = []
findPlayersPos (((_,_), Empty):xs) player = findPlayersPos xs player
findPlayersPos array@((x,y):xs) player
    | y == Full player = (x,Full player) : findPlayersPos xs player
    | otherwise = findPlayersPos xs player

{- getNextPos lst point
    Gives the next possible position from a given point on the board
    RETURNS  :
    EXAMPLES :  getNextPos validPositions (12,6) == (13,6)
                getNextPos validPositions (14,6) == (14,7)
-}
getNextPos ::  (Int, Int) -> Dice -> Player -> (Int, Int)
getNextPos point dice player = let positions = playerValidPositions player
                                in positions !! (fromJust (elemIndex point positions) + diceConverter dice)

-- returns the next position for that player on the winrow
getNextPosWin :: (Int,Int) -> Dice -> Player -> (Int, Int)
getNextPosWin point dice player = let positions = (drop 48 (playerValidPositions player) ++ drop 1 (reverse (drop 49 (playerValidPositions player))))
                                    in  positions !! (fromJust (elemIndex point positions) + diceConverter dice)
-- returns True if the player is on their winning row and False if not
isInWinRow :: (Int,Int) -> Player -> Bool
isInWinRow point player = point `elem` drop 48 (playerValidPositions player)


{- fromJust a
    Translates and integer of type Maybe to only a Integer
    RETURNS  : a single number
    EXAMPLES : fromJust (Just 7) == 7
-}
fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

-- gets the valid positions of that player
playerValidPositions :: Player -> [(Int,Int)]
playerValidPositions player | player == PlayerRed = validPositionsRed
                            | player == PlayerBlue = validPositionsBlue
                            | player == PlayerGreen = validPositionsGreen
                            | player == PlayerYellow = validPositionsYellow
                            | otherwise = validPositionsRed

-- checks if the spawnpoints are empty or not, returns a list of either [Full PlayerRed,Empty,Full PlayerRed,Full PlayerRed]
checkSpawnPoints :: Board -> Player -> [Cell]
checkSpawnPoints board player | player == PlayerRed = map (\i -> (!) board i) redSpawn
                              | player == PlayerBlue = map (\i -> (!) board i) blueSpawn
                              | player == PlayerGreen = map (\i -> (!) board i) greenSpawn
                              | player == PlayerYellow = map (\i -> (!) board i) yellowSpawn
                              | otherwise = [Empty,Empty,Empty,Empty]
-- same as function above without empty
checkSpawnPointsWithoutEmpty :: Board -> Player -> [Cell]
checkSpawnPointsWithoutEmpty board player = filter (/= Empty) (checkSpawnPoints board player)

-- reutrns the coords of the first empty spawnpoint for that player
emptySpawnPoint :: Board -> Player -> (Int,Int)
emptySpawnPoint board player | player == PlayerRed = redSpawn !! whenIsEmpty (checkSpawnPoints board player) 0
                             | player == PlayerBlue = blueSpawn !! whenIsEmpty (checkSpawnPoints board player) 0
                             | player == PlayerGreen = greenSpawn !! whenIsEmpty (checkSpawnPoints board player) 0
                             | player == PlayerYellow = yellowSpawn !! whenIsEmpty (checkSpawnPoints board player) 0
                             | otherwise = (7,7)

-- returns an integer for the first empty Spawnpoint
whenIsEmpty :: [Cell] -> Int -> Int
whenIsEmpty [] acc = acc
whenIsEmpty (x:xs) acc | x == Empty = acc
                       | otherwise = whenIsEmpty xs acc+1

-- returns an list of empty if the spawnpoint of a player is empty
isSpawnEmpty :: Board -> Player -> Bool
isSpawnEmpty board player | player == PlayerRed && checkSpawnPoints board player == [Empty,Empty,Empty,Empty] = True
                         | player == PlayerBlue && checkSpawnPoints board player == [Empty,Empty,Empty,Empty] = True
                         | player == PlayerGreen && checkSpawnPoints board player == [Empty,Empty,Empty,Empty] = True
                         | player == PlayerYellow && checkSpawnPoints board player == [Empty,Empty,Empty,Empty] = True
                         | otherwise = False
-- checks if a coord is in the spawnpoint for that player
isInSpawn :: (Int, Int) -> Player -> Bool
isInSpawn coords player = case player of
    PlayerRed -> elem coords redSpawn
    PlayerBlue -> elem coords blueSpawn
    PlayerGreen -> elem coords greenSpawn
    PlayerYellow -> elem coords yellowSpawn

-- converts dice to an int
diceConverter :: Dice -> Int
diceConverter Void = 0
diceConverter (Dice x) = x

-- converts Cell to Player
cellConverter :: Cell -> Player
cellConverter Empty = PlayerRed
cellConverter (Full player) = player

-- Probably takes the position of the clicked player and then checks wich position that is in the valid positions
-- then adds the dice to that number and checks the validPositions what the coords is for that number.



playerSwitch :: Game -> Game
playerSwitch game =
    case gamePlayer game of
        PlayerRed -> game {gamePlayer = PlayerGreen}
        PlayerGreen -> game {gamePlayer = PlayerYellow}
        PlayerYellow -> game {gamePlayer = PlayerBlue}
        PlayerBlue -> game {gamePlayer = PlayerRed}

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    -- removes spawn piece and puts it in the entrypoint
    -- if there is already a piece on that coord the piece disapears
    -- but it does not reappear in the respective spawnpoint
    | isCoordCorrect cellCoord && board ! cellCoord == Full player && dic == Dice 6 && diceU == True && isInSpawn cellCoord player
        = let entryPoint = getPlayerStart player entryPoints
        in if board ! entryPoint /= Full player 
            then if board ! entryPoint == Empty 
                then playerSwitch $ game { gameBoard = board // [(cellCoord, Empty),(entryPoint, Full player)],
                                diceUpdate = False,
                                rnd = drop 1 (rnd game),
                                dice = Void}
                else playerSwitch $ game { gameBoard = board // [(cellCoord, Empty),(entryPoint, Full player),
                (emptySpawnPoint board (cellConverter $ board ! entryPoint),board ! entryPoint)],
                                diceUpdate = False,
                                rnd = drop 1 (rnd game),
                                dice = Void}
            else game
            
    -- function for the winning row
    | isCoordCorrect cellCoord && board ! cellCoord == Full player && diceU == True && isInWinRow cellCoord player
        = if getNextPosWin cellCoord dic player == (7,7) 
            then if (length (findPlayersPos (assocs board) player)) == 1 then game {gameState = GameOver (Just player),
                                                                                    gameBoard = board // [(cellCoord, Empty)],
                                                                                    rnd = drop 1 (rnd game),
                                                                                    dice = Void}
                else 
                    playerSwitch $ game { gameBoard = board // [(cellCoord, Empty)],
                                diceUpdate = False,
                                rnd = drop 1 (rnd game),
                                dice = Void}

            else playerSwitch $ game { gameBoard = board // [(cellCoord, Empty),((getNextPosWin cellCoord dic player, Full player))],
                                diceUpdate = False,
                                rnd = drop 1 (rnd game),
                                dice = Void}



    -- pick which piece to move
    -- crashes if cellCoord + dic becomes something outside the arrayrange
    -- you can kill your own piece if you walk on it (maybe feature)
    -- cannot do anything if you have no players on the field and your spawnpoint does not have 4 pieces
    | isCoordCorrect cellCoord && board ! cellCoord == Full player && diceU == True && elem cellCoord (playerValidPositions player)
    = if board ! (getNextPos cellCoord dic player) == Empty then
        playerSwitch $ game {gameBoard = board // [(cellCoord, Empty),(getNextPos cellCoord dic player, Full player)],
                           rnd = drop 1 (rnd game),
                           diceUpdate = False,
                           dice = Void}
    else if board ! (getNextPos cellCoord dic player) == Full player then game
        else playerSwitch $ game {gameBoard = board // [(cellCoord, Empty),(getNextPos cellCoord dic player, Full player),
        (emptySpawnPoint board (cellConverter(board ! (getNextPos cellCoord dic player))),board ! (getNextPos cellCoord dic player))],
                           rnd = drop 1 (rnd game),
                           diceUpdate = False,
                           dice = Void}
    -- Dice for when there are no pieces on the board
    | isCoordCorrect cellCoord && board ! cellCoord == Empty && elem cellCoord dicePos && diceU == False &&
    ((((checkSpawnPoints board player) == [Full player, Full player, Full player, Full player]) || 
    (length (findPlayersPos (assocs board) player) == 3 && checkSpawnPointsWithoutEmpty board player == [Full player, Full player, Full player])) ||
    (length (findPlayersPos (assocs board) player) == 2 && checkSpawnPointsWithoutEmpty board player == [Full player, Full player]) || 
    (length (findPlayersPos (assocs board) player) == 1 && checkSpawnPointsWithoutEmpty board player == [Full player]))
       = case Dice (rndNumGen (rnd game)) of
            Dice 6 -> game{rnd = drop 1 (rnd game),dice = Dice (rndNumGen (rnd game)),diceUpdate = True }
            Dice _ -> playerSwitch $ game{rnd = drop 1 (rnd game),dice = Dice (rndNumGen (rnd game)),diceUpdate = False }
    -- dice for everything else
    | isCoordCorrect cellCoord && board ! cellCoord == Empty && elem cellCoord dicePos && diceU == False =
        game{rnd = drop 1 (rnd game),dice = Dice (rndNumGen (rnd game)),diceUpdate = True }
    -- if something bad is pressed redo the function until something good is pressed
    | otherwise = game
    where board = gameBoard game
          player = gamePlayer game
          diceU = diceUpdate game
          dic = dice game

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



------------------ TEST CASES ---------------------

-- snapPictureToCell
-- test1 = TestCase $ assertEqual "snapPictureToCell redCell (7,7)" Translate 300.0 300.0 (Color (RGBA 1.0 0.0 0.0 1.0) (ThickCircle 1.0 30.0)) (snapPictureToCell redCell (7,7))

-- rndNumGen
test2 = TestCase $ assertEqual "rndNumGen [0.79291392193]" 5 (rndNumGen [0.79291392193]) 

--isCoordCorrect
test3 = TestCase $ assertBool "isCoordCorrect (7,7)" (isCoordCorrect (7,7))

--findPlayerPos
test4 = TestCase $ assertEqual "findPlayersPos (assocs(gameBoard emptyBoard)) PlayerRed" [((2,2),Full PlayerRed),((2,3),Full PlayerRed),((3,2),Full PlayerRed),((3,3),Full PlayerRed)] (findPlayersPos (assocs(gameBoard emptyBoard)) PlayerRed)

--getNextPos 
test5 = TestCase $ assertEqual "getNextPos (6,1) (Dice 6) PlayerRed" (8,3) (getNextPos (6,1) (Dice 6) PlayerRed)
test6 = TestCase $ assertEqual "getNextPos (3,6) (Dice 4) PlayerBlue" (6,5) (getNextPos (3,6) (Dice 4) PlayerBlue)
--getNextPosWin
test7 = TestCase $ assertEqual "getNextPosWin (12,7) (Dice 6) PlayerYellow"  (8,7) (getNextPosWin (12,7) (Dice 6) PlayerYellow)

--checkSpawnPoints
test9 = TestCase $ assertEqual "checkSpawnPoints (gameBoard emptyBoard) PlayerBlue" [Full PlayerBlue,Full PlayerBlue,Full PlayerBlue,Full PlayerBlue] (checkSpawnPoints (gameBoard emptyBoard) PlayerBlue)

--whenIsEmpty
test10 = TestCase $ assertEqual "whenIsEmpty (checkSpawnPoints (gameBoard emptyBoard) PlayerBlue) 0" 4 (whenIsEmpty (checkSpawnPoints (gameBoard emptyBoard) PlayerBlue) 0)

--emptySpawnPoint?
-- test11 = TestCase $ assertEqual "emptySpawnPoint (gameBoard emptyBoard) PlayerYellow" () (emptySpawnPoint (gameBoard emptyBoard) PlayerYellow)

--isInSpawn
test12 = TestCase $ assertBool "isInSpawn (12,2) PlayerGreen" (isInSpawn (12,2) PlayerGreen)

--playerSwitch?
test13 = TestCase $ assertEqual "playerSwitch emptyBoard" (emptyBoard {gamePlayer = PlayerGreen}) (playerSwitch emptyBoard)

--playerTurn?
test14 = TestCase $ assertEqual "playerTurn emptyBoard{gameBoard = gameBoard (gameBoard emptyBoard) // [((8,4),Full PlayerRed),((3,2),Empty)]} (8,6)" 
                        (emptyBoard {gameBoard = (gameBoard emptyBoard) // [((8,6),Full PlayerRed),((3,2),Empty)]}) 
                        (playerTurn (emptyBoard {gameBoard = (gameBoard emptyBoard) // [((8,4),Full PlayerRed),((3,2),Empty)],
                                                diceUpdate = True,
                                                dice = Dice 2}) (8,4))
runtests = runTestTT $ TestList [test2,test3,test4,test5,test6,test7,test9,test10,test12]
