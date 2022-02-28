module Mainanton(main) where

import Graphics.Gloss

data Direction = South | West | North | East
data Scoreboard = Score Player Int
data State = Running | GameOver (Maybe Player)
type Player = (PlayerPosition, Direction)
type PlayerPosition = (Integer , Integer)

gridSize = 60
lineWidth = 5
backgroundGrid = Color (makeColorI 192 192 192 255) $ Pictures
    [ Translate (-r) 0  verticalLongLine
    , Translate r 0     verticalLongLine
    , Translate 0 (-r)  horizontalLongLine
    , Translate 0 r     horizontalLongLine
    , Translate 0 (2*r)     horizontalShortLine
    , Translate 0 (3*r)     horizontalShortLine
    , Translate 0 (4*r)     horizontalShortLine
    , Translate 0 (5*r)     horizontalShortLine
    , Translate 0 (6*r)     horizontalShortLine
    , Translate 0 (7*r)     horizontalShortLine
    , Translate 0 (8*r)     horizontalShortLine
    , Translate 0 (-2*r)     horizontalShortLine
    , Translate 0 (-3*r)     horizontalShortLine
    , Translate 0 (-4*r)     horizontalShortLine
    , Translate 0 (-5*r)     horizontalShortLine
    , Translate 0 (-6*r)     horizontalShortLine
    , Translate 0 (-7*r)     horizontalShortLine
    , Translate 0 (-8*r)     horizontalShortLine
    , Translate (2*r)  0   verticalShortLine
    , Translate (3*r)  0   verticalShortLine
    , Translate (4*r)  0   verticalShortLine
    , Translate (5*r)  0   verticalShortLine
    , Translate (6*r)  0   verticalShortLine
    , Translate (7*r)  0   verticalShortLine
    , Translate (8*r)  0   verticalShortLine
    , Translate (-2*r) 0   verticalShortLine
    , Translate (-3*r) 0   verticalShortLine
    , Translate (-4*r) 0   verticalShortLine
    , Translate (-5*r) 0   verticalShortLine
    , Translate (-6*r) 0   verticalShortLine
    , Translate (-7*r) 0   verticalShortLine
    , Translate (-8*r) 0   verticalShortLine


    , Translate (-2*r) 0  verticalLongLine
    , Translate (2*r) 0     verticalLongLine
    , Translate 0 (-2*r)  horizontalLongLine
    , Translate 0 (2*r)     horizontalLongLine
--    , Translate (-3*r) 0  verticalLine
--    , Translate (3*r) 0     verticalLine
--    , Translate 0 (-3*r)  horizontalLine
--    , Translate 0 (3*r)     horizontalLine 
--    , Translate (-4*r) 0  verticalLine
--    , Translate (4*r) 0     verticalLine
--    , Translate 0 (-4*r)  horizontalLine
--    , Translate 0 (4*r)     horizontalLine 
--    , Translate (-5*r) 0  verticalLine
--    , Translate (5*r) 0     verticalLine
--    , Translate 0 (-5*r)  horizontalLine
--    , Translate 0 (5*r)     horizontalLine 
--    , Translate (-6*r) 0  verticalLine
--    , Translate (6*r) 0     verticalLine
--    , Translate 0 (-6*r)  horizontalLine
--    , Translate 0 (6*r)     horizontalLine 
--    , Translate (-7*r) 0  verticalLine
--    , Translate (7*r) 0     verticalLine
--    , Translate 0 (-7*r)  horizontalLine
--    , Translate 0 (7*r)     horizontalLine
--    , Translate (-8*r) 0  verticalLine
--    , Translate (8*r) 0     verticalLine
--    , Translate 0 (-8*r)  horizontalLine
--    , Translate 0 (8*r)     horizontalLine  
    ]
    where r = gridSize / 2

horizontalLongLine = rectangleSolid (gridSize * 8) lineWidth
horizontalShortLine = rectangleSolid (gridSize * 2) lineWidth

verticalLongLine = rectangleSolid lineWidth (gridSize * 8)
verticalShortLine = rectangleSolid lineWidth (gridSize * 2)
winSquare = Color (makeColorI 200 255 200 255) $ rectangleSolid gridSize gridSize


window :: Display
window = InWindow "cuckeroo" (640, 640) (10, 10)

backgroundColor :: Color
backgroundColor = makeColor 255 255 255 255 


drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window backgroundColor backgroundGrid




gameAsPicture _ = Blank
transformGame _ game = game

board :: [[Int]]
board = [[0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],
         [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],
         [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],
         [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],
         [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],
         [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],
         [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
         [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
         [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
         [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],
         [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],
         [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],
         [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],
         [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],
         [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0]]

{-
data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: State
                 } deriving (Eq, Show)
------------------------------------------------------------------------------------------------
initialGame = Game {gameBoard = array indexRange $ zip (range idndexRange) (cycle [Empty])
                 ,  gamePlayer = Player1
                 ,  gameState 
                 }
    where indexRange = ((0,0), (n-1, n-1))




-}