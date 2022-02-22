
type PlayerPosition = (Integer , Integer)

data Direction = South | West | North | East
type Player = (PlayerPosition, Direction)
data Scoreboard = Score Player Int

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