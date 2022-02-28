import System.Random(randomRIO)
import System.IO


--dice runnable with "dice 1"


dice :: Int -> IO[Int]
dice 0 = return []
dice n = do
  r  <- randomRIO (1,6)
  rs <- dice (n-1)
  return (r:rs)
