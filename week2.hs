module FSA2
where
import System.Random

-- to get a randome number:
-- http://learnyouahaskell.com/input-and-output#randomness


getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

-- randomly flip, 4 to -4 or vice versa
randomFlip :: Int -> IO Int
randomFlip x = do 
	b <- getRandomInt 1
	if b == 0 then return x else return (-x)


-- the first argument is the range (both +/-), the second is the length of the list
-- *FSA2> getIntL 3 10
--[2,1,0,-1,0,0,1,0,2,2]

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

--get a list of integers
genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

