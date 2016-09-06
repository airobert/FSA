module FSAlab4
where 
import Data.List
import System.Random
-- extra package required: arithmoi
import Math.NumberTheory.Primes.Testing
-- http://www.serpentine.com/criterion/tutorial.html
-- import Data.Time
-- write a loop and call the time function again and again
-- 

--factors :: Integer -> [Integer]
--factors n = factors' n 2 where
--	factors' 1 _       = []
--	factors' n m 
--	  | n `mod` m == 0 = m : factors' (n `div` m) m
--	  | otherwise      =     factors' n (m+1)


-- exercise 1

next_prime :: Integer ->  Integer
next_prime n = 
  if (simple_test_prime (n + 1)) 
    then n +1
    else next_prime (n+1)

-- MG: By the way, we can use a one-line QuickCheck to check this:
--     *FSAlab4 GOA Test.QuickCheck> quickCheckResult (\n -> (n<=0) || isPrime (next_prime n))

simple_test_prime :: Integer -> Bool
simple_test_prime n =
  simple_test_prime' n 2 where
    simple_test_prime' n m
      | n `mod` m == 0 = False
      | (m * m) > n    = True
      | otherwise      = simple_test_prime' n (m + 1)

factors :: Integer -> [Integer]
factors n = factors' n 2 where
  factors' 1 _       = []
  factors' n m
    | n `mod` m == 0 =  m : factors' (n `div` m) m
    | (m * m) > n    = [n]
    | otherwise      = factors' n (next_prime m )

-- MG: Is this really faster?

-- exercise 2

testAKS :: Integer -> Bool
testAKS n = False


addM :: Integer -> Integer -> Integer -> Integer
addM x y = rem (x+y)

multM :: Integer -> Integer -> Integer -> Integer
multM x y = rem (x*y) 

expM ::  Integer -> Integer -> Integer -> Integer
expM x y = rem (x^y)

-- a more effecient version of this (x^y) mod k
exM :: Integer -> Integer -> Integer -> Integer
exM x y m
  | y ==0  = 1
  | (y `mod` 2) == 0 = ((exM x (y `div` 2) m) ^2) `mod` m  
  | otherwise = x * (exM x (y-1) m) `mod` m 

test x = (x^5) `rem` 5 == ((((x^4) `mod` 5) * x)`mod` 5)
 
-- MG: Did you benchmark it? Where are the results?


--Fermat's test for 

ferman_test :: Integer -> Bool
ferman_test p = 
  foldl (&&) True [(exM a (p-1) p) == 1 | a <- [1..(p-1)]]


randomFlip :: Integer -> IO Integer
randomFlip x = do 
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

getRandomInt :: Integer -> IO Integer
getRandomInt n = getStdRandom (randomR (0,n))


getIntList :: Integer -> IO [Integer]
getIntList p = do 
  n <- getRandomInt 100
  getIntL p n

getIntL :: Integer -> Integer -> IO [Integer]
getIntL _ 0 = return []
getIntL p n = do 
  x <-  getRandomInt p
  y <- randomFlip x
  xs <- getIntL p (n-1)
  return (y:xs)


--  My implementation of Ferman's algorithm with 100 test
--  instead of output "Prime" or "Composite". Just return True or False
-- MG: Why is this False on 5 and 7? They are not Carmichael!
ferman_test_p :: Integer -> IO Bool
ferman_test_p p =
  do
    l <- getIntList p
    return (foldl (&&) True [(exM a (p-1) p) == 1 | a <- [1..9]])

-- Carmichael numbers may fool Ferman's algorithm. 


--Miller–Rabin primality test
-- first, get n-1 = 2^r * d 

divide n' acc = 
  if (n' `mod` 2 ==0) 
    then divide (n' `div` 2) (acc + 1)
    else acc

rewrite :: Integer -> (Integer, Integer)
rewrite n = 
  let r = divide (n) 0 in 
  let d = n `div` (2^r) in 
  (r, d) 

-- test: rewrite 221 = (2,55) as in Wikipedia

-- get a random number from [2 , n-2]  
getRandomIndex :: Integer -> IO Integer
getRandomIndex n = 
  do 
    x <- (getStdRandom (randomR (0,n-4)))
    return (x + 2)

-- if composite then retrn True, otherwise return False (probably prime)
witnessloop :: Integer -> Integer -> IO Bool 
witnessloop n k = 
  if (k == 0) 
    then return False 
    else
      do
        a <- getRandomIndex n
        --print a
        let (r, d) = rewrite (n-1)
        let x = exM a d n
        --print a 
        --print (n-1)
        --print r
        --print (-1)
          -- (a ^ d) `mod` n 
        if (x == 1) || (x == n-1)
          then witnessloop n (k-1)
          else  (repeat_search (r-1) x n k)

repeat_search :: Integer -> Integer -> Integer -> Integer -> IO Bool
repeat_search r' x n k =
  if r' <= 0 
    then return True
    else 
      let x' = (multM x x n) in 
      if (x' == 1) 
      then return True
        else if (x' == n -1) then witnessloop n (k-1)
        else (repeat_search (r' - 1) x n k)

miller_Rabin :: Integer -> IO Bool
miller_Rabin n = 
  do 
    x <- witnessloop n 100
    if x == False then return True
    else return False

-- ex5 

-- first of all, implement a list of composite numbers
-- test, if any algorithm says Yes (consider this number as prime), then we 
-- get a false-positive


-- Additional:
-- I also installed arithmoi package for testing.
-- Ref: http://hackage.haskell.org/package/arithmoi-0.4.1.3/docs/Math-NumberTheory-Primes-Testing.html

-- to generate a list of tesing numbers. We have:
composite_number :: [Integer]
composite_number = [ n | n<- [2..100000000], not(isPrime n) ]

-- find false-positive for Miller-Rabin
find_MR_false_positive :: [Integer] -> [IO Bool]
find_MR_false_positive clist =
  fmap miller_Rabin clist

-- MG: [IO Bool] is not practical, try this:
find_MR_false_positive' :: [Integer] -> IO [Bool]
find_MR_false_positive' [] = return []
find_MR_false_positive' (x:xs) = do
  b <- miller_Rabin x
  bs <- find_MR_false_positive' xs
  return (b:bs)

find_MR clist =
  map (\x -> millerRabinV (fromIntegral x)) clist

test_5 = 
  length (find_MR_false_positive composite_number)

test_5' = 
  length (find_MR composite_number)

-- I have some strange results. I tested numbers within 10000000. Below is my result

--FSAlab4> length composite_number 
--9335420
--FSAlab4> test_5
--9335420
--FSAlab4> test_5'
--9335420

-- They are the same. That was confusing :(

-- MG: The length does not depend on the test result, because you
--     are collecting all the booleans. With my variant of the
--     function you can see this by running:
--       find_MR_false_positive' (take 100 composite_number)


-- ex7 

my_gcd :: Integer -> Integer -> Integer
my_gcd x y = 
  let  l = intersect (factors x ) (factors y) in 
  foldl (*) 1 l


--Modular multiplicative inverse
my_mmi :: Integer -> Integer -> Integer
my_mmi e m = 
  mmi' e m 3
  where 
    mmi' e m c = 
      --if (((e * c) `mod` m) == 1)
      if ((exM e c m ) == 1)
        then c
        else (mmi' e m (c+1)) 

invM :: Integer -> Integer -> Integer
invM x n = let 
   (u,v) = fct_gcd x n
   copr  = x*u + v*n == 1
   i     = if signum u == 1 then u else u + n  
 in 
   if copr then i else error "no inverse"

new_coprime :: Integer -> Integer -> Bool
new_coprime x n = 
  let (u,v) = fct_gcd x n in  
  ((x*u + v*n) == 1)

fct_gcd :: Integer -> Integer -> (Integer,Integer) 
fct_gcd a b = 
  if b == 0 
  then (1,0) 
  else 
     let 
       (q,r) = quotRem a b
       (s,t) = fct_gcd b r 
     in (t, s - q*t)

get_coprime :: Integer -> IO Integer
get_coprime phi = 
  let l = factors phi in 
  do 
    e <- getRandomInt phi
    --if (my_gcd e phi) == e 
    if (new_coprime e phi)
      then return e
      else (get_coprime phi)

-- return (public_keys, private_keys)
get_keys p q = 
  let phi = (p - 1) *  (q -1) in 
  do 
    e <- get_coprime phi
    print e
    print phi
    let d = invM e phi
    return ((p*q, e), d)

encode_num num e n = 
  exM num e n 

decode_num num n private  = 
  exM num private n

-- MG: Please use the function names and types given in the exercises.
--     I find it irritating here that you change the input position
--     of n, so encoding and decoding are no longer the same!
