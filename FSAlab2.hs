-- Note that I simplified the game by only considering samepos and occurscount 
-- rather than take reactions and then decompose to samepos and occurscount
-- MG: That's perfectly fine.

-- About Mastermind:
-- https://en.wikipedia.org/wiki/Mastermind_(board_game)
-- ref: https://www.youtube.com/watch?v=OPeBUDXKGqA
module FSAlab2
where 
import Data.List
import System.Random
import Control.Monad
import Data.Maybe
--import Data.Pair
--import Random (randomRIO)

data Colour   = Red | Yellow | Blue | Green | Orange 
                deriving (Eq,Show,Bounded,Enum)

data Answer   = Black | White deriving (Eq,Show)

type Pattern  = [Colour]
type Feedback = [Answer]

samepos :: Pattern -> Pattern -> Int
samepos _      []                 = 0 
samepos []     _                  = 0 
samepos (x:xs) (y:ys) | x == y    = samepos xs ys + 1
                      | otherwise = samepos xs ys


occurscount ::  Pattern -> Pattern -> Int
occurscount xs []       = 0
occurscount xs (y:ys) 
          | y `elem` xs = occurscount (delete y xs) ys + 1
          | otherwise   = occurscount xs ys 

board_size = 12


-- *******************************
--I don't like this reaction function
--
--reaction :: Pattern -> Pattern -> [Answer]
--reaction secret guess = take n (repeat Black) 
--                     ++ take m (repeat White)
--   where n = samepos secret guess 
--         m = occurscount secret guess - n
-- *******************************

-- define a new reaction function:
reaction :: Pattern -> Pattern -> (Int, Int)
reaction secret guess =
	let n = samepos secret guess in 
    let m = occurscount secret guess - n in 
    (n, m)

-- *********************************************
--generate all possibility of colors
all_color :: Pattern
all_color = [Red, Yellow, Blue, Green, Orange]
--all possible combinations of Pattern
all_list :: [Pattern]
all_list = [[a, b, c, d] | a <- all_color, b <- all_color, c <- all_color, d <- all_color ]
--construct all possible outcome
olist = [(a,b)| a<-[0..4], b<-[0..(4-a)]]
-- *********************************************


-- *********************************************
-- ignore the following for now. These functions are supposed to be used for testing of these algorithms
--get a random guess/result
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))


getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   xs <- getIntL k (n-1)
   return (x:xs)
-- a randome pick function from 
-- http://rosettacode.org/wiki/Pick_random_element#Haskell
pick :: [a] -> IO a
pick xs = getRandomInt 4 >>= return . (xs !!)

getRandomColor :: IO Colour
getRandomColor =  
	pick all_color 
	
-- MG: as we talked earlier, an [IO Colour] is not very helpful.
--     I changed your code to produce an IO [Colour]
getRandomPattern :: IO [Colour]
getRandomPattern = do
	x1 <- getRandomColor
	x2 <- getRandomColor
	x3 <- getRandomColor
	x4 <- getRandomColor
	return [x1, x2, x3, x4]

-- *********************************************

-- part 1

-- filter out impossible guesses
list_filter :: Int -> Int -> Pattern -> [Pattern] -> [Pattern]
list_filter same occurs guess plist =
	filter (\x -> (((occurscount guess x) == occurs) && 
		((samepos guess x) == same))) plist


play_naive' :: Pattern -> [Pattern] -> Int ->  Maybe Int
--   args:   result & all possibilities & steps left 
--  	return the steps taken, otherwise return Nothing
-- MG: The "steps taken" is off by one. Your "Just 0" means "first guess was right".
play_naive' _ _ 0 = Nothing
play_naive' answer plist steps_left = 
	--make a guess . i.e, to get the head of the list
	let guess = head plist in 
	--get how many are the same
	let same = samepos answer guess in 
	let occurs =  occurscount answer guess in 
	let plist' = list_filter same occurs guess (tail plist) in 
	if same == 4 then  Just (board_size - steps_left)
	else if steps_left > 0 then play_naive' answer plist' (steps_left -1) 
	else Nothing

-- MG: A general comment: You use "let ... in ..." a lot and your code
--     looks very imperative. Please look up the syntax of "where" and
--     try to write more functional next week ;-)

-- MG: Please add type definitions!
play_naive result = 
	play_naive' result all_list board_size
--part 2

-- ref: http://stackoverflow.com/questions/20418347/knuths-algorithm-for-solving-mastermind-with-5-guesses
--the most inner loop, count possible patterns satisfying a certain outcome
check_outcome guess outcome [] count = count
check_outcome guess outcome solution_list count =
	let solution = head solution_list in 
	if (reaction guess solution) == outcome  then (check_outcome guess outcome (tail solution_list) (count+1))
	else (check_outcome guess outcome (tail solution_list) count)

-- for each outcome. compute the max of the list of possible patterns
check_each_outcome guess []  reduced_list max_count = max_count
check_each_outcome guess outcome_list  reduced_list max_count = 
	let outcome = head outcome_list in 
	let count = (check_outcome guess outcome reduced_list 0) in 
	if count > max_count then check_each_outcome guess (tail outcome_list) reduced_list count
	else check_each_outcome guess (tail outcome_list) reduced_list max_count


-- a min-max step; to make the max of min on choice of outcomes
check_each_remaining_guess [] min_count best_guess = best_guess
check_each_remaining_guess guess_list min_count best_guess = 
	let next_guess = head guess_list in 
	let value =  check_each_outcome next_guess olist guess_list 0 in 
	if value < min_count then check_each_remaining_guess (tail guess_list) value next_guess
	else check_each_remaining_guess (tail guess_list) min_count best_guess

--choose a next step based on Knuth's algorithm
choose_knuth plist = 
	-- have an input of possible pattern list and return the choosen one
	check_each_remaining_guess plist (999999) (head plist)

play_knuth' :: Pattern -> [Pattern] -> Int -> Maybe Int
-- Same arguments as above
-- MG: Not just the arguments, also the functinn itself is very similar.
--     Think about how you can merge them, have a look at solutions from
--     other people which use a general play function!
play_knuth' _ _ 0 = Nothing
play_knuth' answer plist steps_left = 
	let guess = choose_knuth plist in 
	let same = samepos answer guess in 
	let occurs =  occurscount answer guess in 
	let plist' = list_filter same occurs guess (filter (\x -> not (x == guess)) plist) in 
	if same == 4 then  Just (board_size - steps_left)
	else if steps_left > 0 then play_knuth' answer plist' (steps_left -1) 
	else Nothing

-- Knuth suggested this as first guess
-- MG: suggested? You should be able to find/explain this yourself now.
first_guess  = [Red, Red, Yellow, Yellow]

play_knuth answer =
	let same = samepos answer first_guess in 
	let occurs = occurscount answer first_guess in 
	if same == 4 then Just 1
	else play_knuth' answer (list_filter same occurs first_guess (filter (\x -> not(x == first_guess)) all_list)) (board_size - 1)


-- part 3

-- Kooi's algorithm. The only modification was when comparing value with min_count compared with Knuth's.
check_each_remaining_guess_kooi [] min_count best_guess = best_guess
check_each_remaining_guess_kooi guess_list min_count best_guess = 
	let next_guess = head guess_list in 
	let value =  check_each_outcome next_guess olist guess_list 0 in 
	if value > min_count then check_each_remaining_guess_kooi (tail guess_list) value next_guess
	else check_each_remaining_guess_kooi (tail guess_list) min_count best_guess

choose_kooi plist = 
	-- have an input of possible pattern list and return the choosen one
	check_each_remaining_guess_kooi plist (0) (head plist)

play_kooi' :: Pattern -> [Pattern] -> Int -> Maybe Int
-- Same arguments as above
play_kooi' _ _ 0 = Nothing
play_kooi' answer plist steps_left = 
	let guess = choose_kooi plist in 
	let same = samepos answer guess in 
	let occurs =  occurscount answer guess in 
	let plist' = list_filter same occurs guess (filter (\x -> not (x == guess)) plist) in 
	if same == 4 then  Just (board_size - steps_left)
	else if steps_left > 0 then play_kooi' answer plist' (steps_left -1) 
	else Nothing

play_kooi answer =
	let same = samepos answer first_guess in 
	let occurs = occurscount answer first_guess in 
	if same == 4 then Just 1
	else play_kooi' answer (list_filter same occurs first_guess (filter (\x -> not(x == first_guess)) all_list)) (board_size - 1)

-- part 4
-- Kooi's second algorithm presented is to take the possibility of each pattern in remaining list
-- This was achieved by sum up the count of each possible outcome
check_outcome_kooi2 guess outcome [] count = count
check_outcome_kooi2 guess outcome solution_list count =
	let solution = head solution_list in 
	if (reaction guess solution) == outcome  then (check_outcome_kooi2 guess outcome (tail solution_list) (count+1))
	else (check_outcome_kooi2 guess outcome (tail solution_list) count)

check_each_outcome_kooi2 guess []  reduced_list max_count = max_count
check_each_outcome_kooi2 guess outcome_list reduced_list max_count = 
	let outcome = head outcome_list in 
	let count = (check_outcome_kooi2 guess outcome reduced_list 0) in 
	check_each_outcome_kooi2 guess (tail outcome_list) reduced_list (count + max_count)
-- sum up the count as the possibility

-- if the current guess has better possibility then update the record
check_each_remaining_guess_kooi2 [] best_prob best_guess = best_guess
check_each_remaining_guess_kooi2 guess_list best_prob best_guess = 
	let next_guess = head guess_list in 
	let prob =  check_each_outcome next_guess olist guess_list 0 in 
	-- if probability is greater than best probability
	if prob > best_prob then check_each_remaining_guess_kooi2 (tail guess_list) prob next_guess
	else check_each_remaining_guess_kooi2 (tail guess_list) best_prob best_guess

choose_kooi2 plist = 
	-- have an input of possible pattern list and return the choosen one
	check_each_remaining_guess_kooi2 plist (0) (head plist)
	-- best probability was 0
-- 

play_kooi2' :: Pattern -> [Pattern] -> Int -> Maybe Int
-- Same arguments as above
play_kooi2' _ _ 0 = Nothing
play_kooi2' answer plist steps_left = 
	let guess = choose_kooi2 plist in 
	let same = samepos answer guess in 
	let occurs =  occurscount answer guess in 
	let plist' = list_filter same occurs guess (filter (\x -> not (x == guess)) plist) in 
	if same == 4 then  Just (board_size - steps_left)
	else if steps_left > 0 then play_kooi2' answer plist' (steps_left -1) 
	else Nothing

first_guess_kooi2  = [Red, Red, Yellow, Yellow]

play_kooi2 answer =
	let same = samepos answer first_guess_kooi2 in 
	let occurs = occurscount answer first_guess_kooi2 in 
	if same == 4 then Just 1
	else play_kooi2' answer (list_filter same occurs first_guess (filter (\x -> not(x == first_guess_kooi2)) all_list)) (board_size - 1)


-- part 5

-- I don't know how to make the base of the logarithm depending on the size of the partition. 
-- I feel these partitions are going to be the same size?
-- So here is the old and fixed one 

--check_each_outcome_partition :: Pattern -> [Pattern] -> [pattern] -> [Int] -> [Int]
check_each_outcome_partition guess []  reduced_list partition_list = partition_list
check_each_outcome_partition guess outcome_list  reduced_list partition_list = 
	let outcome = head outcome_list in 
	let count = (check_outcome guess outcome reduced_list 0) in 
 	check_each_outcome_partition guess (tail outcome_list) reduced_list (count : partition_list)
	

--construct all possible outcome
--olist = [(a,b)| a<-[0..4], b<-[0..(4-a)]]
base :: Int -> Double 
base x = logBase (fromIntegral 10) (fromIntegral x)
--I don't understand why the size of partition would change and how this would lead to the chagne of the base

obtain_entropy next_guess olist guess_list = 
	let partition_list = check_each_outcome_partition next_guess olist guess_list [] in 
	let lsize = length guess_list in 
	let f = (\acc x -> (acc + (base x) - (base lsize))) in 
	foldl f (fromIntegral 0) partition_list 

check_each_remaining_guess_kooi3 [] min_entropy best_guess = best_guess
check_each_remaining_guess_kooi3 guess_list min_entropy best_guess = 
	let next_guess = head guess_list in 
	let entropy =  obtain_entropy next_guess olist guess_list in 
	if entropy < min_entropy then check_each_remaining_guess (tail guess_list) entropy next_guess
	else check_each_remaining_guess_kooi3 (tail guess_list) min_entropy best_guess


choose_kooi3 plist = 
	-- have an input of possible pattern list and return the choosen one
	check_each_remaining_guess_kooi3 plist (999999) (head plist)


play_kooi3' :: Pattern -> [Pattern] -> Int -> Maybe Int
-- Same arguments as above
play_kooi3' _ _ 0 = Nothing
play_kooi3' answer plist steps_left = 
	let guess = choose_kooi3 plist in 
	let same = samepos answer guess in 
	let occurs =  occurscount answer guess in 
	let plist' = list_filter same occurs guess (filter (\x -> not (x == guess)) plist) in 
	if same == 4 then  Just (board_size - steps_left)
	else if steps_left > 0 then play_kooi3' answer plist' (steps_left -1) 
	else Nothing


play_kooi3 answer =
	let same = samepos answer first_guess in 
	let occurs = occurscount answer first_guess in 
	if same == 4 then Just 1
	else play_kooi3' answer (list_filter same occurs first_guess (filter (\x -> not(x == first_guess)) all_list)) (board_size - 1)

-- part 6

-- sorry, I don't think my brain is working anymore.
-- MG: Sleep more!

-- Summary:
-- To run the code, simply run as folows.
-- play_naive your_answer_pattern
-- play_knuth your_answer_pattern
-- play_kooi your_answer_pattern
-- play_kooi2 your_answer_pattern
-- play_kooi3 your_answer_pattern


-- Extra (you may ignore the following)
-- I kind of feel like to see how these algorithms work. I want to do a test on then actually:

-- MG: here is one way to do it:
testAll :: IO ()
testAll = do
	x <- getRandomPattern
	print x
	print (play_naive x)
	print (play_knuth x)
	print (play_kooi x)
	print (play_kooi2 x)
	print (play_kooi3 x)
