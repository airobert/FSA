
-- Shuai Wang
-- 11108339
-- MSc Logic, ILLC

module FSAlab1 where
import GHC.Integer

mylength :: [a] -> Int

mylength [] = 0
mylength (_:xs) = 1 + (mylength xs)

--myelem :: a -> [a] -> Bool

-- MG: please add type definitions for all top-level stuff.

myelem _ [] = False
myelem x (y:xs) = 
	if (x == y) 
		then True
	else myelem x xs

myor = foldr (||) False

mymap f lst = foldr (\x acc -> (f x):acc) [] lst

myfilter p lst = foldr (\x acc -> if p x then x:acc else acc) [] lst

myadd lst = foldr (\x acc -> x + acc) 0 lst


-- fold right --------------------------
myreverser lst = foldr (\x acc -> acc ++ [x]) [] lst

-- fold left --------------------------
myreversel lst = foldl (\acc x -> x:acc) [] lst

-- foldr can work on infinite list but not foldl. 
-- Since foldr will immediately return the application of f to the recursive case 
-- of folding over the rest of the list, if f is able to produce some part 
-- of its result without reference to the recursive case, and the rest of the 
-- result is never demanded, then the recursion will stop. This allows right folds 
-- to operate on infinite lists.

-- foldl is more effecient since it use tail recursion since foldl will immediately
-- call itself with new parameters until it reaches the end of the list. Compiled as 
-- a loop, this tail recursion can be efficiently, but can't deal with infinite 
-- lists at all it will recurse forever in an infinite loop.

------------------------------------ Ref: https://wiki.haskell.org/Fold


--convert a Integer to a list of Int

toIntList' :: Integer -> [Integer]
toIntList' v =  
	if (ltInteger v 10)
		then [v] 
	else (modInteger v 10) : (toIntList' (divInteger v 10))

toIntList :: Integer -> [Int]
toIntList x = reverse(map fromIntegral ((toIntList' x)))

--odd digits--
--oddDigits' :: forall t. [t] -> [t]
oddDigits' [] = []
oddDigits' [x] = [x]
oddDigits' lst = (head lst) : (oddDigits' (tail (tail lst))) 
oddDigits lst = oddDigits' (reverse lst)

-- even digits
evenDigits lst =  oddDigits (reverse (tail (reverse lst)))

-- sum of (odd digits * 2)
--sumeven :: forall b. Integral b => [b] -> b
sumeven lst = foldl (\acc x -> (((x*2) `mod`10) + ((x*2) `div`10)) + acc) 0 lst

--sumodd :: forall a. Num a => [a] -> a
sumodd lst = sum lst

sumall :: Integer -> Int
sumall x = ((sumodd(oddDigits(toIntList x))) + sumeven(evenDigits(toIntList x))) 

luhn_digit :: Integer -> Int
luhn_digit x = (sumall x) `mod` 10 

luhn_valid :: Integer -> Bool
luhn_valid x = (luhn_digit x) == 0 

digit :: Integer -> Int
digit x  = 
	if (luhn_digit (4 * x) == 0) 
		then 0 
	else  10 - (luhn_digit (4 * x))


-- a tool to verify the code: 
-- http://planetcalc.com/2464/
-- Test cases: http://www.freeformatter.com/credit-card-number-generator-validator.html

--American Express: the digit is the same as the last digit
last_dig :: Integer -> Int
last_dig x = head (reverse (toIntList x))
--is_American_express x = (last_dig x) == (digit x)

--https://en.wikipedia.org/wiki/Bank_card_number

is_American_express :: Integer -> Bool
is_American_express x = (luhn_valid x) &&
	((head (toIntList x)) == 3) && ( elem (head (tail (toIntList x))) [4,7]) && (mylength (toIntList x) == 15)

is_Master :: Integer -> Bool
is_Master x = (luhn_valid x) &&
	((head (toIntList x)) == 5) && (elem (head (tail (toIntList x))) [1,2,3,4,5])
	&& (mylength (toIntList x) == 16)

is_Visa :: Integer -> Bool
is_Visa x = (luhn_valid x) &&
	((head (toIntList x)) == 4) && (mylength (toIntList x) == 16 || mylength (toIntList x) == 13)


--  Question: Crime Scene Investigation

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]
honest :: [Boy]
guilty :: Boy

--Matthew: Carl didn’t do it, and neither did I.

--Peter: It was Matthew or it was Jack.

--Jack: Matthew and Peter are both lying.

--Arnold: Matthew or Peter is speaking the truth, but not both.

--Carl: What Arnold says is not true.

says :: Boy -> Boy -> Bool

says Matthew x = 
	not ((x == Matthew) || (x ==  Carl))

says Peter x = 
	(x == Matthew) || (x == Jack)

says Jack x = 
	not (says Matthew x) && not (says Peter x)

says Arnold x = 
	 ((says Matthew x) || (says Peter x)) && 
	 not ((says Matthew x) && (says Peter x))

says Carl x = 
	not (says Arnold x)


-- accusers are those who accuse an individual
accusers :: Boy -> [Boy]

accusers who = 
	filter (\x -> says x who) boys

-- get an element from a list of list. The element is of length 3
get_three ::[[a]] -> [a]

get_three [] = error "game setting not well-designed"

get_three (x:xs) = 
	if ((length x)  == 3)
		then x 
	else (get_three xs)
-- MG: this can be done nicer with filter

-- Those who are honest are those three who accuse the same one

honest = get_three (map accusers boys)

-- The one who got accused by three is the one who is guilty
get_guilty :: [Boy] -> Boy
get_guilty [] = error "game setting not well-designed"
get_guilty (x:xs) = 
	if ((length (accusers x))  == 3)
		then x 
	else (get_guilty xs)

guilty = get_guilty boys


-- Result :

-- Matthew, Peter and Carl are honest
-- Jack is guilty

-- correct, good!

-- Bonus Part
-- https://projecteuler.net/problem=9

triplet :: Int -> [(Int,Int,Int)]
triplet x = [(a,b, (x - a - b)) | a<-[1..x], b<-[a..x],  (a * a + b * b ) == (x - a - b) *  (x - a - b)]

-- result is a = 200, b = 375, c = 425 

-- MG: and how do you find it with your function?
