
-- Following tutorial at: http://learnyouahaskell.com/input-and-output#hello-world

--main = putStrLn "hello, world"  

--main = do  
--    putStrLn "Hello, what's your name?"  
--    name <- getLine  
--    putStrLn ("Hey " ++ name ++ ", you rock!") 
   --You can read that piece of code like this: perform the I/O action getLine and then bind its result value to name. getLine has a type of IO String, so name will have a type of String. 

-- file input and input-and-output
--http://learnyouahaskell.com/input-and-output#hello-world


-- http://stackoverflow.com/questions/13134825/how-do-functors-work-in-haskell
-- 
import Data.Random.Extras
echo2 :: IO ()
echo2 = putStrLn "Say something" 
        >> getLine >>= putStrLn

-- >> does one thing after another, but the reason I like this is because >>= takes the String that getLine gave us and fed it to putStrLn which takes a String. What if we wanted to just greet the user:

greet1 :: IO ()
greet1 = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name)

greet2 :: IO ()
greet2 =
	putStrLn "What's your name?"
	>> getLine >>= (\name -> putStrLn ("Hello, " ++ name))


--greet2 :: IO ()
--greet2 = putStrLn "What's your name?" 
--         >> getLine >>= (\name -> putStrLn ("Hello, " ++ name))

greet3 :: IO ()
greet3 = putStrLn "What's your name?" 
         >> fmap ("Hello, "++) getLine >>= putStrLn


 -- http://stackoverflow.com/questions/13134825/how-do-functors-work-in-haskell 


-- fmap id  ==  id                    -- identity identity
-- fmap (f . g)  ==  fmap f . fmap g  -- composition

--The (>>) (then) operator works almost identically in do notation and in unsugared code. For example, suppose we have a chain of actions like the following one:
--https://en.wikibooks.org/wiki/Haskell/do_notation



main = do 
	echo2
	sample 3 [1,2,3]






