  module FSAlab3
  where 
  import Data.List
  import System.Random
  import Picosat
  -- use Picosat in this project 
  -- https://github.com/sdiehl/haskell-picosat

  type Row    = Int 
  type Column = Int 
  type Value  = Int
  type Grid   = [[Value]]
  
  positions, values :: [Int]
  positions = [1..9]
  values    = [1..9] 
  
  blocks :: [[Int]]
  blocks = [[1..3],[4..6],[7..9]]

  indexedblocks :: [(Int,Int)]
  indexedblocks = [(x,y)| x <- [1..3], y <- [1..3]]

  blocks' :: [[Int]]
  blocks' = [[2..4],[6..8]]

  showVal :: Value -> String
  showVal 0 = " "
  showVal d = show d

  showRow :: [Value] -> IO()
  showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
   do  putChar '|'         ; putChar ' '
       putStr (showVal a1) ; putChar ' '
       putStr (showVal a2) ; putChar ' '
       putStr (showVal a3) ; putChar ' '
       putChar '|'         ; putChar ' '
       putStr (showVal a4) ; putChar ' '
       putStr (showVal a5) ; putChar ' '
       putStr (showVal a6) ; putChar ' '
       putChar '|'         ; putChar ' '
       putStr (showVal a7) ; putChar ' '
       putStr (showVal a8) ; putChar ' '
       putStr (showVal a9) ; putChar ' '
       putChar '|'         ; putChar '\n'

  showGrid :: Grid -> IO()
  showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
   do putStrLn ("+-------+-------+-------+")
      showRow as; showRow bs; showRow cs
      putStrLn ("+-------+-------+-------+")
      showRow ds; showRow es; showRow fs
      putStrLn ("+-------+-------+-------+")
      showRow gs; showRow hs; showRow is
      putStrLn ("+-------+-------+-------+")

  type Sudoku = (Row,Column) -> Value

  columns :: Sudoku -> [[Value]]
  columns s = [[s (r, c) | r <- [1..9]] | c <- [1..9]] 

  sud2grid :: Sudoku -> Grid
  sud2grid s = 
    [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 
  
  -- this is the actual sudoku construction!
  grid2sud :: Grid -> Sudoku
  grid2sud gr = \ (r,c) -> pos gr (r,c) 
    where 
    pos :: [[a]] -> (Row,Column) -> a 
    pos gr (r,c) = (gr !! (r-1)) !! (c-1)

  showSudoku :: Sudoku -> IO()
  showSudoku = showGrid . sud2grid

  bl :: Int -> [Int]
  bl x = concat $ filter (elem x) blocks 

  bl' :: Int -> [Int]
  bl' x = concat $ filter (elem x) blocks'

  subGrid :: Sudoku -> (Row,Column) -> [Value]
  subGrid s (r,c) = 
    [ s (r',c') | r' <- bl r, c' <- bl c ]

  subgridList :: Sudoku -> [[Value]] 
  subgridList s =
    [subGrid s (r, c) |r <- [1,4,7], c <- [1,4,7]]

-- NRC sudoku
  subGrid' :: Sudoku -> (Row,Column) -> [Value]
  subGrid' s (r,c) = 
    [ s (r',c') | r' <- bl' r, c' <- bl' c ]

  subgridList' :: Sudoku -> [[Value]] 
  subgridList' s =
    [subGrid' s (r, c) |r <- [2,7], c <- [2,7]]

-- Cross Sudoku
-- the values 

  subgridCrossList :: Sudoku -> [[Value]]
  subgridCrossList s = 
    [[s (r, r) | r <- positions], [s (r, ((9 - r) +1)) | r <- positions]]

  subGrid'' :: Sudoku -> (Row, Column) -> [Value]
  subGrid'' s (r,c) =
    if ((r == 5) && (c == 5)) then nub (concat (subgridCrossList s))
    else if r == c then (head (subgridCrossList s))
      else if r == 10 - c then ((subgridCrossList s) !! 1)
        else []

  freeInSeq :: [Value] -> [Value]
  freeInSeq seq = values \\ seq 

  freeInRow :: Sudoku -> Row -> [Value]
  freeInRow s r = 
    freeInSeq [ s (r,i) | i <- positions  ]

  freeInColumn :: Sudoku -> Column -> [Value]
  freeInColumn s c = 
    freeInSeq [ s (i,c) | i <- positions ]

  freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
  freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

  freeInSubgrid' :: Sudoku -> (Row, Column) -> [Value]
  freeInSubgrid' s (r,c) = freeInSeq (subGrid' s (r,c))

  freeInCross :: Sudoku -> (Row, Column) -> [Value]
  freeInCross s (r,c) = freeInSeq (subGrid'' s (r,c))


  freeAtPos :: Sudoku -> (Row,Column) -> [Value]
  freeAtPos s (r,c) = 
    (freeInRow s r) 
     `intersect` (freeInColumn s c) 
     `intersect` (freeInSubgrid s (r,c)) 

-- free in NRC Sudoku
  freeAtPos' :: Sudoku -> (Row,Column) -> [Value]
  freeAtPos' s (r,c) = 
    (freeInRow s r) 
     `intersect` (freeInColumn s c) 
     `intersect` (freeInSubgrid s (r,c))
     `intersect` (freeInSubgrid' s (r,c))

-- free in Cross Sudoku
  freeAtPos''  :: Sudoku -> (Row,Column) -> [Value]
  freeAtPos'' s (r,c) = 
    (freeInRow s r)
    `intersect` (freeInColumn s c) 
     `intersect` (freeInSubgrid s (r,c))
     `intersect` (freeInCross s (r,c))


  injective :: Eq a => [a] -> Bool
  injective xs = nub xs == xs

  rowInjective :: Sudoku -> Row -> Bool
  rowInjective s r = injective vs where 
     vs = filter (/= 0) [ s (r,i) | i <- positions ]

  colInjective :: Sudoku -> Column -> Bool
  colInjective s c = injective vs where 
     vs = filter (/= 0) [ s (i,c) | i <- positions ]

  subgridInjective :: Sudoku -> (Row,Column) -> Bool
  subgridInjective s (r,c) = injective vs where 
     vs = filter (/= 0) (subGrid s (r,c))

  subgridInjective' :: Sudoku -> (Row,Column) -> Bool
  subgridInjective' s (r,c) = injective vs where 
     vs = filter (/= 0) (subGrid' s (r,c))

  subgridInjective'' :: Sudoku -> (Row,Column) -> Bool
  subgridInjective'' s (r,c) = injective vs where 
     vs = filter (/= 0) (subGrid'' s (r,c))


  consistent :: Sudoku -> Bool
  consistent s = and $
                 [ rowInjective s r |  r <- positions ]
                  ++
                 [ colInjective s c |  c <- positions ]
                  ++
                 [ subgridInjective s (r,c) | 
                      r <- [1,4,7], c <- [1,4,7]]

  consistent' :: Sudoku -> Bool
  consistent' s = and $
                 [ rowInjective s r |  r <- positions ]
                  ++
                 [ colInjective s c |  c <- positions ]
                  ++
                 [ subgridInjective s (r,c) | 
                      r <- [1,4,7], c <- [1,4,7]]
                  ++
                  [ subgridInjective' s (r,c) | 
                      r <- [2,7], c <- [2,7]]

  consistent'' :: Sudoku -> Bool
  consistent'' s = and $
                 [ rowInjective s r |  r <- positions ]
                  ++
                 [ colInjective s c |  c <- positions ]
                  ++
                 [ subgridInjective s (r,c) | 
                      r <- [1,4,7], c <- [1,4,7]]
                  ++
                  [ subgridInjective'' s (r, r) | 
                      r <- positions]
                  ++ [ subgridInjective'' s (r, 10 - r) | 
                      r <- positions]

  extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
  extend = update

  update :: Eq a => (a -> b) -> (a,b) -> a -> b 
  update f (y,z) x = if x == y then z else f x 


  type Constraint = (Row,Column,[Value])

  type Node = (Sudoku,[Constraint])
 
  showNode :: Node -> IO()
  showNode = showSudoku . fst

  solved  :: Node -> Bool
  solved = null . snd

  extendNode :: Node -> Constraint -> [Node]
  extendNode (s,constraints) (r,c,vs) = 
     [(extend s ((r,c),v),
       sortBy length3rd $ 
           prune (r,c,v) constraints) | v <- vs ]

  extendNode' :: Node -> Constraint -> [Node]
  extendNode' (s,constraints) (r,c,vs) = 
     [(extend s ((r,c),v),
       sortBy length3rd $ 
           prune' (r,c,v) constraints) | v <- vs ]

  extendNode'' :: Node -> Constraint -> [Node]
  extendNode'' (s,constraints) (r,c,vs) = 
     [(extend s ((r,c),v),
       sortBy length3rd $ 
           prune'' (r,c,v) constraints) | v <- vs ]

  extendNode''' :: Node -> Constraint -> [Node]
  extendNode''' (s,constraints) (r,c,vs) = 
     [(extend s ((r,c),v),
       sortBy length3rd $ 
           prune''' (r,c,v) constraints) | v <- vs ]

  length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
  length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

  prune :: (Row,Column,Value) 
        -> [Constraint] -> [Constraint]
  prune _ [] = []
  prune (r,c,v) ((x,y,zs):rest)
    | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
    | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
    | sameblock (r,c) (x,y) = 
          (x,y,zs\\[v]) : prune (r,c,v) rest
    | otherwise = (x,y,zs) : prune (r,c,v) rest

  prune' :: (Row,Column,Value) 
        -> [Constraint] -> [Constraint]
  prune' _ [] = []
  prune' (r,c,v) ((x,y,zs):rest)
    | r == x = (x,y,zs\\[v]) : prune' (r,c,v) rest
    | c == y = (x,y,zs\\[v]) : prune' (r,c,v) rest
    | sameblock (r,c) (x,y) = 
          (x,y,zs\\[v]) : prune' (r,c,v) rest
    | sameblock' (r,c) (x,y) = 
          (x,y,zs\\[v]) : prune' (r,c,v) rest
    | otherwise = (x,y,zs) : prune' (r,c,v) rest

  prune'' :: (Row,Column,Value) 
        -> [Constraint] -> [Constraint]
  prune'' _ [] = []
  prune'' (r,c,v) ((x,y,zs):rest)
    | r == x = (x,y,zs\\[v]) : prune'' (r,c,v) rest
    | c == y = (x,y,zs\\[v]) : prune'' (r,c,v) rest
    | sameblock (r,c) (x,y) = 
          (x,y,zs\\[v]) : prune'' (r,c,v) rest
    | sameblock'' (r,c) (x,y) = 
          (x,y,zs\\[v]) : prune'' (r,c,v) rest
    | otherwise = (x,y,zs) : prune'' (r,c,v) rest


  prune''' :: (Row,Column,Value) 
        -> [Constraint] -> [Constraint]
  prune''' _ [] = []
  prune''' (r,c,v) ((x,y,zs):rest)
    | r == x = (x,y,zs\\[v]) : prune''' (r,c,v) rest
    | c == y = (x,y,zs\\[v]) : prune''' (r,c,v) rest
    | sameblock (r,c) (x,y) = 
          (x,y,zs\\[v]) : prune''' (r,c,v) rest
    | sameblock' (r,c) (x,y) = 
          (x,y,zs\\[v]) : prune''' (r,c,v) rest
    | sameblock'' (r,c) (x,y) = 
          (x,y,zs\\[v]) : prune''' (r,c,v) rest
    | otherwise = (x,y,zs) : prune''' (r,c,v) rest
  

  
  sameblock :: (Row,Column) -> (Row,Column) -> Bool
  sameblock (r,c) (x,y) = bl r == bl x && bl c == bl y 

  sameblock' :: (Row,Column) -> (Row,Column) -> Bool
  sameblock' (r,c) (x,y) = bl' r == bl' x && bl' c == bl' y 

  sameblock'' ::(Row,Column) -> (Row,Column) -> Bool
  sameblock'' (r,c) (x,y) = (r == c && x == y) || (((r == (10 - c)) || (c == (10 - r))) && ((x == (10 - y)) || (y == (10 - x))))
  -- MG: Maybe better call this "samediagonal"?

-- all other nodes in the same block

  allsameblock :: (Row, Column) -> [(Row, Column)]
  allsameblock (r, c) = 
    [(r', c') | r' <- values, c' <- values , sameblock (r,c) (r', c')]

  blockbyIndex (r, c) = 
    allsameblock (r *2 + 1, c * 2 + 1)

  initNode :: Grid -> [Node]
  initNode gr = let s = grid2sud gr in 
                if (not . consistent) s then [] 
                else [(s, constraints s)]

  initNode' :: Grid -> [Node]
  initNode' gr = let s = grid2sud gr in 
                if (not . consistent) s then [] 
                else [(s, constraints' s)]

  openPositions :: Sudoku -> [(Row,Column)]
  openPositions s = [ (r,c) | r <- positions,  
                              c <- positions, 
                              s (r,c) == 0 ]

  constraints :: Sudoku -> [Constraint] 
  constraints s = sortBy length3rd 
      [(r,c, freeAtPos s (r,c)) | 
                         (r,c) <- openPositions s ]

  constraints' :: Sudoku -> [Constraint] 
  constraints' s = sortBy length3rd 
      [(r,c, freeAtPos' s (r,c)) | 
                         (r,c) <- openPositions s ]

-- depth first search

  search :: (node -> [node]) 
         -> (node -> Bool) -> [node] -> [node]
  search children goal [] = []
  search children goal (x:xs) 
    | goal x    = x : search children goal xs
    | otherwise = search children goal ((children x) ++ xs)



  solveNs :: [Node] -> [Node]
  solveNs = search succNode solved 

  
  succNode :: Node -> [Node]
  succNode (s,[]) = []
  succNode (s,p:ps) = extendNode (s,ps) p 

  solveNs' :: [Node] -> [Node]
  solveNs' = search succNode' solved 
  
  succNode' :: Node -> [Node]
  succNode' (s,[]) = []
  succNode' (s,p:ps) = extendNode' (s,ps) p 

  solveAndShow :: Grid -> IO[()]
  solveAndShow gr = solveShowNs (initNode gr)

  solveAndShow' :: Grid -> IO[()]
  solveAndShow' gr = solveShowNs' (initNode' gr)
  
  solveShowNs :: [Node] -> IO[()]
  solveShowNs = sequence . fmap showNode . solveNs

  solveShowNs' :: [Node] -> IO[()]
  solveShowNs' = sequence . fmap showNode . solveNs'

  example1 :: Grid
  example1 = [[5,3,0,0,7,0,0,0,0],
              [6,0,0,1,9,5,0,0,0],
              [0,9,8,0,0,0,0,6,0],
              [8,0,0,0,6,0,0,0,3],
              [4,0,0,8,0,3,0,0,1],
              [7,0,0,0,2,0,0,0,6],
              [0,6,0,0,0,0,2,8,0],
              [0,0,0,4,1,9,0,0,5],
              [0,0,0,0,8,0,0,7,9]]

  s1 = grid2sud example1

  example2 :: Grid
  example2 = [[0,3,0,0,7,0,0,0,0],
              [6,0,0,1,9,5,0,0,0],
              [0,9,8,0,0,0,0,6,0],
              [8,0,0,0,6,0,0,0,3],
              [4,0,0,8,0,3,0,0,1],
              [7,0,0,0,2,0,0,0,6],
              [0,6,0,0,0,0,2,8,0],
              [0,0,0,4,1,9,0,0,5],
              [0,0,0,0,8,0,0,7,9]]

  s2 = grid2sud example2

  example3 :: Grid
  example3 = [[1,0,0,0,3,0,5,0,4],
              [0,0,0,0,0,0,0,0,3],
              [0,0,2,0,0,5,0,9,8], 
              [0,0,9,0,0,0,0,3,0],
              [2,0,0,0,0,0,0,0,7],
              [8,0,3,0,9,1,0,6,0],
              [0,5,1,4,7,0,0,0,0],
              [0,0,0,3,0,0,0,0,0],
              [0,4,0,0,0,9,7,0,0]]

  example4 :: Grid
  example4 = [[1,2,3,4,5,6,7,8,9],
              [2,0,0,0,0,0,0,0,0],
              [3,0,0,0,0,0,0,0,0],
              [4,0,0,0,0,0,0,0,0],
              [5,0,0,0,0,0,0,0,0],
              [6,0,0,0,0,0,0,0,0],
              [7,0,0,0,0,0,0,0,0],
              [8,0,0,0,0,0,0,0,0],
              [9,0,0,0,0,0,0,0,0]]

  example5 :: Grid
  example5 = [[1,0,0,0,0,0,0,0,0],
              [0,2,0,0,0,0,0,0,0],
              [0,0,3,0,0,0,0,0,0],
              [0,0,0,4,0,0,0,0,0],
              [0,0,0,0,5,0,0,0,0],
              [0,0,0,0,0,6,0,0,0],
              [0,0,0,0,0,0,7,0,0],
              [0,0,0,0,0,0,0,8,0],
              [0,0,0,0,0,0,0,0,9]]

  example6 :: Grid
  example6 = [[0,0,0,3,0,0,0,0,0],
              [0,0,0,7,0,0,3,0,0],
              [2,0,0,0,0,0,0,0,8],
              [0,0,6,0,0,5,0,0,0],
              [0,9,1,6,0,0,0,0,0],
              [3,0,0,0,7,1,2,0,0],
              [0,0,0,0,0,0,0,3,1],
              [0,8,0,0,4,0,0,0,0],
              [0,0,2,0,0,0,0,0,0]]
  example7 :: Grid
  example7 = [[0,3,0,0,7,0,0,0,0],
              [6,0,0,0,9,0,0,0,0],
              [0,9,8,0,0,0,0,6,0],
              [8,0,0,0,6,0,0,0,3],
              [4,0,0,8,0,3,0,0,1],
              [7,0,0,0,2,0,0,0,6],
              [0,6,0,0,0,0,2,8,0],
              [0,0,0,4,1,9,0,0,5],
              [0,0,0,0,8,0,0,7,9]]
  s7 = grid2sud example7


  emptyN :: Node
  emptyN = (\ _ -> 0,constraints (\ _ -> 0))

  getRandomInt :: Int -> IO Int
  getRandomInt n = getStdRandom (randomR (0,n))

  getRandomItem :: [a] -> IO [a]
  getRandomItem [] = return []
  getRandomItem xs = do n <- getRandomInt maxi
                        return [xs !! n]
                     where maxi = length xs - 1

  randomize :: Eq a => [a] -> IO [a]
  randomize xs = do y <- getRandomItem xs 
                    if null y 
                      then return []
                      else do ys <- randomize (xs\\y)
                              return (head y:ys)

  sameLen :: Constraint -> Constraint -> Bool
  sameLen (_,_,xs) (_,_,ys) = length xs == length ys

  getRandomCnstr :: [Constraint] -> IO [Constraint]
  getRandomCnstr cs = getRandomItem (f cs) 
    where f [] = []
          f (x:xs) = takeWhile (sameLen x) (x:xs)

  rsuccNode :: Node -> IO [Node]
  rsuccNode (s,cs) = do xs <- getRandomCnstr cs
                        if null xs 
                          then return []
                          else return 
                            (extendNode (s,cs\\xs) (head xs))

  rsuccNode' :: Node -> IO [Node]
  rsuccNode' (s,cs) = 
    do
      xs <- getRandomCnstr cs
      if null xs then return [] else return (extendNode' (s,cs\\xs) (head xs))

  rsuccNode'' :: Node -> IO [Node]
  rsuccNode'' (s,cs) = 
    do
      xs <- getRandomCnstr cs
      if null xs then return [] else return (extendNode'' (s,cs\\xs) (head xs))
  
  rsuccNode''' :: Node -> IO [Node]
  rsuccNode''' (s,cs) = 
    do
      xs <- getRandomCnstr cs
      if null xs then return [] else return (extendNode''' (s,cs\\xs) (head xs))


  rsolveNs :: [Node] -> IO [Node]
  rsolveNs ns = rsearch rsuccNode solved (return ns)

  rsolveNs' :: [Node] -> IO [Node]
  rsolveNs' ns = rsearch rsuccNode' solved (return ns)
  --rsolveNs' ns = rsearch rsuccNode' solved (return ns)

  rsolveNs'' :: [Node] -> IO [Node]
  rsolveNs'' ns = rsearch rsuccNode'' solved (return ns)

  rsolveNs''' :: [Node] -> IO [Node]
  rsolveNs''' ns = rsearch rsuccNode''' solved (return ns)

  rsearch :: (node -> IO [node]) 
              -> (node -> Bool) -> IO [node] -> IO [node]
  rsearch succ goal ionodes = 
    do xs <- ionodes 
       if null xs 
         then return []
         else 
           if goal (head xs) 
             then return [head xs]
             else do ys <- rsearch succ goal (succ (head xs))
                     if (not . null) ys 
                        then return [head ys]
                        else if null (tail xs) then return []
                             else 
                               rsearch 
                                 succ goal (return $ tail xs)

  genRandomSudoku :: IO Node
  genRandomSudoku = do [r] <- rsolveNs [emptyN]
                       return r

  genRandomNRC :: IO Node
  genRandomNRC = 
    do [r] <- (rsolveNs' [emptyN])
       return r

  randomS = genRandomSudoku >>= showNode

  randomNRC  = genRandomNRC >>= showNode

  uniqueSol :: Node -> Bool
  uniqueSol node = singleton (solveNs [node]) where 
    singleton [] = False
    singleton [_] = True
    singleton (_:_:_) = False

  eraseS :: Sudoku -> (Row,Column) -> Sudoku
  eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                       | otherwise      = s (x,y)

  eraseN :: Node -> (Row,Column) -> Node
  eraseN n (r,c) = (s, constraints s) 
    where s = eraseS (fst n) (r,c) 

  fp :: Eq a => (a -> a) -> a -> a 
  fp f = until (\ x -> x == f x) f

  eraseBlock' :: Node -> [(Row, Column)] -> Node
  eraseBlock' n xs =
    if xs == []  
      then n
      else (eraseN ((eraseBlock' n (tail xs))) (head xs))


  minimalize :: Node -> [(Row,Column)] -> Node
  minimalize n [] = n
  minimalize n ((r,c):rcs) | uniqueSol n' = minimalize n' rcs
                           | otherwise    = minimalize n  rcs
    where n' = eraseN n (r,c)

  filledPositions :: Sudoku -> [(Row,Column)]
  filledPositions s = [ (r,c) | r <- positions,  
                                c <- positions, s (r,c) /= 0 ]

  genProblem :: Node -> IO Node
  genProblem n = do ys <- randomize xs
                    return (minimalize n ys)
     where xs = filledPositions (fst n)

  main :: IO ()
  main = do [r] <- rsolveNs [emptyN]
            showNode r
            s  <- genProblem r
            showNode s


-- exercise 1

-- precondition

  precondition :: Grid -> Bool
  precondition g = (all (\x -> (length x) == 9) g)  &&  ((length g) == 9)
    -- all elements are between 0 and 9
    && (all (\row -> all (\x -> elem x (values ++ [0])) row) g)
    -- no repeated elememtents in each subgrid, row, column
    && (all (\row -> (length (filter ((/=)0) row) == (length (nub(filter ((/=)0) row))))) g)
    && (all (\row -> (length (filter ((/=)0) row) == (length (nub(filter ((/=)0) row))))) (columns (grid2sud g)))
    && (all (\row -> (length (filter ((/=)0) row) == (length (nub(filter ((/=)0) row))))) (subgridList (grid2sud g)))
    --and of course also consistent
    && consistent (grid2sud g)

-- postcondition is that each column and row and subgrid has all 1 - 9.
  postcondition :: Grid -> Bool
  postcondition g = (all (\row -> all (\x -> elem x row) [1..9]) g)
    && (all (\row -> all (\x -> elem x row) [1..9]) (columns (grid2sud g)))
    && (all (\row -> all (\x -> elem x row) [1..9]) (subgridList (grid2sud g)))
    && consistent (grid2sud g)

-- given a grid. test the depth first search method
  ex1 :: Grid -> Bool
  ex1 g = 
    (precondition g) && (all (\x -> postcondition (sud2grid (fst x))) (solveNs (initNode g)))

-- exercise 2
-- The answer for exercise 2 is: 
-- *FSAlab3> solveAndShow' example6
-- +-------+-------+-------+
-- | 4 7 8 | 3 9 2 | 6 1 5 |
-- | 6 1 9 | 7 5 8 | 3 2 4 |
-- | 2 3 5 | 4 1 6 | 9 7 8 |
-- +-------+-------+-------+
-- | 7 2 6 | 8 3 5 | 1 4 9 |
-- | 8 9 1 | 6 2 4 | 7 5 3 |
-- | 3 5 4 | 9 7 1 | 2 8 6 |
-- +-------+-------+-------+
-- | 5 6 7 | 2 8 9 | 4 3 1 |
-- | 9 8 3 | 1 4 7 | 5 6 2 |
-- | 1 4 2 | 5 6 3 | 8 9 7 |
-- +-------+-------+-------+
-- [()]

-- and this is the only solution

-- exercise 3
-- seems there is always indentation problem if I simply do a copy-paste

  main3 :: IO ()
  main3 = 
    do 
      [r] <- rsolveNs' [emptyN]
      showNode r
      s  <- genProblem r
      showNode s

-- exercise 4
  main4 :: IO ()
  main4 = 
    do 
      [r] <- rsolveNs' [emptyN]
      showNode r
      s  <- genProblem r
      showNode s
      [x] <- (rsolveNs' [s])
      print (uniqueSol x)
      -- MG: This check is useless, x is a solution!
      --     I thoink you'll want to do this:
      print (uniqueSol s)


--  exercise 5
-- generate three empty blocks. There are in total 9 blocks

  eraseBlock3 :: Node -> IO Node
  eraseBlock3 n = 
    do 
      l <- randomize indexedblocks
      let list = blockbyIndex (head l)
      --let list = (allsameblock (head l))
      --return list
      let n1 =  (eraseBlock' n list)
      let list2 = (blockbyIndex (head (tail l)))
      let n2 = (eraseBlock' n1 list2)
      let list3 = (blockbyIndex (head (tail (tail l))))
      let n3 = (eraseBlock' n2 list3)
      return n3

-- print a random sudoku with 3 blocks deleted
  test1 = 
    do 
      x <- genRandomSudoku
      n <- (eraseBlock3 x)
      showNode n 

-- MG: You should still check that they have unique solutions!

-- erase any amount of blocks

-- get a random sudoku with n blocks erased. Assume 3 < n < 
  eraseNBlock ::  Node -> Int -> [(Row, Column)] -> Node
  eraseNBlock n bn bindexlist = 
    if bn == 0  then n
      else 
        do 
          let list = blockbyIndex (head bindexlist)
          eraseNBlock (eraseBlock' n list) (bn - 1) (tail bindexlist)

  test2 bn = 
    do 
      x <- genRandomSudoku
      showNode x
      l <- randomize (indexedblocks)
      let n = (eraseNBlock x bn l)
      showNode n

-- MG: Same here, these do not necessarily have unique solutions.

-- anther fix point solution is:

--  I trie but went into a bizzar situation. I will try again some other time

-- to be continued




--  exericise 6
-- use fix point from empty with succNode
-- first, get the iterateFix function from 
  iterateFix :: Eq a => (a -> a) -> a -> [a]
  iterateFix f = apprx . iterate f where
    apprx (x:y:zs) = if x == y then [x] else x: apprx (y:zs)

  --iterateFix succNode g = 

-- exericse 7 

-- one way to classify sudoku problems is by the (human) solving time [1].
-- However, there is some differnce between human and computer strategies.
-- Humans rather try to solve CSPs by ‘logic techniques’, in other words, 
-- solve by constraint propagation. Moreover humans prefer ‘simple’ techniques 
-- over ‘difficult’ ones (we elaborate on difficulty of logic techniques bellow) [1].
-- Also, according to [1] and my personal experience, we are not good at back-tracking
-- but computers are good at that. One way to design "simple" sudoku problem is to 
-- make the searching tree (of a node) rather wide than deep. Also, because
-- using simple technique is a common way to solve the problem, we can simulate
-- the way human solve problems and test the time/steps as our "value" of difficulty.

-- In contrast, we can define sudokus that take more steps or require quite some
-- back tracking as "hard" sudoku problems. Or simply, we define sudoku that are not
-- easy are hard :)

-- To do such experiment, we combine Monte-Carlo algorithm and some "simple technique"
-- mentioned in [1]. For each node, we perform naked single first. And then delete
-- the number introduced in column, row and block. Keep doing this until we need to make a choice.
-- In each node, we randomly choose one among the ones with the least possible 
-- cases. When completed or reach unsatisfiable situations. we record values as:

-- Following these thoughts. Here is a new search function:

-- Since we are working in pure functional language, we need to find a way to
-- record the total amount of steps left, t

--  type Record = String -> Int
--  init_record :: String -> Int
--  init_record = (\x -> 0)

---- and an update function to update the value
--  update_record :: Record -> (String, Int) -> Record
--  update_record  = update

--  type NodeH =  (Record, Node)

--  solvedH n = solved (snd n)

-- I don't have enough time to sort the rest out, the idea is to modify
-- the search and introduce random choice. At the end of search update the
-- record and the sum is the sore for this sudoku. The sudoku with higher score 
-- will be easier to solve. 

  --searchHuman :: (NodeH -> [NodeH]) -> (NodeH -> Bool) -> [NodeH] -> Record
  --searchHuman children goal [x] = 
  --  if length(children x) == 0 then 
  --    let r = (fst x) in 
  --    let s = r "steps" in 
  --    let v = (r "value") - s in 
  --    update_record r ("value",v) 
  --  else searchHuman children goal ((children x))
  --searchHuman children goal (x:xs) 
  --  | goal x    = 
  --    let s = (fst x) "steps" in 
  --    let r = searchHuman children goal xs in 
  --    update r ("value", s + (r "value")) 
  --  | otherwise = searchHuman children goal ((children x) ++ xs)

-- and the functions being called:

  --extendNodeH :: NodeH -> Constraint -> [NodeH]
  --extendNodeH (rec, (s,constraints)) (r,c,vs) = 
  --   [(rec, (extend s ((r,c),v),
  --     sortBy length3rd $ 
  --         prune (r,c,v) constraints)) | v <- vs ]

  --succNodeH :: NodeH -> [NodeH]
  --succNodeH (r, (s,[])) = []
  --succNodeH (r, (s,p:ps)) = extendNodeH (s,ps) p 


  --solveH :: [NodeH] -> [NodeH]
  --solveH = searchHuman succNodeH solvedH 



-- exercise 9
  main9 :: IO ()
  main9 = 
    do 
      [r] <- rsolveNs'' [emptyN]
      showNode r
      s  <- genProblem r
      showNode s

-- MG: Can you measure the average number of hints here?

--exercise 10

  main10 :: IO ()
  main10 = 
    do 
      [r] <- rsolveNs''' [emptyN]
      showNode r
      s  <- genProblem r
      showNode s

-- Additional: Sudoku as a SAT problem

-- we can reduce a Sudoku problem to a SAT problem [2] by introducing 9 propositional variables for each 
-- cell. 
-- MG: Is this the minimal number of propositions? If less is possible, would that help or not?
-- get a unique number for each 
  get_num r c i = r * 100 + c * 10 + i
  get_neg_num r c i = (-1) * (get_num r c i)

  get_r n = quot n 100
  get_c n = quot n 10 - ((get_r n) * 10)
  get_i n = n `mod` 10

  one_nine = [[get_num r c i | i <- positions]| r <-positions, c <- positions]
  
  unique_cell = [[get_neg_num r c i , get_neg_num r c j]| r <-positions, c <- positions, i <- positions, j<-positions, not (i ==j)]

-- encode each column

  encode_column = [[get_neg_num r i c, get_neg_num r j c]| r <-positions, c <- positions, i <- positions, j<-positions, not (i ==j)]

-- encode each row

  encode_rows = [[get_neg_num i r  c, get_neg_num  j r c]| r <-positions, c <- positions, i <- positions, j<-positions, not (i ==j)]

-- each number

  encode_num g = [ [get_num r c (((g !! (r-1) ) !! (c-1) ))]| r <-positions, c <- positions, not (((g !! (r-1)) !! (c-1) ) == 0)]

-- each subgrid

  --encode_subgrid = [[] | r <-positions, c <- positions , i <- positions]
  posi = [ (r,c) | r <- [1,4,7], c <- [1,4,7]]
  posilist = [[ (r',c') | r' <- [r+0, r+1, r+2], c' <- [c+1, c+2, c+0]]  | (r,c) <- posi]
  encode_subgrid = concat [[[get_neg_num r c i , get_neg_num r' c' i] | (r,c) <- p, (r',c') <- p , not((r,c) == (r',c')) ] |
                       i <- positions, p <- posilist]
  --encode_subgrid = [[get_neg_num r' c' i |  (r,c) <- posi , r' <- [r+1, r+2, r+0], c' <- [c+1, c+2, c+0]] | i <- positions]


--take a grid and solve/verify it
  mainx :: [[Int]] -> IO ()
  mainx g =
    --let encode_subgrid = [] in 
    let f s x = if x < 0 then s else update s ((get_r x, get_c x), get_i x) in
    let init_s = (fst emptyN) in  
    do 
      Solution solution <- solve (one_nine ++ unique_cell ++ encode_rows ++ encode_column ++ (encode_num g) ++ encode_subgrid)
      showSudoku (foldl f init_s solution)


  -- to use, for a grid example1:
  -- mainx example1

  -- MG: Nice, maybe you can explain part of this on Friday?

--  Another way to verify the correctness of the depth search method is therefore to use the mainx function 


-- Refernece list:

-- [1] Human Problem Solving: Sudoku Case Study
-- http://www.fi.muni.cz/reports/files/2011/FIMU-RS-2011-01.pdf


-- [2] Sudoku as SAT problem
-- http://sat.inesc-id.pt/~ines/publications/aimath06.pdf



