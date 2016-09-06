--
-- Please take a look at the FSA_report_lab5.pdf for explanations
--

  module FSAlab5
  where 
  import Data.List

-----------------------------------------------

  -- import Yices

  import Math.SMT.Yices.Parser
  import Math.SMT.Yices.Syntax
  import Math.SMT.Yices.Pipe
  import Data.List
  import Control.Monad
 -- set paths to your yices
 -- please download yices 1.0.40. I haven't got time to update the bindings yet
  yicesPath ="./yices-1.0.40/bin/yices" -- your yices path

  -- define data type in Yices for later use.

  int = VarT "int"
  nat = VarT "nat"
  bool = VarT "bool"
  real = VarT "real"

  -- define also some constants
  true = LitB True
  false = LitB False

  -- I find it confusing so I decided to introduce my own types:

---------------------------------------------


  infix 1 ==> 
  
  (==>) :: Bool -> Bool -> Bool
  p ==> q = (not p) || q
 
  forall = flip all

  assert :: (a -> b -> Bool) -> (a -> b) -> a -> b 
  assert p f x = if p x (f x) then f x 
                  else error "assert"

  invar :: (a -> Bool) -> (a -> a) -> a -> a
  invar p = assert (\ x y -> not (p x) || p y) 

  while = until . (not.)

  type Man   = Int
  type Woman = Int
  type Mpref = [(Man,[Woman])]
  type Wpref = [(Woman,[Man])]
  type Engaged = [(Woman,Man)]

  mt :: Mpref
  mt = [(1,[2,1,3]), (2, [3,2,1]), (3,[1,3,2])]
  
  wt :: Wpref
  wt = [(1,[1,2,3]),(2,[3,2,1]), (3,[1,3,2])]

  -- In particuluar, Yices workes with Integer, not Int

  type ManY   = Integer
  type WomanY = Integer
  type MprefY = [[WomanY]] -- this was changed!
  type WprefY = [[ManY]] -- this was changed!
  type EngagedY = [(WomanY, ManY)]

  mt' :: MprefY
  mt' = [[2,1,3], [3,2,1], [1,3,2]]
  
  wt' :: WprefY
  wt' = [[1,2,3], [3,2,1], [1,3,2]]
  -------------------------------------

  type PrefFct = Int -> Int -> Int -> Bool
  
  plist2pfct :: [(Int,[Int])] -> PrefFct 
  plist2pfct table x y y' = 
    let 
      Just prefs = lookup x table
    in elem y (takeWhile (/= y') prefs)

  stableMatch :: (Wpref,Mpref) -> Engaged
  stableMatch (wpref,mpref) = 
    let 
       men     = map fst mpref
       free    = men
       engaged = []
       f       = \ (_,_,x) ->  x
    in
     f $ stable wpref (mpref,free,engaged)

  stable ::  Wpref -> (Mpref,[Man],Engaged) -> (Mpref,[Man],Engaged)
  stable wpref = let 
     wpr = plist2pfct wpref
   in while (\ (_,free, _) -> not (null free))
             (\ (mpr, (m:free), engaged)  ->
              let
                 Just (w:ws) = lookup m mpr
                 match = lookup w engaged
                 mpr' = (m,ws) : (delete (m,w:ws) mpr)
                 (engaged',free') = case match of 
                     Nothing -> ((w,m):engaged,free)
                     Just m' -> 
                       if wpr w m m' then (
                           (w,m) : (delete (w,m') engaged),
                            m':free)
                       else (engaged, m:free)
              in (mpr',free',engaged'))

  makeMatch = stableMatch (mt,wt)

  makeMatch2 = stableMatch (wt,mt)

  freeProp :: (Mpref,[Man],Engaged) -> Bool
  freeProp (mpref, free, engaged) = let 
      men  = map fst mpref
      emen = map snd engaged 
    in forall men (\x -> elem x free == notElem x emen)

  isStable :: (Wpref, Mpref) -> Engaged -> Bool
  isStable (wpref, mpref) engaged = let 
      wf = plist2pfct wpref
      mf = plist2pfct mpref
    in 
      forall engaged (\ (w,m) -> forall engaged 
            (\ (w',m') -> (wf w m' m ==> mf m' w' w)
                           && 
                          (mf m w' w ==> wf w' m' m)))

  stableMatch' :: (Wpref, Mpref)  -> Engaged
  stableMatch' = assert isStable stableMatch 

  mt2 = [(1, [1, 5, 3, 9, 10, 4, 6, 2, 8, 7]),
         (2, [3, 8, 1, 4, 5, 6, 2, 10, 9, 7]),
         (3, [8, 5, 1, 4, 2, 6, 9, 7, 3, 10]),
         (4, [9, 6, 4, 7, 8, 5, 10, 2, 3, 1]), 
         (5, [10, 4, 2, 3, 6, 5, 1, 9, 8, 7]),
         (6, [2, 1, 4, 7, 5, 9, 3, 10, 8, 6]),
         (7, [7, 5, 9, 2, 3, 1, 4, 8, 10, 6]),
         (8, [1, 5, 8, 6, 9, 3, 10, 2, 7, 4]),
         (9, [8, 3, 4, 7, 2, 1, 6, 9, 10, 5]), 
         (10, [1, 6, 10, 7, 5, 2, 4, 3, 9, 8])]

  wt2 =[(1, [2, 6, 10, 7, 9, 1, 4, 5, 3, 8]),
        (2, [2, 1, 3, 6, 7, 4, 9, 5, 10, 8]),
        (3, [6, 2, 5, 7, 8, 3, 9, 1, 4, 10]),
        (4, [6, 10, 3, 1, 9, 8, 7, 4, 2, 5]),
        (5, [10, 8, 6, 4, 1, 7, 3, 5, 9, 2]),
        (6, [2, 1, 5, 9, 10, 4, 6, 7, 3, 8]),
        (7, [10, 7, 8, 6, 2, 1, 3, 5, 4, 9]),
        (8, [7, 10, 2, 1, 9, 4, 8, 5, 3, 6]),
        (9, [9, 3, 8, 7, 6, 2, 1, 5, 10, 4]),
        (10, [5, 8, 7, 1, 2, 10, 3, 9, 6, 4])]

  mt2' :: MprefY
  mt2' = [[1, 5, 3, 9, 10, 4, 6, 2, 8, 7],
         [3, 8, 1, 4, 5, 6, 2, 10, 9, 7],
         [8, 5, 1, 4, 2, 6, 9, 7, 3, 10],
         [9, 6, 4, 7, 8, 5, 10, 2, 3, 1], 
         [10, 4, 2, 3, 6, 5, 1, 9, 8, 7],
         [2, 1, 4, 7, 5, 9, 3, 10, 8, 6],
         [7, 5, 9, 2, 3, 1, 4, 8, 10, 6],
         [1, 5, 8, 6, 9, 3, 10, 2, 7, 4],
         [8, 3, 4, 7, 2, 1, 6, 9, 10, 5], 
         [1, 6, 10, 7, 5, 2, 4, 3, 9, 8]]
  wt2' :: WprefY
  wt2' = [[2, 6, 10, 7, 9, 1, 4, 5, 3, 8],
        [2, 1, 3, 6, 7, 4, 9, 5, 10, 8],
        [6, 2, 5, 7, 8, 3, 9, 1, 4, 10],
        [6, 10, 3, 1, 9, 8, 7, 4, 2, 5],
        [10, 8, 6, 4, 1, 7, 3, 5, 9, 2],
        [2, 1, 5, 9, 10, 4, 6, 7, 3, 8],
        [10, 7, 8, 6, 2, 1, 3, 5, 4, 9],
        [7, 10, 2, 1, 9, 4, 8, 5, 3, 6],
        [9, 3, 8, 7, 6, 2, 1, 5, 10, 4],
        [5, 8, 7, 1, 2, 10, 3, 9, 6, 4]]

  makeMatch3 = stableMatch' (mt2,wt2)

-- 
----------------------------------------------------
  -- some men and woman with their names as strings
  type Name = String 
  get_name :: String -> Int -> [String]
  get_name gender n = map (\x -> gender ++ (show x)) [1..n]

  defs w m = concat (map (\x -> map (\y -> DEFINE ((x ++""++ y) , bool) Nothing) m) w)



  -- define a list of variables
  
  var_list_w:: [Name] -> [Name] -> [[ExpY]]
  var_list_w w m =  (map (\x -> map (\y -> VarE (x ++ "" ++ y)) m) w)
  -- reuse of the defined values
  var_list_m :: [[ExpY]] -> [[ExpY]]
  var_list_m vl_w = 
    let n = length vl_w in 
    let list = map fromIntegral [1..n] in 
    let f ll  index = map (\l -> l !! (index-1)) ll in  
    map (f vl_w) list

  -- all variables 
  var_all_list vl_w = (concat vl_w)
  -- a list of VarE to be used later for adding constrants

-- test 1 : display a list of variables 
  test1 = 
    let n = 3 in 
    let m = get_name "m" n in 
    let w = get_name "w" n in 
    let vl_w = var_list_w w m in 
    let vl_m = var_list_m vl_w in 
    -- var_all_list vl_w vl_m 
    --vl_m
    defs w m


----start encoidng --------

  differ x l = map (\y -> ASSERT (NOT (AND [x,y]))) l
  -- 

  ctr_unique [] = []
  ctr_unique [x] = []
  ctr_unique l = 
    (differ (head l) (tail l)) ++ (ctr_unique (tail l))

  -- each agent is uniquely matched to another agent of opposite gender
  unique_engate var_list_w var_list_m = concat (map ctr_unique var_list_w) ++ concat (map ctr_unique var_list_m)

  --every agent has to get engaged
  must_engate var_list_w var_list_m = (map (\l -> ASSERT (OR l)) var_list_w) ++ (map (\l -> ASSERT (OR l)) var_list_m) 


  test2 = 
    let n = 3 in 
    let m = get_name "m" n in 
    let w = get_name "w" n in 
    let vl_w = var_list_w w m in 
    let vl_m = var_list_m vl_w in 
    let vall =  var_all_list vl_w in 
    --unique_engate vl_w vl_m 
    must_engate vl_w vl_m



  get_weight :: Int -> Integer
  get_weight 0 = 2
  get_weight x = (get_weight (x-1)) +1
  --get_weight 1 = 5
  --get_weight 2 = 7
  --get_weight 3 = 8
  --get_weight x = 2 * (get_weight (x-1)) - (get_weight (x-2)) - 1

  get_index :: Int -> Int -> MprefY -> Int
  get_index row e ll =
    let n = length ll in 
    let (Just i) = elemIndex (toInteger e) (ll !! (row -1) ) in
    n - i

  --encode_max :: [[ExpY]] -> MprefY -> [[ExpY]] -> MprefY -> [CmdY]
  encode_max vl_w pl_w vl_m pl_m = 
    let n = length vl_w in 
    let people = [(woman, man)| woman <- [1..n], man <- [1..n]] in 
    let weight' woman man pl_w pl_m = ((get_weight (get_index woman man pl_w)) + (get_weight (get_index man woman pl_m))) in 
    let get_assert acc (woman, man) = [ASSERT_P ((vl_w !! (woman-1) ) !! (man-1) )  (Just (weight' woman man pl_w pl_m)) ] ++ acc in 
    foldl get_assert [] people


  test3 = 
    let n = 3 in 
    let m = get_name "m" n in 
    let w = get_name "w" n in 
    let vl_w = var_list_w w m in 
    let vl_m = var_list_m vl_w in 
    let vall =  var_all_list vl_w in 
    --unique_engate vl_w vl_m 
    --must_engate vl_w vl_m
    encode_max vl_w wt' vl_m mt'
    --vl_w
    --get_index 1 2 mt' 

  ---------------------------------------------------------------------------
  -- a value retrieval function for decoding (to be used later)

  -- actually there is a simpler implementation called lookup? TODO
  obtain_bvalue x [] = Nothing
  obtain_bvalue x l = 
    let h = head l in 
    let ((VarE vname) := (LitB b)) = h in 
    let VarE xname = x in 
    if xname == vname then 
      Just b 
      else 
        (obtain_bvalue x (tail l))

  obtain_bool_value vars valuelist =
    map (\x -> obtain_bvalue  x valuelist) vars


-- get engatement 
  get_eng :: [Int] -> [Int] -> [Maybe Bool] -> [(Int, Int)]
  get_eng women men lst = do
     let n = length men
     l<- [ (w, m) | w <- women, m <- men , ((lst !!((w -1) * n + (m-1))) == (Just True))]
     return l 

  convert_integer_int_pair_list :: [(Integer,Integer)] -> [(Int, Int)]
  convert_integer_int_pair_list l = 
     let l' = [((fromInteger x),(fromInteger y))| (x, y) <- l] in 
     l'


-- the following is a function to obtain stable engagem for women's preference list(wpl) and men's (mpl).
  obtain_eng :: WprefY -> WprefY -> IO Engaged
  obtain_eng wpl mpl =
    do yp@(Just hin, Just hout, Nothing, p) <- createYicesPipe yicesPath []
       let n = length wpl
       let m = get_name "m" n
       let w = get_name "w" n
       let vl_w = var_list_w w m
       let vl_m = var_list_m vl_w
       let all_var = var_all_list vl_w
       --obtain a list of men and women
       let men = [1..n]
       let women = [1..n]

       let unique_eg = unique_engate vl_w vl_m 
       print "----unique engage ----"
       print unique_eg
       print "----must_engate------"
       let must_eg = must_engate vl_w vl_m 
       print must_eg
       print "-----test sat -------"
       runCmdsY yp ((defs w m) ++ unique_eg ++ must_eg)
       -- hard clauses must be satisfiable
       --Sat ss <- checkY yp
       --return ss
       let mymax = (encode_max vl_w wpl vl_m mpl)
       print "--------maxsat----------------"
       print mymax
       runCmdsY yp (mymax  ++ [MAXSAT])
       print "-----------result<sat or unknown, can't be unsat>-------------"
       Sat ss' <- checkMAX yp
       print ss' 
       let model = obtain_bool_value all_var ss'
       print "-------and model is ----------------"
       print model
       --return model 
       let eng = get_eng women men model
       return eng

-- the sum of the weight from both men and women
  evaluate_eng wpl mpl eng =
    foldl (\acc (w,m) ->  acc + (get_weight (get_index w m wpl) + get_weight (get_index m w mpl)) ) 0 eng  

-- a test for stable matching
  test_stable_maxsat = do
    eng <- obtain_eng wt' mt'
    print eng
    print (evaluate_eng wt' mt' eng)
    print "--------and the correct one is ------"
    print (evaluate_eng wt' mt' makeMatch)
    return $ isStable (wt, mt) eng
    -- MG: Please explain the output more.

-- you will get the following error :
-- "unknown"
-- *** Exception: user error (Pattern match failure in do expression at 5.hs:348:8-14)
  test_stable_maxsat2 = do
    eng <- obtain_eng wt2' mt2'
    print eng
    print (evaluate_eng wt2' mt2' eng)
    print "--------and the correct one is ------"
    print makeMatch3
    print (evaluate_eng wt2' mt2' makeMatch3)
    return $ isStable (wt2, mt2) eng

--- It has been shown that (this version of) Yices is not good enough for MaxSAT.
-- MG: This takes very long and it seems strange that just increasing
--     the number of people should break things. I guess the pseudo-
--     binding to Yices can not cope with it.

-- roommate problem
-- Assume we only have 4 men (this is kind of sad):

  rpl :: MprefY
  rpl = [[2,3,4], [1,3,4], [2,1,4], [3,2,1]]
  -- we introduce a list of men (names)

  row = get_name "r" 4
  col = get_name "c" 4
-- introduce a matrix of 4 by 4
  def_matrix  = defs row col

  val_matrix = 
    var_list_w row col
    --[[r1c1,r1c2,r1c3,r1c4],
    --[r2c1,r2c2,r2c3,r2c4],
    --[r3c1,r3c2,r3c3,r3c4],
    --[r4c1,r4c2,r4c3,r4c4]]

  room_max = [ASSERT_P ((val_matrix!! (i-1)) !! (j-1)) (Just (get_weight (get_index i j rpl))) | i <- [1..4], j <- [1..4], i /= j]

  -- note that we are going to set r_i c_i false since you can't live by yourself

-- the following is a hardcoded simple testing function for the stable roommate matching problem
  test_roommate :: IO Engaged
  test_roommate = 
    do yp@(Just hin, Just hout, Nothing, p) <- createYicesPipe yicesPath []
       let n = 4
       let unique_eg =  (concat (map ctr_unique val_matrix))
       putStrLn "----unique engage ----"
       print unique_eg
       putStrLn "----must_engate------"
       let must_eg = (map (\l -> ASSERT (OR l)) val_matrix)
       print must_eg
       let diagnal = [ASSERT (NOT ((val_matrix !! (i-1)) !!(i-1))) | i <- [1..4]]
       putStrLn "-----diagnal ----------"
       print diagnal
       putStrLn "-----mirror ----------"
       let mirror = [ ASSERT (((val_matrix !! (i-1)) !!(j-1)):=((val_matrix !! (j-1)) !!(i-1))) | i <- [1..4], j <- [1..4], i < j ]
       print mirror 
       putStrLn "-----test sat -------"
       runCmdsY yp ((def_matrix) ++ unique_eg ++ must_eg ++ diagnal ++ mirror)
       -- hard clauses must be satisfiable
       Sat ss <- checkY yp
       return ss

       --let mymax = (encode_max vl_w wpl vl_m mpl)
       putStrLn "--------maxsat----------------"
       print room_max
       runCmdsY yp (room_max  ++ [MAXSAT])
       putStrLn "-----------result<sat or unknown, can't be unsat>-------------"
       Sat ss' <- checkMAX yp
       print ss' 
       let model = obtain_bool_value (concat val_matrix) ss'
       putStrLn "-------and model is ----------------"
       print model
       --return model 
       let eng = get_eng [1..4] [1..4] model
       return eng


-- The result is: [(1,2),(2,1),(3,4),(4,3)]

--
-- Please take a look at the FSA_report_lab5.pdf for further explanations and ex4
--

-- MG: Very nice report, though also many typos. I understand you
--     did not have much time. For the future, you could also look
--     into this https://github.com/pepeiborra/bindings-yices which
--     is a proper binding and not using pipes.






















