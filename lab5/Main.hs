-- vim:sw=2:ts=2:expandtab:autoindent

module Main where 

-- example of generating three test cases of 6 integer variables
-- l1, u1, i, l2, u2, j sush that l1 < i <= u1 and l2 < j <= u2.

import Math.SMT.Yices.Parser
import Math.SMT.Yices.Syntax
import Math.SMT.Yices.Pipe
import Data.List
import Control.Monad
-- import Random

yicesPath = "./yices-1.0.40/bin/yices" -- copy yices here

--main_test =
--  do yp@(Just hin, Just hout, Nothing, p) <- createYicesPipe yicesPath []
--     runCmdsY yp (defs ++ ctrs)

--     -- gr <- getStdGen
--     -- let (rn,gr') = next gr

--     Sat ss <- checkY yp

--     print (head ss)

--     runCmdsY yp [ASSERT_P (NOT $ ss!!0) Nothing]
--     Sat ss <- checkY yp
--     print ss

--     runCmdsY yp [ASSERT_P (NOT $ ss!!0) Nothing]
--     Sat ss <- checkY yp
--     print ss

--     exitY yp
--     --return ss


--defs = map (\x -> DEFINE (x,int) Nothing) ["l1","u1","i","l2","u2","j"]

--ctrs = map ASSERT [ l1:<u1, l1:<=i, i:<=u1, l2:<u2, l2:<=j, j:<=u2 ]
--     -- ++ map (\e -> ASSERT_P e Nothing) [ i:<j, j:<i ]
--     where
--       l1 = VarE "l1"
--       u1 = VarE "u1"
--       i = VarE "i"
--       l2 = VarE "l2"
--       u2 = VarE "u2"
--       j = VarE "j"

int = VarT "int"
nat = VarT "nat"
bool = VarT "bool"
real = VarT "real"

true = LitB True
false = LitB False


--test =
--  do yp@(Just hin, Just hout, Nothing, p) <- createYicesPipe yicesPath []
--     runCmdsY yp (defs' ++ ctrs' )

--     -- gr <- getStdGen
--     -- let (rn,gr') = next gr

--     Sat ss <- checkY yp

--     runCmdsY yp (mymax ++ [MAXSAT])
--     Sat ss <- checkY yp

--     return (obtain_bool_value vlist' ss)



--defs' = map (\x -> DEFINE (x,bool) Nothing) ["l1","l2","l3","l4","l5","l6"]

--vlist' = map (\x -> VarE x) ["l1","l2","l3"]
--l1 = VarE "l1"
--l2 = VarE "l2"
--l3 = VarE "l3"

--ctrs' = map ASSERT [ OR vlist']
--      ++ map (\e -> ASSERT_P e Nothing) [l1:=false]
--      ++ [ASSERT (FORALL [("l1",bool), ("l2", bool)] (OR[AND[l1, l2], l3]))]

----ASSERT_P ExpY (Maybe Integer) : 
--mymax = [ASSERT_P (l2:=true) (Just 8)]

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




mt = [(1,[2,1,3]), (2, [3,2,1]), (3,[1,3,2])]
wt = [(1,[1,2,3]),(2,[3,2,1]), (3,[1,3,2])]

get_name i n = map (\x -> i ++ (show x)) [1..n]

n = 3
-- string of integer basically :)
m = get_name "m" n
w = get_name "w" n

men = map fst mt
women = map fst wt

--define a list of n * n variables
defs :: [CmdY]
defs  = concat (map (\x -> map (\y -> DEFINE ((x ++""++ y) , bool) Nothing) w) m)

var_list_m::[[ExpY]]
var_list_m =  (map (\x -> map (\y -> VarE (x ++ "" ++ y)) w) m)
--var_list_m = (map (\x -> map (\y -> VarE (x ++ " " ++ y)) w) m)
var_list_w = 
  let list = map  fromIntegral [1..n] in 
  let f ll  index = map (\l -> l !! (index-1)) ll in  
  map (f var_list_m) list


var_all_list = (concat (var_list_w ++ var_list_m))
-- a list of VarE to be used later for adding constrants


-- x can not be true at the same time with any elem of l
differ x l = map (\y -> ASSERT (NOT (AND [x,y]))) l
-- 

ctr_unique [] = []
ctr_unique [x] = []
ctr_unique l = 
  (differ (head l) (tail l)) ++ (ctr_unique (tail l))

unique_engate = concat (map ctr_unique var_list_m) ++ concat (map ctr_unique var_list_w)

must_engate = (map (\l -> ASSERT (OR l)) var_list_m) ++ (map (\l -> ASSERT (OR l)) var_list_w) 

-- the maxsat part

--mymax = [ASSERT_P (l2:=true) (Just 8)]

-- for men



encode_max :: [[ExpY]] -> [(Integer, [Integer])] -> [[CmdY]]
encode_max varlist preflist = 
  let f vl who (p, w) = ASSERT_P ((vl!!(who - 1)) !! (p-1)) (Just w) in
  let wei = reverse (map fromIntegral [1..n]) in  
  let encode vl (who, plist) = (map (f vl (fromIntegral who)) (zip (map fromIntegral plist) wei)) in 
  map (encode varlist) preflist



mymax = concat ((encode_max var_list_m mt) ++ (encode_max var_list_w wt))


test1 =
  do yp@(Just hin, Just hout, Nothing, p) <- createYicesPipe yicesPath []
     runCmdsY yp ((defs) ++ unique_engate ++ must_engate)
     Sat ss <- checkY yp
     --return ss
     print "-------unique ------"
     print unique_engate
     print "-------must --------"
     print must_engate
     print "--------max ----------"
     print mymax
     runCmdsY yp (mymax  ++ [MAXSAT])
     print "----------result--------------"
     Sat ss' <- checkMAX yp
     print ss'
     print "----------model--------------"
     return (obtain_bool_value var_all_list ss')

--eng = [ (w, m) | w <- [1,2,3], m <-[1..5], m > 2]

--eng = [(w, m) | w <- [1..3], m <- [1..4]]
--eng = [ (w, m) | w <- women, m <- men , ((test1!!(w*n + m)) == (Just True))]
infix 1 ==> 
  
(==>) :: Bool -> Bool -> Bool
p ==> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

get_eng lst = do
   l <- test1
   return [ (w, m) | w <- women, m <- men , ((l!!((fromIntegral w -1) * n + (fromIntegral m-1))) == (Just True))]

eng = get_eng test1

isStable (wpref, mpref) engaged = let 
    wf = plist2pfct wpref
    mf = plist2pfct mpref
  in 
    forall engaged (\ (w,m) -> forall engaged 
          (\ (w',m') -> (wf w m' m ==> mf m' w' w)
                         && 
                        (mf m w' w ==> wf w' m' m)))
plist2pfct table x y y' = 
  let 
    Just prefs = lookup x table
  in elem y (takeWhile (/= y') prefs)

result engage = do
  let x = 1
  eng' <- engage
  return (isStable (wt, mt) eng')






 
