--Please go to the folder lab5. this is only a backup of the exerise itself

  module FSAlab5
  where 
  import Data.List

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


--obtain a list of men and women
  men = map fst mt
  women = map fst wt

  

  type PrefFct = Int -> Int -> Int -> Bool
  
  plist2pfct :: [(Int,[Int])] -> PrefFct 
  plist2pfct table x y y' = 
    let 
      Just prefs = lookup x table
    in elem y (takeWhile (/= y') prefs)
    -- takeWhile is to get before y
    -- *FSAlab5> takeWhile (/= 3) [1,2,3,4,5,6]
    -- [1,2]


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

  makeMatch3 = stableMatch' (mt2,wt2)

