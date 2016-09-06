module TOTP

where 

import Data.List

type Name     = String
type Index    = [Int]
data Id       = Id Name Index deriving (Eq,Ord)

instance Show Id where 
  show (Id name [])  = name
  show (Id name [i]) = name ++ ('_': show i)
  show (Id name is)  = name ++ ('_': showInts is)
     where showInts [] = ""  
           showInts (i:is) = show i ++ showInts is

ix = Id "x" []
iy = Id "y" []
iz = Id "z" []

data Term     = Var Id | Struct Id [Term] deriving Eq

instance Ord Term where 
  compare (Var x) (Var y) = compare x y 
  compare (Var x) _       = LT
  compare _       (Var y) = GT 
  compare (Struct a ts) (Struct b rs) 
    | a == b = compare ts rs
    | a < b  = LT
    | a > b  = GT

x    = Var ix
y    = Var iy
z    = Var iz

a     = Struct (Id "a" []) []
b     = Struct (Id "b" []) []
c     = Struct (Id "c" []) []
zero  = Struct (Id "z" []) []
s     = Struct (Id "s" [])
t     = Struct (Id "t" [])
u     = Struct (Id "u" [])
one   = s[zero]
two   = s[one]
three = s[two]
four  = s[three]
five  = s[four]

instance Show Term where 
  show (Var id)         = show id 
  show (Struct id [])   = show id 
  show (Struct id ts)   = show id ++ concat [ show ts ]

isVar :: Term -> Bool
isVar (Var _) = True
isVar _       = False

varsInTerm :: Term -> [Id]
varsInTerm (Var i)       = [i]
varsInTerm (Struct i ts) = varsInTerms ts

varsInTerms :: [Term] -> [Id]
varsInTerms = nub . concat . map varsInTerm

data Frm =  Atom Id [Term]
         | Eq Term Term
         | N Frm
         | I Frm Frm
         | E Frm Frm 
         | C [Frm]
         | D [Frm] 
         | Forall Id Frm 
         | Exists Id Frm
     deriving (Eq,Ord)

instance Show Frm where 
  show (Atom id []) = show id 
  show (Atom id ts) = show id ++ concat [ show ts ]
  show (Eq t1 t2)   = show t1 ++ "==" ++ show t2
  show (N form)   = '-': (show form)
  show (I f1 f2) = "(" ++ show f1 ++ "==>" ++ show f2 ++ ")"
  show (E f1 f2) = "(" ++ show f1 ++ "<=>" ++ show f2 ++ ")"
  show (C fs)     = "*(" ++ showLst fs ++ ")"
  show (D fs)     = "+(" ++ showLst fs ++ ")"
  show (Forall id f) = "!" ++  show id ++ (' ' : show f)
  show (Exists id f) = "?" ++  show id ++ (' ' : show f)

showLst,showRest :: [Frm] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ',': show f ++ showRest fs

type Subst = [(Id,Term)]

epsilon :: Subst
epsilon = []

dom :: Subst -> [Id]
dom = map fst

rng :: Subst -> [Term]
rng = map snd 

restriction :: [Id] -> Subst -> Subst
restriction ids = filter (\ (x,_) -> elem x ids)

appI :: Subst -> Id -> Term
appI []          y             = (Var y)
appI ((x,x'):xs) y | x == y    = x'
                   | otherwise = appI xs y 

appT :: Subst -> Term -> Term
appT b (Var y)       = appI b y
appT b (Struct n ts) = Struct n (appTs b ts) 

appTs :: Subst -> [Term] -> [Term]
appTs = map . appT

appF :: Subst -> Frm -> Frm 
appF b (Atom a ts)  = Atom a (appTs b ts)
appF b (N f)      = N (appF b f)
appF b (I f1 f2) = I (appF b f1) (appF b f2) 
appF b (E f1 f2) = E (appF b f1) (appF b f2) 
appF b (C fs)     = C (appFs b fs)
appF b (D fs)     = D (appFs b fs)
appF b (Forall v f) = Forall v (appF b' f) 
                      where b' = filter (\ (x,_) -> x /= v) b
appF b (Exists v f) = Exists v (appF b' f) 
                      where b' = filter (\ (x,_) -> x /= v) b

appFs :: Subst -> [Frm] -> [Frm]
appFs b = map (appF b)

cleanUp :: Subst -> Subst
cleanUp = filter (\ (x,x') -> x' /= (Var x))

compose :: Subst -> Subst -> Subst 
compose xs ys = 
  (cleanUp [ (y,(appT xs y')) | (y,y') <-  ys ])
   ++
  (filter (\ (x,x') -> x `notElem` (dom ys)) xs)

demo1 = compose [(ix,y)] [(iy,x)]
demo2 = compose [(iy,x)] [(ix,y)]
demo3 = compose [(iy,z)] [(ix,y)]

isRenaming :: Subst -> Bool
isRenaming s = all isVar rngS && (nub rngS) == rngS
  where rngS = rng s

unifyTs :: Term -> Term -> [Subst]
unifyTs (Var x)       (Var y)       =
        if x==y then [epsilon] else [[(x,Var y)]]
unifyTs (Var x)       t2            = 
        [ [(x,t2)] | x `notElem` varsInTerm t2 ]
unifyTs t1            (Var y)       = 
        [ [(y,t1)] | y `notElem` varsInTerm t1 ]
unifyTs (Struct a ts) (Struct b rs) = 
        [ u | a==b, u <- unifyTlists ts rs ]

unifyTlists :: [Term] -> [Term] -> [Subst]
unifyTlists []     []     = [epsilon]
unifyTlists []     (r:rs) = []
unifyTlists (t:ts) []     = []
unifyTlists (t:ts) (r:rs) = 
 [ compose s2 s1 | s1 <- unifyTs t r,
                   s2 <- unifyTlists (appTs s1 ts) 
                                     (appTs s1 rs) ]

unif :: Term -> Term -> Subst
unif tm1 tm2 = case unifyTs tm1 tm2 of 
   []  -> error "terms do not unify"
   [s] -> s

skolem ::  Int -> [Id] -> Term
skolem k is = Struct (Id "sk" [k]) [ (Var x) | x <- is ]

sk :: Frm -> Frm
sk f = fst (skf f [] True 0) 

skf :: Frm -> [Id] -> Bool -> Int -> (Frm,Int)
skf (Atom n ts) ixs pol k = ((Atom n ts),k)
skf (C fs) ixs pol k = ((C fs'),j) 
      where (fs',j) = skfs fs ixs pol k 
skf (D fs) ixs pol k = ((D fs'),j) 
      where (fs',j) = skfs fs ixs pol k 
skf (Forall x f) ixs True k = ((Forall x f'),j) 
     where (f',j) = skf f ixs' True k 
           ixs'   = insert x ixs
skf (Forall x f) ixs False k = skf (appF b f) ixs False (k+1)
     where b = [(x,(skolem k ixs))]
skf (Exists x f) ixs True k = skf (appF b f) ixs True (k+1) 
     where b = [(x,(skolem k ixs))]
skf (Exists x f) ixs False k = ((Exists x f'),j) 
     where (f',j) = skf f ixs' False k 
           ixs'   = insert x ixs
skf (N f) ixs pol k = ((N f'),j)
     where (f',j) = skf f ixs (not pol) k 

skfs :: [Frm] -> [Id] -> Bool -> Int -> ([Frm],Int)
skfs []     _   _   k  = ([],k)
skfs (f:fs) ixs pol k  = ((f':fs'),j) 
   where 
   (f', j1) = skf  f  ixs pol k
   (fs',j)  = skfs fs ixs pol j1 

p,q,r :: [Term] -> Frm
p      = Atom (Id "p" [])
q      = Atom (Id "q" [])
r      = Atom (Id "r" [])

refl   = Forall ix (r [x,x])
irrefl = Forall ix (N (r [x,x]))
corefl = Forall ix (Forall iy (I (r [x,y]) (Eq x y)))
trans  = Forall ix (Forall iy (Forall iz 
         (D [N (r [x,y]),N (r [y,z]),r [x,z]])))
ctrans = Forall ix (Forall iy (Forall iz 
         (D [r [x,y], r [y,z],N (r [x,z])])))
symm   = Forall ix (Forall iy 
         (D [N  (r [x,y]), r [y,x]]))
antisymm = Forall ix (Forall iy 
         (I (C [r [x,y], r [y,x]]) (Eq x y)))
asymm  = Forall ix (Forall iy 
         (D [N (r [x,y]), N (r [y,x])]))
serial = Forall ix (Exists iy (r [x,y]))
serial1 = Forall ix (Forall iy (Exists iz (r [x,y,z])))
serial2 = Forall ix (Exists iy (Exists iz (r [x,y,z])))
trichotomous = Forall ix (Forall iy (D [r[x,y],r[y,x],Eq x y]))
euclidean = Forall ix (Forall iy (Forall iz 
  (I (C [r[x,y],r[x,z]]) (r[y,z]))))

relprop1 = D [N asymm,irrefl]
relprop2 = D [N trans,N irrefl,asymm]
relprop3 = D [N trans,N symm,N serial,refl]

prune :: Frm -> Frm 
prune f@(Atom _ _) =  f
prune (N f) = N (prune f) 
prune (C fs) = C (map prune fs) 
prune (D fs) = D (map prune fs) 
prune (Forall _ f) = prune f
prune (Exists _ f) = prune f

data Cl = Cl [Term] [Term] deriving (Eq,Ord,Show)

appCl :: Subst -> Cl -> Cl 
appCl s (Cl neg pos) = Cl (appTs s neg) (appTs s pos)

appCls :: Subst -> [Cl] -> [Cl]
appCls b = map (appCl b)

varsInClause :: Cl -> [Id]
varsInClause (Cl neg pos) = 
  nub (varsInTerms neg ++ varsInTerms pos)

nnf :: Frm -> Frm 
nnf f@(Atom _ _) = f 
nnf f@(N (Atom _ _)) = f 
nnf (N (N f)) = nnf f
nnf (C fs) = C (map nnf fs)
nnf (D fs) = D (map nnf fs)
nnf (N (C fs)) = D (map (nnf.N) fs)
nnf (N (D fs)) = C (map (nnf.N) fs)

cnf :: Frm -> Frm 
cnf f@(Atom _ _) = f 
cnf f@(N (Atom _ _)) = f 
cnf (C fs) = C (map cnf fs)
cnf (D []) = D []
cnf (D [f]) = cnf f 
cnf (D (f:fs)) = dist (cnf f) (cnf (D fs))

dist :: Frm -> Frm -> Frm 
dist (C []) _ = C []
dist (C [f1]) f2 = dist f1 f2
dist (C (f1:fs)) f2 = C [dist f1 f2, dist (C fs) f2]
dist _ (C []) = C []
dist f1 (C [f2]) = dist f1 f2
dist f1 (C (f2:fs)) = C [dist f1 f2, dist f1 (C fs)]
dist f1 f2 = D [f1,f2]

flat :: Frm -> Frm
flat (C fs) = C (flatC fs)
flat (D fs) = D (flatD fs)
flat f        = f

flatC :: [Frm] -> [Frm] 
flatC [] = []
flatC ((C fs):gs) = flatC (fs ++ gs)
flatC (f:fs) = flat f : flatC fs

flatD :: [Frm] -> [Frm]
flatD [] = []
flatD ((D fs):gs) = flatD (fs ++ gs)
flatD (f:fs) = f: flatD fs

nubF :: Frm -> Frm
nubF (C fs) = C (map nubF fs)
nubF (D fs) = D (nub (sort fs))
nubF f      = f 

dsj2cl :: Frm -> Cl
dsj2cl (D lits) = 
   Cl [ (Struct n ts) | (N (Atom n ts)) <- lits ] 
      [ (Struct n ts) | at@(Atom n ts)  <- lits ] 
dsj2cl lit = dsj2cl (D [lit])

cnf2cls :: Frm -> [Cl]
cnf2cls (C fs) = map dsj2cl fs
cnf2cls f = cnf2cls (C [f])

nonTriv :: Cl -> Bool
nonTriv (Cl neg pos) = null (intersect neg pos) 

cls :: Frm -> [Cl] 
cls = nub . filter nonTriv . 
        cnf2cls . nubF . flat . cnf . nnf . prune . sk

infix  6 :-
data Dclause =  Term :- [Term] deriving Show

type Goal   =  [Term]

father, fatherOf ::  [Term] -> Term
father = Struct (Id "father" [])
fatherOf = Struct (Id "father_of" [])

fatherC :: Dclause
fatherC = father [Var (Id "X" [])] 
          :- [fatherOf [Var (Id "X" []),Var (Id "_" [])]]

nil :: Term
nil  = Struct (Id "nil" []) [] 

cons :: [Term] -> Term
cons = Struct (Id "cons" [])

type Definition = (Name,[Dclause]) 
data Database   = Db [Definition] deriving Show

dclausesFor           :: Name -> Database -> [Dclause]
dclausesFor a (Db defs) = 
  case dropWhile (\ (n,def) -> n<a) defs of
  []          -> []
  ((n,def):_) -> if a==n then def else []

renameVars :: Int -> Term -> Term
renameVars level (Var (Id s n)) = Var (Id s [level])
renameVars level (Struct s ts)  = 
  Struct s (map (renameVars level) ts)

renDclauses :: Database -> Int -> Term -> [Dclause]
renDclauses db n (Var _)          = []
renDclauses db n (Struct (Id a _) _) = 
             [r tm:-map r tp | (tm:-tp) <- dclausesFor a db]
                                where r = renameVars n

type Alt   = ([Term], Subst) 

alts :: Database -> Int -> Term -> [Alt]
alts db n g = [ (tp,u) | (tm:-tp) <- renDclauses db n g, 
                          u       <- unifyTs g tm         ]

memb :: [Term] -> Term
memb = Struct (Id "member" []) 

member :: Definition 
member = ("member", [memb [x,cons [x,y]] :- [],
                     memb [x,cons [y,z]] :- [memb [x,z]]])

db = Db [member]

goal = memb [x, cons [a, cons [b, cons [c,nil]]]]

resolStep = alts db 0 goal

type Stack = [ (Subst, [Term], [Alt]) ]

prove :: Database -> [Term] -> [Subst]
prove db gl = solve 1 epsilon gl []
  where 
  solve :: Int -> Subst -> [Term] -> Stack -> [Subst]
  solve n s []     ow = s : backtrack n ow
  solve n s (g:gs) ow = choose n s gs (alts db n (appT s g)) ow

  choose :: Int -> Subst -> [Term] -> [Alt] -> Stack -> [Subst]
  choose n s gs []          ow = backtrack n ow
  choose n s gs ((tp,u):rs) ow = 
      solve (n+1) (compose u s) (tp++gs) ((s,gs,rs):ow)

  backtrack :: Int -> Stack -> [Subst]
  backtrack n []               = []
  backtrack n ((s,gs,rs):ow)   = choose (n-1) s gs rs ow

data Node  = Nd Index [Term] [Term] [Frm] deriving Show

type Tableau = [Node]

alpha :: Frm -> Bool
alpha (C _)     = True
alpha (N (D _)) = True
alpha _         = False 

beta :: Frm -> Bool
beta (D _)     = True
beta (N (C _)) = True
beta _         = False

gamma :: Frm -> Bool
gamma (Forall _ _)     = True
gamma (N (Exists _ _)) = True
gamma _                = False

plit, nlit, dneg :: Frm -> Bool
plit (Atom n ts)     = True 
plit _               = False
nlit (N (Atom n ts)) = True 
nlit _               = False 
dneg (N (N f))       = True
dneg _               = False 

f2t :: Frm -> Term
f2t (Atom n ts)       = Struct n ts
f2t (N (Atom n ts)) = Struct n ts

components :: Frm -> [Frm]
components (C fs)          = fs
components (D fs)          = fs 
components (N (C fs))    = map (\ f -> N f) fs
components (N (D fs))    = map (\ f -> N f) fs
components (N (N f))      = [f]
components (Forall x f)       = [f]
components (N (Exists x f)) = [N f]

binder :: Frm -> Id 
binder (Forall x f)     = x 
binder (N (Exists x f)) = x 

decompose :: Frm -> ([Id],Frm)
decompose form = decomp [] form where 
  decomp xs f = if gamma f then decomp (xs ++ [x]) f' 
                           else (xs,f) 
      where x    = binder f
            [f'] = components f

step :: Node  -> Tableau 
step (Nd i pos neg []) = [Nd i pos neg []]
step (Nd i pos neg (f:fs)) 
  | plit  f = if elem (f2t f) neg 
              then [] else [Nd i ((f2t f):pos) neg fs]
  | nlit  f = if elem (f2t f) pos
              then [] else [Nd i pos ((f2t f):neg) fs]
  | dneg  f = [Nd i pos neg ((components f) ++ fs)]
  | alpha f = [Nd i pos neg ((components f) ++ fs)]
  | beta  f = [(Nd (i++[n]) pos neg (f':fs)) |
                          (f',n)   <- zip (components f) [0..] ]
  | gamma f = [Nd i pos neg  (f':(fs++[f]))]
    where 
    (xs,g) = decompose f 
    b      = [((Id x j), Var (Id x i)) | (Id x j) <- xs ]
    f'     = appF b g

stepD :: Int -> Node -> (Int,Tableau)
stepD k node@(Nd i pos neg []) = (k,[Nd i pos neg []])
stepD k (Nd i pos neg (f:fs))
  | plit  f = if elem (f2t f) neg 
              then (k,[]) else (k,[Nd i ((f2t f):pos) neg fs])
  | nlit  f = if elem (f2t f) pos 
              then (k,[]) else (k,[Nd i pos ((f2t f):neg) fs])
  | dneg  f = (k,[Nd i pos neg ((components f) ++ fs)])
  | alpha f = (k,[Nd i pos neg ((components f) ++ fs)])
  | beta  f = (k,[(Nd (i++[n]) pos neg (f':fs)) |
                       (f',n)   <- zip (components f) [0..] ])
  | gamma f = (k-1,[Nd i pos neg  (f':(fs++[f]))])
    where 
    (xs,g) = decompose f 
    b      = [((Id x j), Var (Id x i)) | (Id x j) <- xs ]
    f'     = appF b g

expanded :: Node -> Bool 
expanded (Nd i pos neg []) = True
expanded  _                = False

expand :: Int -> Tableau -> Tableau 
expand 0 tableau = tableau 
expand _ []      = []
expand n (node:nodes) = if expanded node 
                       then (node:(expand n nodes))
                       else if k == n 
                       then expand n     (newnodes ++ nodes)
                       else expand (n-1) (nodes ++ newnodes) 
    where (k,newnodes) = stepD n node

checkN :: Node -> [Subst]
checkN (Nd _ pos neg _) = 
   concat [ unifyTs p n | p <- pos, n <- neg ]

appNd :: Subst -> Node -> Node 
appNd b (Nd i pos neg forms) = 
  Nd i (appTs b pos) (appTs b neg) (appFs b forms)

appTab :: Subst -> Tableau -> Tableau
appTab = map . appNd

checkT :: Tableau -> [Subst]
checkT []           = [epsilon]
checkT [node]       = checkN node
checkT (node:nodes) = 
  concat [ checkT (appTab s nodes) | s <- checkN node ]

initTab :: Frm -> Tableau 
initTab form = [Nd [] [] [] [form]]

refuteDepth :: Int -> Frm -> Bool
refuteDepth k form = checkT tableau /= []
   where tableau = expand k (initTab form)

data Quant = Un Id | Ex Id deriving (Eq,Show)

pf :: Bool -> Frm -> [Quant]
pf _ (Atom _ _) = []
pf pol (N f) = pf (not pol) f 
pf pol (C fs) = concat (map (pf pol) fs)
pf pol (D fs) = concat (map (pf pol) fs)
pf True (Forall i f) = Un i : pf True f 
pf False (Forall i f) = Ex i : pf False f 
pf True (Exists i f) = Ex i : pf True f 
pf False (Exists i f) = Un i : pf False f 

prefix :: [Quant] -> Frm -> Frm 
prefix qs f = prf (reverse qs) f where 
  prf [] f = f 
  prf (Un i:qs) f = prf qs (Forall i f) 
  prf (Ex i:qs) f = prf qs (Exists i f) 

prenex :: Frm -> Frm
prenex f = let 
   p = pf True f
   f' = prune f
 in prefix p f'

thm :: Int -> Frm -> Bool
thm n = (refuteDepth n) . sk . N

sat :: Int -> Frm -> Bool
sat n = not . (refuteDepth n) . sk

formula = D [N trans, N symm, N serial, refl]

