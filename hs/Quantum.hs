module Quantum where

import Data.List
import Data.Map
import Data.Complex
import Data.IORef
import System.Random


-- 2.1
data Move = Vertical | Horizontal deriving (Show,Eq,Ord)
data Rotation = CtrClockwise | Clockwise deriving (Show,Eq)
data Color = Red | Yellow | Blue deriving (Show,Eq)

data Qop a b = Qop (Map (a,b) PA)
data QR a = QR (IORef (QV a))

-- Basis will also be an instance of Eq and Ord
class (Eq a, Ord a) => Basis a where
     basis :: [a] -- :: == has type

instance Basis Bool where
  basis = [False, True]

instance Basis Move where
  basis = [Vertical,Horizontal]

instance (Basis a,Basis b) => Basis (a,b)
   where basis = [(a,b) | a <- basis, b <- basis] --comprehension

type PA = Complex Double --prob amp type
type QV a = Map a PA --quantum values as (a, PA)

qv :: (Basis a) => [(a,PA)] -> QV a 
qv = fromList --expects a list of QV

qFT :: QV Bool
qFT = qv [(False,1/sqrt(2)),(True,1/sqrt(2))] -- |+>

pr :: (Basis a) => QV a -> a -> PA
pr q b = findWithDefault 0 b q -- return PA, if ! return 0
-- use ex: pr qFT False

-- examples:
qFalse, qTrue :: QV Bool
qFalse = Data.Map.singleton False 1 -- |0>
qTrue = Data.Map.singleton True 1 -- |1>

-- 2.3
p1,p2,p3 :: QV (Bool,Bool)
p1 = qv [((False,False),1),((False,True),1)] -- |00> + |01>
p2 = qv [((False,False),1),((True,True),1)] -- |00> + |11>
p3 = qv [((False,False),1),
         ((False,True),1),
         ((True,False),1),
         ((True,True),1)] -- |00> + |01> + |10> + |11>
----

-- Produto Tensorial
(&*) :: (Basis a, Basis b) => QV a -> QV b -> QV (a,b)
qa &* qb = qv[((a,b), pr qa a * pr qb b)|a <- basis, b <- basis]         


-- 3
-- Operações quânticas como funções
qnot_f :: QV Bool -> QV Bool
qnot_f v = qv [(False, pr v True),
               (True, pr v False)] -- why return a PA = 0 element??


hadamard_f :: QV Bool -> QV Bool
hadamard_f v = let p = pr v False
                   q = pr v True
               in qv[(False,p+q),(True,p-q)]    

-- Matrix operations
qop :: (Basis a, Basis b) => [((a,b), PA)] -> Qop a b
qop = Qop . fromList -- (.) == function composition

qApp :: (Basis a, Basis b) => Qop a b -> QV a -> QV b
qApp (Qop m) v = 
  let bF b  = sum [ pr m (a,b) * pr v a | a <- basis]
  in qv [(b, bF b) | b <- basis]

qnot_op = 
  qop [
    ((False,True),1),
    ((True,False),1)
  ]

hadamard_op = 
  qop [
    ((False,False),1),
    ((False,True),1),
    ((True,False),1),
    ((True,True),-1)
  ]

-- 3.2
cop :: (Basis a , Basis b) => (a -> Bool) -> Qop b b -> Qop (a,b) (a,b)
cop enable (Qop u) = 
  qop(
    [(((a,b),(a,b)),1) | (a,b) <- basis, not (enable a)] ++
    [(((a,b1),(a,b2)), pr u (b1, b2)) 
     |a <- basis, enable a, b1 <- basis, b2 <- basis]
  ) -- (++) list append


cnot :: Qop (Bool,Bool) (Bool,Bool)
cnot = cop id qnot_op -- how to use??

toffoli :: Qop ((Bool,Bool), Bool) ((Bool,Bool), Bool)
toffoli = cop (uncurry (&&)) qnot_op

-- 4.1
(*>>) :: Basis a => PA -> QV a -> QV a -- (*>) complained: already defined
c *>> v = Data.Map.map (\a -> c * a) v

normalize :: Basis a => QV a -> QV a
normalize v = (1 / norm v :+ 0) *>> v

norm :: Basis a => QV a -> Double
norm v = 
  let probs = [((\a -> a * a) . magnitude) x | x <- elems v] -- works with a comprehension
  in sqrt (sum probs)

--4.5
mkQR :: QV a -> IO (QR a)
mkQR v = do
  r <- newIORef v
  return (QR r)

observeR :: Basis a => QR a -> IO a
observeR (QR ptr) = do
  v <- readIORef ptr
  res <- observeV v
  writeIORef ptr (Data.Map.singleton res 1)
  return res

observeV ::  Basis a => QV a -> IO a
observeV v = do
  let nv = normalize v
      probs = [(((\x -> x * x) . magnitude) . pr nv) basis | basis <- keys v]
  r <- getStdRandom (randomR (0.0,1.0))
  let cPsCs = zip (scanl1 (+) probs) basis
      Just (_,res) = find (\(p,_) -> r < p) cPsCs
  print probs
  return res

test = do
  x <- mkQR qFT
  o1 <- observeR x
  o2 <- observeR x
  o3 <- observeR x
  print (o1,o2,o3) 


-- {-
observeLeft :: (Basis a, Basis b) => QR (a,b) -> IO a
observeLeft (QR ptr) = do
  v <- readIORef ptr
  let leftF a = sqrt (sum [((\x -> x * x) . magnitude) pr v (a,b) | b <- basis])
      leftV = qv [ (a, leftF a) | a <- basis]
  aobs <- observeV leftV
  let nv = qv [((aobs,b), pr v (aobs,b)) | b <- basis]
  writeIORef ptr (normalize nv)
  return aobs 
-- -}

-- 5

