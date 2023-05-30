module Quantum where


import Data.List
import Data.Map
import Data.Complex

--data Bool = False | True

data Move = Vertical | Horizontal deriving (Show,Eq,Ord)
data Rotation = CtrClockwise | Clockwise deriving (Show,Eq)
data Color = Red | Yellow | Blue deriving (Show,Eq)


class (Eq a, Ord a) => Basis a where
     basis :: [a]

instance Basis Bool where
  basis = [False, True]

instance Basis Move where
  basis = [Vertical,Horizontal]

-- [ | ] list comprehension functional: no loops!!
instance (Basis a,Basis b) => Basis (a,b)
   where basis = [(a,b) | a <- basis, b <- basis]

type PA = Complex Double

type QV a = Map a PA


qv :: (Basis a) => [(a,PA)] -> QV a 
qv = fromList

qFT :: QV Bool
qFT = qv [(False,1/sqrt(2)),(True,1/sqrt(2))]

pr :: (Basis a) => QV a -> a -> PA
pr q b = findWithDefault 0 b q

qFalse, qTrue :: QV Bool
qFalse = Data.Map.singleton False 1
qTrue = Data.Map.singleton True 1


p1,p2,p3 :: QV (Bool,Bool)
p1 = qv [((False,False),1),((False,True),1)]
p2 = qv [((False,False),1),((True,True),1)]
p3 = qv [((False,False),1),
         ((False,True),1),
         ((True,False),1),
         ((True,True),1)]

-- Produto Tensorial
(&*) :: (Basis a, Basis b) => QV a -> QV b -> QV (a,b)
qa &* qb = qv[((a,b), pr qa a * pr qb b)|a <- basis, b <- basis]         


-- Operações quânticas como funções

qnot_f :: QV Bool -> QV Bool
qnot_f v = qv [(False, pr v True),
               (True, pr v False)]


hadamard_f :: QV Bool -> QV Bool
hadamard_f v = let p = pr v False
                   q = pr v True
               in qv[(False,p+q),(True,p-q)]    











