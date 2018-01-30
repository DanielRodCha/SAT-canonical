module Logic where 

import Control.Monad ( liftM
                     , liftM2)
import Data.List     ( union
                     , subsequences)
import Test.QuickCheck ( Arbitrary
                       , arbitrary
                       , oneof
                       , elements
                       , sized
                       , quickCheck)
import qualified Data.Set as S

type VarProp = String

data FProp = T
           | F
           | Atom VarProp
           | Neg FProp 
           | Conj FProp FProp 
           | Disj FProp FProp 
           | Impl FProp FProp 
           | Equi FProp FProp 
  deriving (Eq,Ord)

instance Show FProp where
    show (T)        = "⊤"
    show (F)        = "⊥"
    show (Atom x)   = x
    show (Neg x)    = "-" ++ show x
    show (Conj x y) = "(" ++ show x ++ " ∧ " ++ show y ++ ")"
    show (Disj x y) = "(" ++ show x ++ " ∨ " ++ show y ++ ")"
    show (Impl x y) = "(" ++ show x ++ " -> " ++ show y ++ ")"
    show (Equi x y) = "(" ++ show x ++ " <-> " ++ show y ++ ")"

p, q, r :: FProp
p  = Atom "p"
q  = Atom "q"
r  = Atom "r"

no :: FProp -> FProp
no = Neg

(∨) :: FProp -> FProp -> FProp
(∨)   = Disj
infixr 5 ∨

(∧) :: FProp -> FProp -> FProp
(∧)   = Conj
infixr 4 ∧

(→) :: FProp -> FProp -> FProp
(→)  = Impl
infixr 3 →

(<->) :: FProp -> FProp -> FProp
(<->) = Equi
infixr 2 <->

instance Arbitrary FProp where
  arbitrary = sized prop
    where
      prop n  | n <= 0     = atom
              | otherwise  = oneof [ 
                    atom
                    , liftM Neg subform
                    , liftM2 Conj subform  subform
                    , liftM2 Disj subform  subform
                    , liftM2 Impl subform  subform
                    , liftM2 Equi subform' subform' ]
        where
          atom     = oneof [liftM Atom (elements ["p","q","r","s"]),
                            elements [F,T]]
          subform  = prop ( n `div` 2)
          subform' = prop ( n `div` 4)

-- | For example,
-- >>> substitute (no p) "p" q
-- ¬q
-- >>> substitute (no (q ∧ no p)) "p" (q ↔ p)
-- ¬(q ∧ ¬(q ↔ p))
substitute :: FProp -> VarProp -> FProp -> FProp
substitute T            _ _ = T
substitute F            _ _ = F
substitute (Atom f)     p g | f == p = g
                           | otherwise = Atom f
substitute (Neg f)      p g = Neg (substitute f p g)
substitute (Conj f1 f2) p g = Conj (substitute f1 p g) (substitute f2 p g)
substitute (Disj f1 f2) p g = Disj (substitute f1 p g) (substitute f2 p g)
substitute (Impl f1 f2) p g = Impl (substitute f1 p g) (substitute f2 p g)
substitute (Equi f1 f2) p g = Equi (substitute f1 p g) (substitute f2 p g)

type Interpretation = [FProp]

-- | For example, 
-- 
-- >>> signify ((p ∨ q) ∧ ((no q) ∨ r)) [r]
-- False
-- >>> signify ((p ∨ q) ∧ ((no q) ∨ r)) [p,r]
-- True
signify :: FProp -> Interpretation -> Bool
signify T          _ = True
signify F          _ = False
signify (Atom f)   i = (Atom f) `elem` i
signify (Neg f)    i = not (signify f i)
signify (Conj f g) i = (signify f i) && (signify g i)
signify (Disj f g) i = (signify f i) || (signify g i)
signify (Impl f g) i = signify (Disj (Neg f) g) i
signify (Equi f g) i = signify (Conj (Impl f g) (Impl g f)) i

-- | For example,
--
-- >>> isModelForm [r]   ((p ∨ q) ∧ ((no q) ∨ r))
-- False
-- >>> isModelForm [p,r] ((p ∨ q) ∧ ((no q) ∨ r))
-- True
isModelForm :: Interpretation -> FProp -> Bool
isModelForm i f = signify f i

-- | For example, 
--
-- >>> propSymbolsForm (p ∧ q → p)
-- [p,q]
propSymbolsForm :: FProp -> [FProp]
propSymbolsForm T          = []
propSymbolsForm F          = []
propSymbolsForm (Atom f)   = [(Atom f)]
propSymbolsForm (Neg f)    = propSymbolsForm f
propSymbolsForm (Conj f g) = propSymbolsForm f `union` propSymbolsForm g
propSymbolsForm (Disj f g) = propSymbolsForm f `union` propSymbolsForm g
propSymbolsForm (Impl f g) = propSymbolsForm f `union` propSymbolsForm g
propSymbolsForm (Equi f g) = propSymbolsForm f `union` propSymbolsForm g

-- |  For example,
--
-- >>> interpretationsForm (p ∧ q → p)
-- [[],[p],[q],[p,q]]
interpretationsForm :: FProp -> [Interpretation]
interpretationsForm = subsequences . propSymbolsForm

-- | For example,
--
-- >>> modelsForm ((p ∨ q) ∧ ((no q) ∨ r)) 
-- [[p],[p,r],[q,r],[p,q,r]]
modelsForm :: FProp -> [Interpretation]
modelsForm f = [i | i <- interpretationsForm f, isModelForm i f]

-- | For example,
--
-- >>> isValid (p → p)
-- True
-- >>> isValid (p → q)
-- False
-- >>> isValid ((p → q) ∨ (q → p))
-- True
isValid :: FProp -> Bool
isValid f = modelsForm f == interpretationsForm f

-- | For example,
--
-- >>> isUnsatisfiable (p ∧ (no p))
-- True
-- >>> isUnsatisfiable ((p → q) ∧ (q → r))
-- False
isUnsatisfiable :: FProp -> Bool
isUnsatisfiable = null . modelsForm

-- | For example,
--
-- >>> isSatisfiable (p ∧ (no p))
-- False
-- >>> isSatisfiable ((p → q) ∧ (q → r))
-- True
isSatisfiable :: FProp -> Bool
isSatisfiable = not . null . modelsForm

type KB = S.Set FProp

-- | For example,
--
-- >>> propSymbolsKB (S.fromList [p ∧ q → r, p → r])
-- [p,r,q]
propSymbolsKB :: KB -> [FProp]
propSymbolsKB = foldl (\acc f -> union acc (propSymbolsForm f)) []

-- |  For example,
--
-- >>> interpretationsKB (S.fromList [p → q, q → r])
-- [[],[p],[q],[p,q],[r],[p,r],[q,r],[p,q,r]]
interpretationsKB :: KB -> [Interpretation]
interpretationsKB = subsequences . propSymbolsKB

-- | For example, 
--
-- >>> isModelKB [r] (S.fromList [q,no p ,r])
-- False
-- >>> isModelKB [q,r] (S.fromList [q,no p ,r])
-- True
isModelKB :: Interpretation -> KB -> Bool
isModelKB i = all (isModelForm i)

-- | For example,
--
-- >>> modelsKB $ S.fromList [(p ∨ q) ∧ ((no q) ∨ r), q → r]
-- [[p],[p,r],[q,r],[p,q,r]]
-- >>> modelsKB $ S.fromList [(p ∨ q) ∧ ((no q) ∨ r), r → q]
-- [[p],[q,r],[p,q,r]]
modelsKB :: KB -> [Interpretation]
modelsKB s = [i | i <- interpretationsKB s, isModelKB i s]

-- |For example,
--
-- >>> isConsistent $ S.fromList [(p ∨ q) ∧ ((no q) ∨ r), p → r]
-- True
-- >>> isConsistent $ S.fromList [(p ∨ q) ∧ ((no q) ∨ r), p → r, no r]
-- False
isConsistent :: KB -> Bool
isConsistent = not . null . modelsKB

-- | For example,
--
-- >>> isInconsistent $ S.fromList [(p ∨ q) ∧ ((no q) ∨ r), p → r]        
-- False
-- >>> isInconsistent $ S.fromList [(p ∨ q) ∧ ((no q) ∨ r), p → r, no r]  
-- True
isInconsistent :: KB -> Bool
isInconsistent = null . modelsKB

-- |For example,
--
-- >>> isConsequence (S.fromList [p → q, q → r]) (p → r)
-- True
-- >>> isConsequence (S.fromList [p]) (p ∧ q)
-- False
isConsequence :: KB -> FProp -> Bool
isConsequence k f =
  null [i | i <- interpretationsKB (S.insert f k)
          , isModelKB i k
          , not (isModelForm i f)]

-- |
-- >>> quickCheck prop_isValid
-- +++ OK, passed 100 tests.
prop_isValid :: FProp -> Bool
prop_isValid f =
   isValid f == isConsequence S.empty f

-- |
-- >>> quickCheck prop_isConsequence
-- +++ OK, passed 100 tests.
prop_isConsequence :: KB -> FProp -> Bool
prop_isConsequence k f =
   isConsequence k f == isInconsistent (S.insert (Neg f) k)

-- | For example,
--
-- >>> isConsequenceKB (S.fromList [p → q, q → r]) (S.fromList [p → q, p → r])
-- True
-- >>> isConsequenceKB (S.fromList [p]) (S.fromList [p ∧ q])
-- False
isConsequenceKB :: KB -> KB -> Bool
isConsequenceKB k = all (isConsequence k)

-- | For example,
--
-- >>> equivalent (p → q) (no p ∨ q)
-- True
-- >>> equivalent (p) (no (no p))
-- True
equivalent :: FProp -> FProp -> Bool
equivalent f g = isValid (f <-> g)

-- |For example,
--
-- >>> equivalentKB (S.fromList [p → q,r ∨ q]) (S.fromList [no p ∨ q, q ∨ r])
-- True
-- >>> equivalentKB (S.fromList [p ∧ q]) (S.fromList [q,p])
-- True
equivalentKB :: KB -> KB -> Bool
equivalentKB k k' = isConsequenceKB k k' && isConsequenceKB k' k

-- |
-- >>> quickCheck prop_equivalent
-- +++ OK, passed 100 tests.
prop_equivalent :: FProp -> FProp -> Bool
prop_equivalent f g =
  equivalent f g == equivalentKB (S.singleton f) (S.singleton g)