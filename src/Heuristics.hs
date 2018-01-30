module Heuristics ( Heuristics
                  , vars
                  , varsKB
                  , frequency
                  , revFreq ) where

import Logic
import Data.List (foldl', sortOn, union)
import qualified Data.Set as S

-- | For example, 
--
-- >>> vars (p ∧ q → p)
-- ["p","q"]
vars :: FProp -> [VarProp]
vars T          = []
vars F          = []
vars (Atom f)   = [f]
vars (Neg f)    = vars f
vars (Conj f g) = vars f `union` vars g
vars (Disj f g) = vars f `union` vars g
vars (Impl f g) = vars f `union` vars g
vars (Equi f g) = vars f `union` vars g


varsKB :: KB -> [VarProp]
varsKB = foldl (\acc f -> union acc (vars f)) []

-- | The Heuristic data type indicates the order in which variables
-- are forgotten. 
type Heuristics = S.Set FProp -> [VarProp] -> [VarProp]

-- | (__ frequency ps vs __) is the list of variables vs sorted by
-- frequency of occurrence in ps. For example:
--
-- >>> [x1,x2] = map var ["x1","x2"] :: [PolF2]
-- >>> frequency (S.fromList [x1,x2,x1+1]) [x1,x2]
-- [x2,x1]
frequency :: Heuristics
frequency ps vs = sortOn frequency' vs
   where frequency' v = length ( filter (== v) ps')
         ps' = foldl' (\acc p -> (vars p) ++ acc) [] ps

-- | (__ revFreq ps vs __) is the list of variables vs ordered in
-- reverse order of frequency in ps. For example:
--
-- >>> [x1,x2] = map var ["x1","x2"] :: [PolF2]
-- >>> revFreq (S.fromList [x1,x2,x1+1]) [x1,x2]
-- [x1,x2]
revFreq :: Heuristics
revFreq ps = (reverse . frequency ps)

