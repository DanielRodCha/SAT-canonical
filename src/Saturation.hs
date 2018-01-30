module Saturation where 

import Logic
import Heuristics (Heuristics
                  , vars
                  , varsKB
                  , frequency)
import Canonical (canonical)
import Data.List (union)
import qualified Data.Set as S

canonicalAux :: VarProp -> FProp -> S.Set FProp -> S.Set FProp -> S.Set FProp
canonicalAux v f kb acum
  | S.null kb = acum
  | cR == (F)   = S.fromList [F]
  | otherwise = canonicalAux v f kb' (S.insert cR acum)
                where (g,kb') = S.deleteFindMin kb
                      cR      = canonical v f g

canonicalKB :: VarProp -> S.Set FProp -> S.Set FProp -> S.Set FProp
canonicalKB v kb acum
  | acum == S.fromList [F] = acum
  | S.null kb              = acum
  | otherwise              = canonicalKB v kb' (canonicalAux v f kb acum)
             where (f,kb') = S.deleteFindMin kb

forgetVarKB :: VarProp -> S.Set FProp -> S.Set FProp
forgetVarKB v kb = canonicalKB v kb1 kb2
      where (kb1,kb2) = S.partition (\f -> elem v (vars f)) kb
      
saturateKB :: S.Set FProp -> [VarProp] -> Heuristics -> Bool
saturateKB kb [] _                     = S.notMember F kb
saturateKB kb (v:vs) h | S.member F kb = False
                       | otherwise     = saturateKB aux (h aux vs) h
                             where aux = forgetVarKB v kb