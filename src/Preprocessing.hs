module Preprocessing where

import Logic
import LogicParser
import Simplification (simplification)
import Heuristics (vars
                  , varsKB
                  , frequency)

import Data.List (foldl'
                 , union)
import Data.Char
import qualified Data.Set as S

literal2Pol :: String -> FProp
literal2Pol "0"       = T
literal2Pol ('-':lit) = Neg (Atom lit)
literal2Pol lit       = Atom lit

-- | __(clause2pol cs)__ is a pair /(p,vs)/, where /p/ is the polynomial that
-- corresponds to the clause /cs/ (which is written in DIMACS format) and /vs/ is
-- the set of its variables. For example,
--
-- >>> clause2Pol ["1"]
-- (x1,fromList [x1])
-- >>> clause2Pol ["1","-2"]
-- (x1x2+x2+1,fromList [x1,x2])
clause2Pol :: [String] -> (FProp,[VarProp])
clause2Pol (c:cs) | c == "c" || c == "p" = (T,[])
clause2Pol cs = aux $ foldl' (\acc x -> Disj (literal2Pol x) acc) (T) cs
  where aux a = (a,vars a)

-- | __(dimacs2pols f)__ is the pair (/ps/,/vs/) where ps is the set of polynomials
-- wich corresponds to the formula in DIMACS format writed in the file /f/ and
-- /vs/ is the list of variables wich occurs in any polynomial. For example,
--
-- >>> dimacs2Pols "exDIMACS/easy/example1.txt"
-- (fromList [x1x2+x1+x2,1],[x1,x2])
-- >>> dimacs2Pols "exDIMACS/easy/example4.txt"
-- (fromList [x1x2+x1+x2,x1x2+x1+1,x1x2+x2+1,x1x2+1,1],[x1,x2])
dimacs' f = do
  s0 <- readFile f
  return $
    (foldr (\x acc -> (aux2 ((clause2Pol . words) x) acc))
             (S.empty,[])) $ lines $ s0
     where aux2 (a,b) (acc,vs) = (S.insert a acc, union vs b)

lit :: String -> FProp
lit "0"       = F
lit ('-':xs) = Neg (Atom ('p':xs))
lit xs = Atom ('p':xs)

clause (c:cs) | c == "c" || c == "p" = T
clause    cs = simplification $ foldl' (\acc x -> Disj (lit x) acc) (F) cs

dimacs f = do
  s0 <- readFile f
  return $ aux $ foldr (\x acc -> S.insert ((clause . words) x) acc) (S.empty) $ lines $ s0
     where aux x = (x,(varsKB x))

-- | __(formulas f)__ is the pair (/ps/,/vs/) where ps is the set of polynomials
-- wich corresponds to the formula in FORMULAS format writed in the file /f/ and
-- /vs/ is the list of variables wich occurs in any polynomial. For example,
formulas f = do
  s0 <- readFile f
  return $
    (foldr (\x acc -> (aux2 (aux3 x) acc))
             (S.empty,[])) $ lines $ s0
     where aux2 (a,b) (acc,vs) = (S.insert a acc, union vs b)
--           aux2 (a,b) (acc',vs) = (S.insert a (removeDivisors a acc'), S.union vs b)
           aux3   = aux4 . unbox . parseFProp
           aux4 x = (x, vars x)

unbox :: Either a b -> b
unbox (Right x) = x