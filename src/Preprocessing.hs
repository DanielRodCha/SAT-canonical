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

lit :: String -> FProp
lit "0"       = F
lit ('-':xs) = Neg (Atom ('p':xs))
lit xs = Atom ('p':xs)

clause (c:cs) | c == "c" || c == "p" = T
clause    cs = simplification $ foldl' (\acc x -> Disj (lit x) acc) (F) cs

-- | __(dimacs f)__ is the pair (/ps/,/vs/) where ps is the set of formulas
-- wich corresponds to the formula in DIMACS format writed in the file /f/ and
-- /vs/ is the list of variables wich occurs in any propositional formula.
dimacs f = do
  s0 <- readFile f
  return $ aux $ foldr (\x acc -> S.insert ((clause . words) x) acc) (S.empty) $ lines $ s0
     where aux x = (x,(varsKB x))

-- | __(formulas f)__ is the pair (/ps/,/vs/) where ps is the set of formulas
-- wich corresponds to the formula in Prover9 format writed in the file /f/ and
-- /vs/ is the list of variables wich occurs in any propositional formula.
formulas f = do
  s0 <- readFile f
  return $ aux $ foldr (\x acc -> S.insert ((unbox . parseFProp) x) acc) (S.empty) $ lines $ s0
     where aux x           = (x,(varsKB x))
           unbox (Right x) = x
