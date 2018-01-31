module Main where

import Preprocessing
import Saturation
import System.Environment
import Heuristics ( Heuristics
                  , frequency
                  , revFreq)

-- | __(main f)__ is verified if the set of formulas in DIMACS format in the
-- file /f/ were satisfiable. Otherwise, /(main f)/ would return False.

main = do
 [f,h] <- getArgs
 putStrLn ("The satisfactibility of instance " ++ f ++ " is:")
 (f',f'') <- formulas f
 let h' = aux h 
 let sol = saturateKB f' (h' f' f'') h'
 print sol
 return sol


aux :: String -> Heuristics
aux "frequency"   = frequency
aux "revFreq"     = revFreq