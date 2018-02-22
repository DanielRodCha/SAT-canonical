module MainFunctions where

import Logic
import Heuristics
import Canonical (canonical)
import Preprocessing ( dimacs
                     , formulas)
import Saturation    ( forgetVarKB
                     , saturateKB)
import WeakBasis
import System.Environment
import qualified Data.Set as S

forgetVarListKB :: S.Set FProp -> [VarProp] -> [S.Set FProp]
forgetVarListKB _ []   = []
forgetVarListKB ps (v:vs) = (aux:(forgetVarListKB aux vs))
                where aux = forgetVarKB v ps

forgetVarListKB' :: S.Set FProp -> [VarProp] -> Heuristics -> [S.Set FProp]
forgetVarListKB' _ [] _ = []
forgetVarListKB' ps vs h = (aux:(forgetVarListKB' aux vs' h))
                   where (v:vs') = h ps vs
                         aux = forgetVarKB v ps

saturateKBTrace :: S.Set FProp -> [VarProp] -> Heuristics -> [(S.Set FProp,Bool)]
saturateKBTrace ps [] _                     = [(ps,S.notMember F ps)]
saturateKBTrace ps (v:vs) h | S.member F ps = [(ps,False)]
                            | otherwise     = ((ps,True):(saturateKBTrace aux (h aux vs) h))
                               where aux    = forgetVarKB v ps

saturateKBSizeTrace :: S.Set FProp -> [VarProp] -> Heuristics -> [((VarProp, Int),Bool)]
saturateKBSizeTrace ps [] _                     = [(("TheEnd",S.size ps),S.notMember F ps)]
saturateKBSizeTrace ps (v:vs) h | S.member F ps = [((v,S.size ps),False)]
                                | otherwise     = (((v,S.size ps),True):(saturateKBSizeTrace aux (h aux vs) h))
                                   where aux    = forgetVarKB v ps
                               
satFORMULAS f h = do
 (f',vs) <- formulas f
 putStrLn ("The KB is:") 
 print f'
 putStrLn ("And the satisfactibility of instance " ++ f ++
           " is:")
 let sol = saturateKB f' (h f' vs) h
 return sol

satCNF f h = do
 putStrLn ("The satisfactibility of instance " ++ f ++
           " is:")
 (f',vs) <- dimacs f
 let sol = saturateKB f' (h f' vs) h
 return sol