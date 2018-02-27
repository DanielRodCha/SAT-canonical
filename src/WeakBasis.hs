module WeakBasis where

import Logic
import Heuristics
import Preprocessing ( dimacs
                     , formulas)
import Saturation    ( forgetVarKB
                     , saturateKB)
--import MainFunctions
import System.Environment
import Data.List (nub,delete)
import qualified Data.Set as S

decided :: S.Set FProp -> Bool
decided kb = kb == (S.singleton T) || S.member F kb

notDecided = not . decided

forgetVarsKB :: S.Set FProp -> [VarProp] -> S.Set FProp
forgetVarsKB ps []     = ps
forgetVarsKB ps vs' = (forgetVarsKB aux vs)
                where (v:vs) = frequency ps vs' 
                      aux = forgetVarKB v ps

subsequencesN :: [a] -> [[a]]
subsequencesN [v] = [[]]
subsequencesN (v:vs) = [vs] ++ map (v:) (subsequencesN vs)

isWeakBasis :: S.Set FProp -> [VarProp] -> Bool
isWeakBasis kb vs = ((decided . forgetVarsKB kb) vs) &&
                     all notDecided [forgetVarsKB kb vs' | vs' <- subsequencesN vs]


algorithm :: (Eq a) => ([a] -> Bool) -> [a] -> [[a]]
algorithm p [] = []
algorithm p xs | (p xs) && (null aux) = [xs]
               | otherwise = nub $ concatMap (algorithm p) aux
                     where aux = filter p (subsequencesN xs)

-----------------------------

algorithm' :: (Eq a) => ([a] -> Bool) -> ([a],[a]) -> [[a]]
algorithm' _ (xs,[]) = [xs]
algorithm' p (xs,ys) | p xs = algorithm'Aux p xs aux
                     | otherwise = []
                              where aux = filter (p . fst) (subsequencesAux (xs,ys))

algorithm'Aux :: (Eq a) => ([a] -> Bool) -> [a] -> [([a],[a])] -> [[a]]
algorithm'Aux _ xs []  = [xs]
algorithm'Aux p _ aux = concatMap (algorithm' p) aux

subsequencesAux (_,[]) = []
subsequencesAux ([],_)  = []
subsequencesAux (xs,(y:ys)) = ((delete y xs,ys):subsequencesAux (xs,ys))


weakBasis' kb vs = algorithm' property (vs,vs)
                    where property = (decided . forgetVarsKB kb)
-----------------------------

-- algorithm' :: (Eq a) => ([a] -> Bool) -> [a] -> [[a]] -> [[a]]
-- algorithm' p [] acc = filter p acc
-- algorithm' p vs []  = []
-- algorithm' p (v:vs) (x:acc) sol | p x = algorithm' p vs acc (properInsert x sol)
--                                 | otherwise = expanse p (v:vs) x `properUnion`
--                                     algorithm' p vs acc
                               
-- expanse p [] _ = []
-- expanse p (v:vs) x = algorithm' p vs [v:x] `properUnion` expanse p vs x

-- anySubset [] _  = True
-- anySubset _ []  = True
-- anySubset xs ys = null $ foldr (\ x ys' -> if (elem x ys')
--                                            then (delete y ys)
--                                            else ys xs
--                 | otherwise = foldr (\ y acc -> acc && elem y xs) True ys

-- isSubset xs ys = foldr (\ x acc -> acc && elem x ys) True xs

-- anySubset' xs ys = isSubset xs ys || isSubset ys xs

-- properInsert x xs = foldr (\y acc -> isSubset x y )

-- properUnion a b = undefined

-- weakBasis' kb (v:vs) = algorithm' property vs [[w]|w<-(v:vs)] 
--                     where property = (decided . forgetVarsKB kb)

-- weakBasisCNF' f = do
--  putStrLn ("Weak Basis of instance " ++ f ++
--            " are:")
--  (f',vs) <- dimacs f
--  let sol = weakBasis' f' vs
--  return sol
-----------------------------

weakBasis kb vs = algorithm property vs
                  where property = (decided . forgetVarsKB kb)


weakBasisCNF f = do
 putStrLn ("Weak Basis of instance " ++ f ++
           " are:")
 (f',vs) <- dimacs f
 let sol = weakBasis f' vs
 return sol

weakBasisFORMULAS f = do
 putStrLn ("Weak Basis of instance " ++ f ++
           " are:")
 (f',vs) <- formulas f
 let sol = weakBasis f' vs
 return sol


-- --algorithmAux :: (Eq a) => ([a] -> Bool) -> [[a]] -> [[a]] -> [[a]]
-- algorithmAux p [] [] sol   = sol
-- algorithmAux p [] next sol = algorithmAux p next (S.empty) sol
-- algorithmAux p (x:xs) next sol | (p x) && (null aux) = algorithmAux p xs next (x:sol)
--                                | otherwise = nub $ concatMap (algorithm p) aux
--                                   where aux = filter p (subsequencesN xs)

-- Note: p se cumple para la lista mayor y se busca la menor lista que lo cumpla.

-- expanse (vs,vs') = [(v:vs,vs'') | (v,vs'') <- zip vs' (subsequencesN vs')]

-- calculateWB kb ((vs,vs'):vss) acum | (decided . forgetVarsKB kb) vs = calculateWB kb vss (vs:acum)
--                                    | calculateWB 