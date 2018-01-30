module Simplification where 

import Logic
import qualified Data.Set as S

-- | (withTF f) is verified if the formula /f/ contains any occurrence
-- of the constants T or F:
withTF :: FProp -> Bool
withTF (T)        = True
withTF (F)        = True
withTF (Atom x)   = False
withTF (Neg x)    = withTF x
withTF (Conj x y) = withTF x || withTF y
withTF (Disj x y) = withTF x || withTF y
withTF (Impl x y) = withTF x || withTF y
withTF (Equi x y) = withTF x || withTF y

simplification :: FProp -> FProp
simplification (T)           = T
simplification (F)           = F
simplification (Neg T)       = F
simplification (Neg F)       = T
simplification for | (not . withTF) for = for
                   | otherwise = simplification' for

simplification' (Conj T y) = simplification y
simplification' (Conj x T) = simplification x
simplification' (Disj T y) = T
simplification' (Disj x T) = T
simplification' (Conj F y) = F
simplification' (Conj x F) = F
simplification' (Disj F y) = simplification y
simplification' (Disj x F) = simplification x
simplification' (Impl T y) = simplification y
simplification' (Impl x T) = T
simplification' (Impl F y) = T
simplification' (Impl x F) = simplification (Neg (simplification x))
simplification' (Conj x y) = simplification (Conj (simplification x) (simplification y))
simplification' (Disj x y) = simplification (Disj (simplification x) (simplification y))
simplification' (Impl x y) = simplification (Impl (simplification x) (simplification y))
simplification' (Neg x)    = simplification (Neg (simplification x))
simplification' (Equi x y) = simplification (Conj (Impl x y) (Impl y x))

-- |
-- >>> quickCheck prop_simp
-- +++ OK, passed 100 tests.
prop_simp :: FProp -> Bool
prop_simp = (not . withTF . simplification)
