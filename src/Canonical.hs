module Canonical where 

import Logic
import Simplification (simplification)
import qualified Data.Set as S

canonical :: VarProp -> FProp -> FProp -> FProp
canonical v f g = simplification $ canonical' v f g

canonical' :: VarProp -> FProp -> FProp -> FProp
canonical' v f g = Disj (substitute fg v T) (substitute fg v F)
                   where fg = Conj f g