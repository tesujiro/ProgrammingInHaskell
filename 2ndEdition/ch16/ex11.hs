-- Assumption
comp' e c = comp e ++ c

-- Base
comp' (Val n) c
 -- Apply Assumption
 = comp (Val n) ++ c
 -- Apply Definition
 = [PUSH n] ++ c
 = PUSH n : c

comp' (Add x y) c
 -- Apply Assumption
 = comp (Add x y) ++ c
 -- Apply Definition
 = comp x ++ comp y ++ [ADD] ++ c
 = comp x ++ (comp y ++ (ADD : c))
 -- Apply Reverse Assumption
 = comp' x (comp y ++ (ADD : c))
 -- Apply Reverse Assumption
 = comp' x (comp' y (ADD : c))


