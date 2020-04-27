
module FSM.Logic where

import FSM.States
import FSM.Automata

import Data.Set
import qualified Data.Map as Map

             
data CTL a = Atom a
            | Not (CTL a)
            | Or (CTL a) (CTL a)
            | EX (CTL a)
            | EF (CTL a)
            | EG (CTL a)
            | EU (CTL a) (CTL a)
            deriving (Eq,Ord,Show)
            

            
-- A(phi) = ¬ E (¬ phi)
             
-- Derived operators

andCTL :: CTL a -> CTL a -> CTL a
andCTL p q = Not (Or (Not p) (Not q))

aX :: CTL a -> CTL a
aX q = Not (EX (Not q))

aF :: CTL a -> CTL a
aF q = Not (EF (Not q))

aG :: CTL a -> CTL a
aG q = Not (EG (Not q))

aU :: CTL a -> CTL a -> CTL a 
aU p q = Not (Or (EU (Not q) (Not (Or p q))) (EG (Not q)))

--A[φUψ] == ¬( E[(¬ψ)U¬(φ∨ψ)] ∨ EG(¬ψ) )

checkCTL :: Eq a => CTL a -> Automata -> AutomataInfo (CTL a) -> Map.Map Int Bool
checkCTL (Atom a) tom info = 
    let states = (toList (getStates tom))
    in checkCTLaux (Atom a) info states (Map.fromList [(x,True) | x <- states])
checkCTL (Not a) tom info = notMap (checkCTL a tom info)




checkCTLaux :: Eq a => CTL a -> AutomataInfo (CTL a) -> [State] ->  Map.Map Int Bool -> Map.Map Int Bool
checkCTLaux (Atom a) info [l] label_map = 
    let content_info = getStateInfo (getInfoInState info l Nothing)
        content = Map.elems content_info
        new_bool = elem (Atom a) content
        f _ = Just new_bool
        new_map = Map.update f l label_map
    in new_map
checkCTLaux (Atom a) info (l:ls) label_map = 
    let content_info = getStateInfo (getInfoInState info l Nothing)
        content = Map.elems content_info
        new_bool = elem (Atom a) content
        f _ = Just new_bool
        new_map = Map.update f l label_map
    in checkCTLaux (Atom a) info ls new_map



notMap :: Map.Map Int Bool -> Map.Map Int Bool
notMap label_map = notMapAux label_map (Map.keys label_map)

notMapAux :: Map.Map Int Bool -> [Int] -> Map.Map Int Bool
notMapAux label_map [l] = 
    let Just old_bool = Map.lookup l label_map
        f _ = Just (not old_bool)
        new_map = Map.update f l label_map
    in new_map
notMapAux label_map (l:ls) = 
    let Just old_bool = Map.lookup l label_map
        f _ = Just (not old_bool)
        new_map = Map.update f l label_map
    in notMapAux new_map ls
            
