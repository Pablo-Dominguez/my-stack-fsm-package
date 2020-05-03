
module FSM.Logic (
    -- * Definition of the CTL language
    CTL (..),
    
    -- * Implementation of the model checking algorithm for CTL
    checkCTL
    
) where

import FSM.States
import FSM.Automata

import Data.Set
import qualified Data.Map as Map

-- | This is the definition of the CTL language. More info about this language can be found <https://en.wikipedia.org/wiki/Computation_tree_logic here>. You can find the details of the constructors in the definition of the 'CTL' data.
--
--
-- Here you can find a visual explanation of the constructors defined below.
-- <<https://i.imgur.com/e4tPiOY.jpg CTL examples>>
--
-- <https://www.researchgate.net/figure/CTL-tree-logic-1_fig6_257343964 Source>: A SAFE COTS-BASED DESIGN FLOW OF EMBEDDED SYSTEMS by Salam Hajjar
--

data CTL a = Atom a -- ^ It defines an atomic statement. E.g.:     'Atom' @"The plants look great."@
        | Not (CTL a) --  'Not' negates a 'CTL' formula.
        | And (CTL a) (CTL a) --  'And' 
        | Or (CTL a) (CTL a)
        | EX (CTL a) -- ^ 'EX' means that the 'CTL' formula holds in at least one of the inmediate successors states.
        | EF (CTL a) -- ^ 'EF' means that the 'CTL' formula holds in at least one of the future states.
        | EG (CTL a) -- ^ 'EG' means that the 'CTL' formula holds always from one of the future states.
        | AX (CTL a) -- ^ 'AX' means that the 'CTL' formula holds in every one of the inmediate successors states.
        | AF (CTL a) -- ^ 'AF' means that the 'CTL' formula holds in at least one state of every possible path.
        | AG (CTL a) -- ^ 'AG' means that the 'CTL' formula holds in the current states and all the successors in all paths. (It is true globally)
        | EU (CTL a) (CTL a) -- ^ 'EU' means that exists a path from the current state that satisfies the first 'CTL' formula /until/ it reaches a state in that path that satisfies the second 'CTL' formula.
        | AU (CTL a) (CTL a) -- ^ 'AU' means that every path from the current state satisfies the first 'CTL' formula /until/ it reaches a state that satisfies the second 'CTL' formula.
        deriving (Ord,Show)

instance Eq a => Eq (CTL a) where
    (Atom a) == (Atom b) = a == b
    (Not a) == (Not b) = a == b
    (And a b) == (And c d) = (a == c) && (b == d)
    (Or a b) == (Or c d) = (a == c) && (b == d)
    (EX a) == (EX b) = a == b
    (EF a) == (EF b) = a == b
    (EG a) == (EG b) = a == b
    (AX a) == (AX b) = a == b
    (AF a) == (AF b) = a == b
    (AG a) == (AG b) = a == b
    (EU a b) == (EU c d) = (a == c) && (b == d)
    (AU a b) == (AU c d) = (a == c) && (b == d)
    -- Aditional equivalences
    (Or a b) == (Not (And (Not c) (Not d))) = (a == c) && (b == d)
    (And a b) == (Not (Or (Not c) (Not d))) = (a == c) && (b == d)
    
    (AX a) == (Not (EX (Not b))) = a == b
    (AF a) == (Not (EX (Not b))) = a == b
    (AG a) == (Not (EX (Not b))) = a == b
    
    (EX a) == (Not (AX (Not b))) = a == b
    (EF a) == (Not (AX (Not b))) = a == b
    (EG a) == (Not (AX (Not b))) = a == b
    
    (AU a b) == Not (Or (EU (Not c1) (Not (Or d c2))) (EG (Not c3))) = (a == d) && (b == c1) && (c1 == c2) && (c2 == c3)
   
--
-- A(phi) = ¬ E (¬ phi)
             
-- Derived operators
{-
andCTL :: CTL a -> CTL a -> CTL a
andCTL p q = Not (Or (Not p) (Not q))

orCTL :: CTL a -> CTL a -> CTL a
orCTL p q = Not (And (Not p) (Not q))

aX :: CTL a -> CTL a
aX q = Not (EX (Not q))

aF :: CTL a -> CTL a
aF q = Not (EF (Not q))

aG :: CTL a -> CTL a
aG q = Not (EG (Not q))

aU :: CTL a -> CTL a -> CTL a 
aU p q = Not (orCTL (EU (Not q) (Not (orCTL p q))) (EG (Not q)))
-}
--A[φUψ] == ¬( E[(¬ψ)U¬(φ∨ψ)] ∨ EG(¬ψ) )

-- | This is the function that implements the model checking algorithm for CTL as defined by Queille, Sifakis, Clarke, Emerson and Sistla <https://dl.acm.org/doi/abs/10.1145/5397.5399 here> and that was later improved. 
--
-- This function takes as an argument a 'CTL' formula, an 'Automata' and information about the states as defined in "FSM.Automata" and "FSM.States" respectively and checks whether the 'Automata' implies the 'CTL' formula. Once the algorithm has finished, you just need to look at the value in the initial state of the automata to know if it does, for example with:
--
-- @
-- Map.lookup (getInitialState 'Automata') (checkCTL 'CTL' 'Automata' 'AutomataInfo')
-- @ 
--
checkCTL :: Eq a => CTL a -> Automata -> AutomataInfo (CTL a) -> Map.Map Int Bool
checkCTL (Atom a) tom info = 
    let states = (toList (getStates tom))
    in checkCTLaux (Atom a) info tom states (Map.fromList [(x,True) | x <- states])
checkCTL (Not a) tom info = notMap (checkCTL a tom info)
checkCTL (And a b) tom info = andMap (checkCTL a tom info) (checkCTL b tom info)
checkCTL (EX a) tom info =
    let states = (toList (getStates tom))
    in checkCTLaux (EX a) info tom states (Map.fromList [(x,False) | x <- states])
checkCTL (EU a b) tom info = 
    let sublabel1 = checkCTL a tom info
        sublabel2 = checkCTL b tom info
        states = (toList (getStates tom))
        init_list = [x | (x,k) <- (Map.toList sublabel2), k == True]
    in checkCTLauxEU (EU a b) info tom (Map.fromList [(x,False) | x <- states]) (Map.fromList [(x,False) | x <- states]) init_list sublabel1
checkCTL (AU a b) tom info = 
    let sublabel1 = checkCTL a tom info
        sublabel2 = checkCTL b tom info
        states = (toList (getStates tom))
        degree_map = Map.fromList [(x,length (toList (getOutgoingStates tom x))) | x <- states]
        label_map = (Map.fromList [(x,False) | x <- states])
        init_list = [x | (x,k) <- (Map.toList sublabel2), k == True]
    in checkCTLauxAU (AU a b) info tom label_map degree_map init_list sublabel1
    

checkCTLaux :: Eq a => CTL a -> AutomataInfo (CTL a) -> Automata -> [State] ->  Map.Map Int Bool -> Map.Map Int Bool
checkCTLaux (Atom a) info tom [l] label_map = 
    let content_info = getStateInfo (getInfoInState info l Nothing)
        content = Map.elems content_info
        new_bool = elem (Atom a) content
        f _ = Just new_bool
        new_map = Map.update f l label_map
    in new_map
checkCTLaux (Atom a) info tom (l:ls) label_map = 
    let content_info = getStateInfo (getInfoInState info l Nothing)
        content = Map.elems content_info
        new_bool = elem (Atom a) content
        f _ = Just new_bool
        new_map = Map.update f l label_map
    in checkCTLaux (Atom a) info tom ls new_map
checkCTLaux (EX a) info tom ls label_map = 
    let sublabel = checkCTL a tom info --habría que almacenarlo para no hacerlo en cada iteración, pero con ello tendría que o bien crear una función auxiliar extra o añadir otro argumento a esta función auxiliar
    in checkCTLauxEX (EX a) info tom ls label_map sublabel



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
            
andMap :: Map.Map Int Bool -> Map.Map Int Bool -> Map.Map Int Bool
andMap label_map1 label_map2 = andMapAux label_map1 label_map2 (Map.keys label_map1)

andMapAux ::  Map.Map Int Bool -> Map.Map Int Bool -> [Int] -> Map.Map Int Bool
andMapAux label_map1 label_map2 [l] = 
    let Just bool1 = Map.lookup l label_map1
        Just bool2 = Map.lookup l label_map2
        f _ = Just (bool1 && bool2)
        new_map = Map.update f l label_map1
    in new_map
andMapAux label_map1 label_map2 (l:ls) =
    let Just bool1 = Map.lookup l label_map1
        Just bool2 = Map.lookup l label_map2
        f _ = Just (bool1 && bool2)
        new_map = Map.update f l label_map1
    in andMapAux new_map label_map2 ls
    
checkCTLauxEX :: CTL a -> AutomataInfo (CTL a) -> Automata -> [State] ->  Map.Map Int Bool -> Map.Map Int Bool -> Map.Map Int Bool
checkCTLauxEX (EX a) info tom [l] label_map marked_map =
    let connected = toList (getOutgoingStates tom l)
        connected_map = Map.filterWithKey (\k _ -> (elem k connected)) marked_map
        new_bool = or (Map.elems connected_map)
        f _ = Just new_bool
        new_map = Map.update f l label_map
    in new_map
checkCTLauxEX (EX a) info tom (l:ls) label_map marked_map =
    let connected = toList (getOutgoingStates tom l)
        connected_map = Map.filterWithKey (\k _ -> (elem k connected)) marked_map
        new_bool = or (Map.elems connected_map)
        f _ = Just new_bool
        new_map = Map.update f l label_map
    in checkCTLauxEX (EX a) info tom ls new_map marked_map

checkCTLauxEU :: CTL a -> AutomataInfo (CTL a) -> Automata ->  Map.Map Int Bool -> Map.Map Int Bool -> [State] -> Map.Map Int Bool -> Map.Map Int Bool
checkCTLauxEU (EU a b) info tom label_map seenbefore_map [] sublabel = label_map
checkCTLauxEU (EU a b) info tom label_map seenbefore_map (k:ks) sublabel = 
    let previous_states = toList (getIncomingStates tom k)
        f _ = Just True
        new_map = Map.update f k label_map
        (added_previous,new_seenbefore_map) = checkEUprevious seenbefore_map previous_states sublabel ks
    in checkCTLauxEU (EU a b) info tom new_map new_seenbefore_map added_previous sublabel
        
        
        
checkEUprevious :: Map.Map Int Bool -> [State] -> Map.Map Int Bool -> [State] -> ([State],Map.Map Int Bool)
checkEUprevious seenbefore_map [] sublabel ls = (ls,seenbefore_map)
checkEUprevious seenbefore_map (p:ps) sublabel ls 
    | previous_bool == False = 
        let f _ = Just True
            new_seenbefore_map = Map.update f p seenbefore_map
        in if previous_marked == True
            then checkEUprevious new_seenbefore_map ps sublabel (ls++[p])
            else checkEUprevious new_seenbefore_map ps sublabel ls
    | otherwise = checkEUprevious seenbefore_map ps sublabel ls 
    where Just previous_bool = Map.lookup p seenbefore_map
          Just previous_marked = Map.lookup p sublabel

checkCTLauxAU :: (CTL a) -> AutomataInfo (CTL a) -> Automata ->  Map.Map Int Bool -> Map.Map Int Int -> [State] ->  Map.Map Int Bool ->  Map.Map Int Bool 
checkCTLauxAU (AU a b) info tom label_map degree_map (l:ls) sublabel =
    let previous_states = toList (getIncomingStates tom l)
        f _ = Just True
        new_map = Map.update f l label_map
        (added_previous,new_degree_map) = checkAUprevious new_map degree_map previous_states sublabel ls
    in checkCTLauxAU (AU a b) info tom new_map new_degree_map added_previous sublabel


        
checkAUprevious :: Map.Map Int Bool -> Map.Map Int Int -> [State] -> Map.Map Int Bool -> [State] -> ([State], Map.Map Int Int)
checkAUprevious label_map degree_map [] sublabel ls = (ls,degree_map)
checkAUprevious label_map degree_map (p:ps) sublabel ls =
    if (new_degree == 0) && (previous_marked == True) && (label == False)
    then checkAUprevious label_map new_degree_map ps sublabel (ls++[p])
    else checkAUprevious label_map new_degree_map ps sublabel ls
    where Just previous_degree = Map.lookup p degree_map
          new_degree = previous_degree -1
          f _ = Just new_degree
          new_degree_map = Map.update f p degree_map
          Just previous_marked = Map.lookup p sublabel
          Just label = Map.lookup p label_map
          
