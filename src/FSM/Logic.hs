
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

data CTL a = CTrue | CFalse -- ^ Basic bools.
        | RArrow a b -- ^ Basic imply.
        | DArrow a b -- ^ Basic double imply.
        | Atom a -- ^ It defines an atomic statement. E.g.:     'Atom' @"The plants look great."@
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
    (CTrue) == Not (CFalse) = True
    (Atom a) == (Atom b) = a == b
    (Not a) == (Not b) = a == b
    (And a b) == (And c d) = (a == c) && (b == d)
    (Or a b) == (Or c d) = (a == c) && (b == d)
    (RArrow a b) == (Or (Not c) d) = (a == c) && (b == d)
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
    Not (Not a) == b = a == b
    
    (AX a) == (Not (EX (Not b))) = a == b
    (AF a) == (Not (EX (Not b))) = a == b
    (AG a) == (Not (EX (Not b))) = a == b
    
    (EX a) == (Not (AX (Not b))) = a == b
    (EF a) == (Not (AX (Not b))) = a == b
    (EG a) == (Not (AX (Not b))) = a == b
    
    (AU a b) == Not (Or (EU (Not c1) (Not (Or d c2))) (EG (Not c3))) = (a == d) && (b == c1) && (c1 == c2) && (c2 == c3)

data LTL a = LTL_Atom a -- ^ It defines an atomic statement. E.g.:     'Atom' @"The plants look great."@
        | LTL_Not (LTL a) --  'Not' negates a 'CTL' formula.
        | LTL_And (LTL a) (LTL a) --  'And' 
        | LTL_Or (LTL a) (LTL a)
        | LTL_X (LTL a) -- ^ 'EX' means that the 'CTL' formula holds in at least one of the inmediate successors states.
        | LTL_F (LTL a) -- ^ 'EF' means that the 'CTL' formula holds in at least one of the future states.
        | LTL_G (LTL a) -- ^ 'EG' means that the 'CTL' formula holds always from one of the future states.
        | LTL_U (LTL a) (LTL a) -- ^ 'EU' means that exists a path from the current state that satisfies the first 'CTL' formula /until/ it reaches a state in that path that satisfies the second 'CTL' formula.
        deriving (Ord,Show)   
        
instance Eq a => Eq (LTL a) where
    (LTL_Atom a) == (LTL_Atom b) = a == b
    (LTL_Not a) == (LTL_Not b) = a == b
    (LTL_And a b) == (LTL_And c d) = (a == c) && (b == d)
    (LTL_Or a b) == (LTL_Or c d) = (a == c) && (b == d)
    (LTL_X a) == (LTL_X b) = a == b
    (LTL_F a) == (LTL_F b) = a == b
    (LTL_G a) == (LTL_G b) = a == b
    (LTL_U a b) == (LTL_U c d) = (a == c) && (b == d)
    -- Aditional equivalences
    (LTL_Or a b) == (LTL_Not (LTL_And (LTL_Not c) (LTL_Not d))) = (a == c) && (b == d)
    (LTL_And a b) == (LTL_Not (LTL_Or (LTL_Not c) (LTL_Not d))) = (a == c) && (b == d)
    


-- | This is the function that implements the model checking algorithm for CTL as defined by Queille, Sifakis, Clarke, Emerson and Sistla <https://dl.acm.org/doi/abs/10.1145/5397.5399 here> and that was later improved. 
--
-- This function takes as an argument a 'CTL' formula, an 'Automata' and information about the states as defined in "FSM.Automata" and "FSM.States" respectively and checks whether the 'Automata' implies the 'CTL' formula. Once the algorithm has finished, you just need to look at the value in the initial state of the automata to know if it does, for example with:
--
-- @
-- Map.lookup (getInitialState 'Automata') (checkCTL 'CTL' 'Automata' 'AutomataInfo')
-- @ 
--
checkCTL :: Eq a => CTL a -> Automata -> AutomataInfo (CTL a) -> Map.Map Int Bool
checkCTL CTrue tom info = 
    let states = (toList (getStates tom))
    in Map.fromList [(x,True) | x <- states]
checkCTL (Atom a) tom info =
    let states = (toList (getStates tom))
    in checkCTLauxAtom (Atom a) info tom states Map.empty
checkCTL (Not a) tom info = checkCTLauxNot (checkCTL a tom info)
checkCTL (And a b) tom info = checkCTLauxAnd (checkCTL a tom info) (checkCTL b tom info)
checkCTL (Or a b) tom info = checkCTL (Not (And (Not a) (Not b))) tom info
checkCTL (RArrow a b) tom info = checkCTL (Or (Not a) b) tom info
checkCTL (DArrow a b) tom info = checkCTL (Or (And a b) (And (Not a) (Not b))) tom info
checkCTL (EX a) tom info =
  let states = (toList (getStates tom))
      sublabel = checkCTL a tom info
  in checkCTLauxEX tom states sublabel (Map.fromList [(x,False) | x <- states])
checkCTL (AX a) tom info = checkCTL (Not (EX (Not a))) tom info
checkCTL (EU a b) tom info = 
    let sublabel1 = checkCTL a tom info
        sublabel2 = checkCTL b tom info
        states = (toList (getStates tom))
        init_list = [x | (x,k) <- (Map.toList sublabel2), k == True]
    in checkCTLauxEU tom (Map.fromList [(x,False) | x <- states]) (Map.fromList [(x,False) | x <- states]) init_list sublabel1
checkCTL (EF a) tom info = checkCTL (EU CTrue a) tom info
checkCTL (AU a b) tom info = 
    let sublabel1 = checkCTL a tom info
        sublabel2 = checkCTL b tom info
        states = (toList (getStates tom))
        degree_map = Map.fromList [(x,length (toList (getOutgoingStates tom x))) | x <- states]
        label_map = (Map.fromList [(x,False) | x <- states])
        init_list = [x | (x,k) <- (Map.toList sublabel2), k == True]
    in checkCTLauxAU tom label_map degree_map init_list sublabel1
checkCTL (AF a) tom info = checkCTL (AU CTrue a) tom info
checkCTL (EG a) tom info = checkCTL (Not (AF (Not a))) tom info
checkCTL (AG a) tom info = checkCTL (Not (EF (Not a))) tom info
    

checkCTLauxAtom :: Eq a => CTL a -> AutomataInfo (CTL a) -> Automata -> [State] ->  Map.Map Int Bool -> Map.Map Int Bool
checkCTLauxAtom (Atom a) info tom [] label_map = label_map
checkCTLauxAtom (Atom a) info tom (l:ls) label_map =
  let content_info = getStateInfo (getInfoInState info l Nothing) 
      content = Map.elems content_info
      new_bool = elem (Atom a) content
      f _ = Just new_bool
      new_map = Map.alter f l label_map
  in checkCTLauxAtom (Atom a) info tom ls new_map   
    
    
checkCTLauxNot :: Map.Map Int Bool -> Map.Map Int Bool
checkCTLauxNot label_map = notMapAux label_map (Map.keys label_map)

notMapAux :: Map.Map Int Bool -> [Int] -> Map.Map Int Bool
notMapAux label_map [] = label_map
notMapAux label_map (l:ls) = 
    let Just old_bool = Map.lookup l label_map
        f _ = Just (not old_bool)
        new_map = Map.update f l label_map
    in notMapAux new_map ls
            
checkCTLauxAnd :: Map.Map Int Bool -> Map.Map Int Bool -> Map.Map Int Bool
checkCTLauxAnd label_map1 label_map2 = andMapAux label_map1 label_map2 (Map.keys label_map1)

andMapAux ::  Map.Map Int Bool -> Map.Map Int Bool -> [Int] -> Map.Map Int Bool
andMapAux label_map1 label_map2 [] = label_map1
andMapAux label_map1 label_map2 (l:ls) =
    let Just bool1 = Map.lookup l label_map1
        Just bool2 = Map.lookup l label_map2
        f _ = Just (bool1 && bool2)
        new_map = Map.update f l label_map1
    in andMapAux new_map label_map2 ls
    
checkCTLauxEX :: Automata -> [State] ->  Map.Map Int Bool -> Map.Map Int Bool -> Map.Map Int Bool
checkCTLauxEX tom [] label_map marked_map = marked_map
checkCTLauxEX tom (l:ls) label_map marked_map =
    let connected = toList (getOutgoingStates tom l)
        connected_map = Map.filterWithKey (\k _ -> (elem k connected)) label_map
        new_bool = or (Map.elems connected_map)
        f _ = Just new_bool
        new_map = Map.update f l marked_map -- es alter?
    in checkCTLauxEX tom ls label_map new_map

checkCTLauxEU :: Automata ->  Map.Map Int Bool -> Map.Map Int Bool -> [State] -> Map.Map Int Bool -> Map.Map Int Bool
checkCTLauxEU tom label_map seenbefore_map [] sublabel = label_map
checkCTLauxEU tom label_map seenbefore_map (k:ks) sublabel = 
    let previous_states = toList (getIncomingStates tom k)
        f _ = Just True
        new_map = Map.update f k label_map
        (added_previous,new_seenbefore_map) = checkEUprevious seenbefore_map previous_states sublabel ks
    in checkCTLauxEU tom new_map new_seenbefore_map added_previous sublabel
        
        
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

checkCTLauxAU :: Automata ->  Map.Map Int Bool -> Map.Map Int Int -> [State] ->  Map.Map Int Bool ->  Map.Map Int Bool 
checkCTLauxAU tom label_map degree_map (l:ls) sublabel =
    let previous_states = toList (getIncomingStates tom l)
        f _ = Just True
        new_map = Map.update f l label_map
        (added_previous,new_degree_map) = checkAUprevious new_map degree_map previous_states sublabel ls
    in checkCTLauxAU tom new_map new_degree_map added_previous sublabel
        
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
