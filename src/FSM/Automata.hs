
-- Import packages -----------------------------------------

module FSM.Automata (

    Automata,
    -- * Creating functions
    createAutomata,
    
    -- * Accesing functions
    getStates,
    getAcceptingStates,
    getInitialState,
    getInputs,
    getAssociations,
    getTransitions,
    getHoles,
    
    -- * Checking functions
    validInput,
    
    -- * Action functions
    
    performAction,
    -- * Editing functions
    addState,
    deleteState,
    changeInitialState,
    addAcceptingState
     

) where

import Data.Set 
import qualified Data.List   as L
import qualified Data.Matrix as M
import qualified Data.Vector as V

-- Custom show functions ---------------------------------------

showIntSet :: [Int] -> Int -> String
showIntSet [l] 0 = id "{" ++ show l ++ id "}"
showIntSet [l] _ = show l ++ id "}"
showIntSet (l:ls) 0 = id "{" ++ show l ++ id ","++ showIntSet ls 1
showIntSet (l:ls) k = show l ++ id ","++ showIntSet ls k

showCharSet :: [Char] -> Int -> String
showCharSet [l] 0 = id "{" ++ show l ++ id "}"
showCharSet [l] _ = show l ++ id "}"
showCharSet (l:ls) 0 = id "{" ++ show l ++ id ","++ showCharSet ls 1
showCharSet (l:ls) k = show l ++ id ","++ showCharSet ls k
    
-- Create data types -----------------------------------------

data Automata = A (Set Int,Set Char,Int,M.Matrix Int,Set Int,Int)
                --deriving Show
                
instance Show Automata where
    show (A (s,i,s0,m,a,c)) = 
        (id "\n" ++ id "Set of states:" ++ id "\n" ++ s' ++ id "\n" ) ++
        (id "\n" ++ id "Set of inputs (language):" ++ id "\n"  ++ i' ++ id "\n" ) ++
        (id "\n" ++ id "Initial state:" ++ id "\n" ++ show s0 ++ id "\n" ) ++
        (id "\n" ++ id "Matrix of associations:" ++ id "\n" ++ show m ++ id "\n" ) ++
        (id "\n" ++ id "Set of accepting states:" ++ id "\n" ++ a') ++ 
        (id "\n" ++ id "Current state:" ++ id "\n" ++ show c ++ id "\n") ++ id "\n" 
        where s' = showIntSet (toList s) 0
              i' = showCharSet (toList i) 0
              a' = showIntSet (toList a) 0

-- Creating functions -----------------------------------------

-- | This is the main function for creating the Automata abstract data type. By default, the inital state and the current state of the automata are the same.
--
--  Please pay attention to how the object is built. E.g.,
--
-- > createAutomata s i s0 m a c0
--  where:
--
-- -s is the number of states of the automata.
-- -i is the language the automata accepts.
-- -s0 is the initial state of the automata.
-- -m is the matrix of associations of the automata. (Details here: 'getAssociations')
-- -a is the list of accepting states of the automata.
-- -c0 is the placeholder for the current state. If it's the first time defining this Automata, leave it as c0 = s0
--
-- More specifically you could
--
-- > import qualified Data.Matrix as M
-- > mat = M.fromLists [[2,0,0,0],[2,1,4,0],[1,4,0,0],[0,0,0,3]]
-- > tom = createAutomata 4 ['a', 'b', 'c', 'd'] 1 mat [4] 1


createAutomata :: Int -> String -> Int -> M.Matrix Int -> [Int] -> Int -> Automata
createAutomata s i s0 m a c0
    | s < 1 = 
        error "Number of states must be greater than 1"
    | not (member s0 s') =
        error "Not valid initial state"
    | not ((M.nrows m,M.ncols m) == (size s',size i')) =
        error "Not valid matrix size"
    | not (isSubsetOf (delete 0 (fromList (M.toList m))) s') =
        error "Not valid matrix elems"
    | not (isSubsetOf a' s') =
        error "Not valid accepting states"
    | otherwise = A (s',i',s0,m,a',c0)
      where s' = fromList [1..s]
            i' = fromList (L.sort i)
            a' = fromList (L.sort a)



-- Accessing functions ----------------------------------------- 
 
 
-- | This function returns the set of states of the automata. It is really of not much use since the generation of the automata only needs the number of states and not the whole set of them, but just in case you want to check which states does the current automata have. 
getStates :: Automata -> Set Int 
getStates t = s
    where A (s,i,s0,m,a,c) = t

-- | This function returns the list of accepting states of the automata. It is a list and not a set for coherence purpouses. When you build the automata you have to give a list of accepting states so I though it made sense to also return a list of accepting states as the accessing function.
getAcceptingStates :: Automata -> [Int]
getAcceptingStates t = a'
    where A (s,i,s0,m,a,c) = t     
          a' = toList a

-- | This function returns the current initial state of the automata.
getInitialState :: Automata -> Int 
getInitialState t = s0
    where A (s,i,s0,m,a,c) = t 

-- | This function returns the string of inputs that the automata accepts.    
getInputs :: Automata -> String
getInputs t = toList i
    where A (s,i,s0,m,a,c) = t 
          
-- | This function returns the associations matrix of the automata.  This matrix is built according to the following rules:
--
-- 1. The columns of the matrix represent the inputs of the language that the automata accepts in <https://en.wikipedia.org/wiki/Lexicographical_order lexicographical order>.
-- 2. The rows of the matrix represent the states of the automata in ascending order.
-- 3. The element \(a_{ij} = k \) means that the state  \(i\) is connected to the state  \(k\) thanks to the input that the column  \(j\)  of the matrix represents.
--
-- More info can be found here: <https://en.wikipedia.org/wiki/State-transition_table Wikipedia: State-transition table>
--
-- Continuing with the previous example, the following matrix correspongs to the automata in the figure.
--
-- > mat = M.fromLists [[2,0,0,0],[2,1,4,0],[1,4,0,0],[0,0,0,3]]
-- > tom = createAutomata 4 ['a', 'b', 'c', 'd'] 1 mat [4] 1
--
-- The code above represent this matrix: 
--
-- >     'a' 'b' 'c' 'd'         <= inputs
-- >   ------------------
-- > 1 |  2   0   0   0 
-- > 2 |  2   1   4   0  
-- > 3 |  1   4   0   0 
-- > 4 |  0   0   0   3  
-- > 
-- > ^
-- > |
-- > states
-- 
-- And the matrix above represents the transitions in the following automata:
--
-- <<https://i.imgur.com/ymWLlsb.png Tom automata figure>>
{-
--
-- +-----------+------------+----------+----------+----------+
-- |           | 'a'        | 'b'      | 'c'      | 'd'      |  
-- +-----------+------------+----------+----------+----------+
-- | 1         |  \[                                         |
-- +-----------+    \begin{matrix}                           |
-- | 2         |                                             |
-- +-----------+                                             |
-- | 3         |                                             |
-- +-----------+                                             |
-- | 4         |    2 & 0 & 0 & 0 \\                         |
-- |           |    2 & 1 & 4 & 0 \\                         |
-- |           |    1 & 4 & 0 & 0 \\                         |
-- |           |    0 & 0 & 0 & 3                            |
-- |           |    \end{matrix}                             |
-- |           |                                             |
-- |           |    \]                                       |
-- +-----------+------------+----------+----------+----------+
--
-}

getAssociations :: Automata -> M.Matrix Int
getAssociations t = m
    where A (s,i,s0,m,a,c) = t
         
-- | This function returns the inputs that a state accepts for transitioning into another state.
--
getTransitions :: Automata -> Int -> Set Char
getTransitions t k 
    | not (member k s) = error "Not a valid state"
    | otherwise = fromList l
    where m = getAssociations t
          i = getInputs t
          s = getStates t
          row = V.toList (M.getRow k m)
          l = [ a | (a,k) <- zip i row, k /= 0]

-- | This function returns the current state in which the automata currently is.
--
getCurrentState :: Automata -> Int
getCurrentState t = c
    where A (s,i,s0,m,a,c) = t 
          
-- | This function returns the states you can possibly reach from a given state.
--
getOutgoingStates :: Automata -> Int -> Set Int
getOutgoingStates t k
    | not (member k s) = error "Not a valid state"
    | otherwise = fromList l
    where m = getAssociations t
          s = getStates t
          row = V.toList (M.getRow k m)
          l = [ p | p <- row, p /= 0]
          
-- | This function returns the states that can possibly reach a given state.
--
getIncomingStates :: Automata -> Int -> Set Int
getIncomingStates t k
    | not (member k s) = error "Not a valid state"
    | otherwise = fromList l
    where m = getAssociations t
          s = getStates t
          rows = [(n,member k (fromList (V.toList (M.getRow n m)))) | n <- [1..(M.nrows m)]]
          l = [n | (n,bool) <- rows, bool == True]         
          
-- | This function returns those states of the automata that do not have any input to any other state, i.e., once that a 'hole' state is reached, none of the rest of state can be reached anymore for the current execution.
getHoles :: Automata -> Set Int
getHoles t = fromList hs
    where A (s,i,s0,m,a,c) = t 
          hs = [n | n <- toList s,
                and [n == (M.getRow n m)V.!k || (M.getRow n m)V.!k == 0 | k <- [0..((size i)-1)]]]

-- | This function returns the states of the given automata that cannot be reached.
--
getIsolated :: Automata -> Set Int
getIsolated t = fromList l
    where s = getStates t
          l = [ p | p <- toList s, getIncomingStates t p == empty]


-- Checking functions -----------------------------------------

validInputAux :: String -> Automata -> Int -> Bool
validInputAux str a k
    | not (isSubsetOf (fromList str) i) = error "Invalid input"
    | elem k h && member k ac = True
    | elem k h && not (member k ac) = False
    | L.null str && member k ac = True
    | L.null str && not (member k ac) = False
    | not (member st (getTransitions a k)) =  error ("Not valid input "  ++ (show st) ++ " for state " ++ (show k) )
    | otherwise = validInputAux (tail str) a k'
    where s = getStates a
          i = fromList (getInputs a)
          s0 = getInitialState a
          m = getAssociations a
          ac = fromList (getAcceptingStates a)
          h = getHoles a
          st = head str
          k' = M.getElem k ((findIndex st i)+1) m
         



-- | This function test if a string is @/valid/@, i.e., if when the automata receives the string, ends in one of the accepting states.
validInput :: String -> Automata -> Bool
validInput str a = validInputAux str a s0
    where s0 = getInitialState a

-- Action functions

-- | This funcion perform the given transition from the current state and changes to a new current state.
performAction :: Automata -> Char -> Automata
performAction t char
    | not (member char i) = error ("This is not one of the valid inputs. Please try again with one of the following options: " ++ ts')
    | not (member char ts) = error ("This is not one of the valid inputs for the current state. Please try again with one of the following options: " ++ ts')
    | otherwise = changeCurrentState t c'
        where i = fromList (getInputs t)
              c = getCurrentState t
              ts = getTransitions t c
              ts' = showCharSet (toList ts) 0
              m = getAssociations t
              n = (findIndex char i) + 1
              c' = M.getElem c n m

-- Editing functions -----------------------------------------


-- | Function for adding a state to an Automata with the list of associations to the other states. If you would want to add a non-connected state, simply enter the list [0,..,0], with as many zeros as possible inputs.
addState :: Automata -> [Int] -> Automata
addState a ls 
    | L.length ls /= L.length (getInputs a) = error ( "Not a valid list of associations" ) 
    | otherwise = createAutomata s i s0 m t c
    where s = (M.nrows (getAssociations a)) +1
          i = getInputs a
          s0 = getInitialState a
          t = getAcceptingStates a
          c = getCurrentState a
          m = M.fromLists ((M.toLists (getAssociations a))++[ls])

dropElemAtIndex :: Int -> [[Int]] -> [[Int]]
dropElemAtIndex i ls = L.take (i-1) ls ++ L.drop i ls

-- | This function deletes a state and all the connections it has with any other state. Please note that this function automatically reassigns new numbers for the remaining states, so the states and the associations matrix change accordingly. E.g. if you delete in the previous automata the 3rd state, then since the new automata has just 3 states, the old 4th state becomes the new 3rd state.
deleteState :: Automata -> Int ->Automata
deleteState a i 
    | not (elem i (getStates a)) = error ( "This state is not one of the states of the automata." )
    | (getInitialState a) == i = error ( "You are trying to delete the initial state. If you want to perform this action, first change the initial state and then delete the old one.")
    | elem i (fromList (getAcceptingStates a)) && L.length (getAcceptingStates a) == 1 = error ("You are trying to delete the only accepting state.")
    | i == c = error ( "You are trying to delete the current state. If you want to perform this action, first change the current state and then delete the old one.")
    | otherwise = createAutomata s i' s0' m t c
    where s = (M.nrows (getAssociations a)) -1
          i' = getInputs a
          c = getCurrentState a
          s0 = getInitialState a
          s0' = if s0 < i then s0 else s0-1
          t = [if l < i then l else l-1 | l <- toList ((fromList (getAcceptingStates a)) `difference` singleton i)]
          rows = M.toLists (getAssociations a)
          rows_deleted = dropElemAtIndex i ([[if l < i 
                                              then l
                                              else if l > i
                                                   then l-1
                                                   else 0 | l <- ls] | ls <- rows])
          m = M.fromLists rows_deleted

-- | This function changes the initial state.
changeInitialState :: Automata -> Int -> Automata
changeInitialState t s0' 
    | not (elem s0' (getStates t)) = error ( "This state is not one of the states of the automata." )
    | (getInitialState t) == s0' = error ( "State " ++ show s0' ++ " is already the initial state.")
    | otherwise = createAutomata s' i' s0' m a c
        where a = getAcceptingStates t 
              c = getCurrentState t
              s' = size (getStates t)
              i' = getInputs t
              m = getAssociations t

-- | This function changes the current state.
changeCurrentState :: Automata -> Int -> Automata
changeCurrentState t c'
    | not (elem c' (getStates t)) = error ( "This state is not one of the states of the automata." )
    | (getCurrentState t) == c' = error ( "State " ++ show c' ++ " is already the current state.")
    | otherwise = createAutomata s' i' s0 m a c
        where a = getAcceptingStates t 
              s0 = getInitialState t
              c = getCurrentState t
              s' = size (getStates t)
              i' = getInputs t
              m = getAssociations t 
    
-- | This function adds one accepting state
addAcceptingState :: Automata -> Int -> Automata
addAcceptingState t a0
    | not (elem a0 (getStates t)) = error ( "This state is not one of the states of the automata." )
    | elem a0 (getAcceptingStates t)  = error ( "State " ++ show a0 ++ " is already one of the accepting states.")
    | otherwise = createAutomata s' i' s0 m a' c
    where a = getAcceptingStates t
          c = getCurrentState t
          a' = a ++ [a0]
          s' = size (getStates t)
          i' = getInputs t
          m = getAssociations t
          s0 = getInitialState t
          
    
