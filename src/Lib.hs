
-- Import packages -----------------------------------------

module Lib (

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
    
    
{-   
type States = Set
type Inputs = Set
type Start = Set
type Delta = M.Matrix
type Goal = Set

type Automata = (States,Inputs,Start,Delta,Goal)
type Automata a b d e = A (Set a,Set b,Int,M.Matrix d,Set e)
-}

--type no me deja usar Show

-- Create data types -----------------------------------------

data Automata = A (Set Int,Set Char,Int,M.Matrix Int,Set Int)
                deriving Show
                
{-
showSet :: Set Int -> String
showSet s =
    "{"++(L.intersperse ','
               [head (show (elemAt t s)) | t <- [0..((size s)-1)]])++"}"
    -}
-- No se me ocurre como implementarlo, pero estaría guay
-- Ej: showSet (fromList [2..5])

-- Creating functions -----------------------------------------

-- | This is the main function for creating the Automata abstract data type. 
--
--  Please pay attention to how the object is built. E.g.,
--
-- > createAutomata s i s0 m a
--  where:
--
-- -s is the number of states of the automata.
-- -i is the language the automata accepts.
-- -s0 is the initial state of the automata.
-- -m is the matrix of associations of the automata. (Details here: 'getAssociations')
-- -a is the list of accepting states of the automata.
--
-- More specifically you could
--
-- > import qualified Data.Matrix as M
-- > mat = M.fromLists [[2,0,0,0],[2,1,4,0],[1,4,0,0],[0,0,0,3]]
-- > tom = createAutomata 4 ['a', 'b', 'c', 'd'] 1 mat [4]


createAutomata :: Int -> String -> Int -> M.Matrix Int -> [Int] -> Automata
createAutomata s i s0 m a
    | not (member s0 s') =
        error "Not valid initial state"
    | not ((M.nrows m,M.ncols m) == (size s',size i')) =
        error "Not valid matrix size"
    | not (isSubsetOf (delete 0 (fromList (M.toList m))) s') =
        error "Not valid matrix elems"
    | not (isSubsetOf a' s') =
        error "Not valid accepting states"
    | otherwise = A (s',i',s0,m,a')
      where s' = fromList [1..s]
            i' = fromList (L.sort i)
            a' = fromList (L.sort a)
{-
s: número de estados
i: lista del lenguaje
s0: estado inicial
m: matriz de asociaciones, filas == estados, columnas ==  inputs
a: subconjunto de estados de aceptación (lista)

matriz = M.fromLists [[2,0,0,0],[2,1,4,0],[1,4,0,0],[0,0,0,3]]
createAutomata 4 ['a', 'b', 'c', 'd'] 1 matriz [4]


-}
            
            
-- Nota: los he ordenado porque me va a hacer falta a la hora de usar
-- validInput en la matriz (getIndex)
-- Nota : He reemplazado creaAutomata :: [Int] por creaAutomata :: Int,
-- para que sólo haya que dar el número de estados (y se numeren de forma
-- automática)
-- Nota : not (isSubsetOf (insert 0 (fromList (M.toList m))) (insert 0 s'))
-- Ej: creaAutomata [1..3] "abc" 2 (M.fromList 3 3 [1,2,3,1,2,3,1,2,3]) [1]
                 
-- Funciones de acceso

{-  Alternativa:

get_ (A (s,i,s0,m,a)) = s
 
 -}

-- Accessing functions ----------------------------------------- 
 
 
-- | This function returns the set of states of the automata. It is really of not much use since the generation of the automata only needs the number of states and not the whole set of them, but just in case you want to check which states does the current automata have. 
getStates :: Automata -> Set Int 
getStates t = s
    where A (s,i,s0,m,a) = t

-- | This function returns the list of accepting states of the automata. It is a list and not a set for coherence purpouses. When you build the automata you have to give a list of accepting states so I though it made sense to also return a list of accepting states as the accessing function.
getAcceptingStates :: Automata -> [Int]
getAcceptingStates t = a'
    where A (s,i,s0,m,a) = t     
          a' = toList a

-- | This function returns the current initial state of the automata.
getInitialState :: Automata -> Int 
getInitialState t = s0
    where A (s,i,s0,m,a) = t 

-- | This function returns the current initial state of the automata.    
getInputs :: Automata -> Set Char 
getInputs t = i
    where A (s,i,s0,m,a) = t 
          
-- | This function returns the associations matrix of the automata.  This matrix is built according to the following rules:
--
-- 1. The columns of the matrix represent the inputs of the language that the automata accepts in lexicographical order.
-- 2. The rows of the matrix represent the states of the automata in ascending order.
-- 3. The element \(a_{ij} = k \) means that the state  \(i\) is connected to the state  \(k\) thanks to the input that the column  \(j\)  of the matrix represents.
--
-- Continuing with the previous example, the following matrix correspongs to the automata in the figure.
--
-- > mat = M.fromLists [[2,0,0,0],[2,1,4,0],[1,4,0,0],[0,0,0,3]]
-- > tom = createAutomata 4 ['a', 'b', 'c', 'd'] 1 mat [4]
--
-- The code above represent this matrix: 
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
-- And the matrix above represents the transitions in the following automata:
--
--

getAssociations :: Automata -> M.Matrix Int
getAssociations t = m
    where A (s,i,s0,m,a) = t
         
getTransitions :: Automata -> Int -> Set Char
getTransitions t k 
    | not (member k s) = error "Not a valid state"
    | otherwise = l
    where m = getAssociations t
          i = getInputs t
          s = getStates t
          row = V.toList (M.getRow k m)
          l = fromList [ a | (a,k) <- zip (toList i) row, k /= 0]

    
getHoles :: Automata -> Set Int
getHoles t = fromList hs
    where A (s,i,s0,m,a) = t 
          hs = [n | n <- toList s,
                and [n == (M.getRow n m)V.!k || (M.getRow n m)V.!k == 0 | k <- [0..((size i)-1)]]]
         
-- getHoles devuelve los estados de los que no parte ninguna arista, i.e. aquellos en los que la matriz tiene en su fila todos los elementos iguales al índice de la fila o nulos
-- revisar
               
-- Quiero añadir una pequeña descripción con instrucciónes sobre
-- la librería al principio, y que haciendo help o algo así se pueda leer
 
-- nota: las columnas de la 'matriz de adyacencia' (posibles inputs)
-- deben estar en orden lexicográfico (ver L.sort)
{-

Ej: 4 posibles imputs, 3 estados:

        | '1' | '3' | 'i' | 'j'      <- inputs
        -------------------------
      1 |  0  |  2  |  3  |  0
      2 |  1  |  0  |  0  |  3       <- si uso la forma de la matriz 
      3 |  1  |  1  |  0  |  2          "con ceros", no puede haber n 
                                        en la fila n-esima
      ^
      |
     estados

 
-}
--Input checking

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
          i = getInputs a
          s0 = getInitialState a
          m = getAssociations a
          ac = fromList (getAcceptingStates a)
          h = getHoles a
          st = head str
          k' = M.getElem k ((findIndex st i)+1) m
         
         
-- Duda: debería incluir los estados, la matriz y to eso como argumentos
-- para ahorrar memoria, o no influye mucho? (está haciendo el where y
-- llamando a las funciones get por cada iteración)

validInput :: String -> Automata -> Bool
validInput str a = validInputAux str a s0
    where s0 = getInitialState a

-- valid input "baac" tomatito falla

-- OPCIONES: o en la matriz hay 0 y en cada fila n no aparece el mismo n,
-- o bien no hay ceros y si puede aparecer el propio n 


-- 2 Dudas (modelizar la matriz 

-- Editing functions -----------------------------------------


-- Function for adding a state to an Automata from the list of
-- associations
addState :: Automata -> [Int] -> Automata
addState a ls 
    | L.length ls /= size (getInputs a) = error ( "Not a valid list of associations" ) 
    | otherwise = createAutomata s i s0 m t
    where s = (M.nrows (getAssociations a)) +1
          i = toList (getInputs a)
          s0 = getInitialState a
          t = getAcceptingStates a
          m = M.fromLists ((M.toLists (getAssociations a))++[ls])

dropElemAtIndex :: Int -> [[Int]] -> [[Int]]
dropElemAtIndex i ls = L.take (i-1) ls ++ L.drop i ls
          
deleteState :: Automata -> Int ->Automata
deleteState a i 
    | not (elem i (getStates a)) = error ( "This state is not one of the states of the automata." )
    | (getInitialState a) == i = error ( "You are trying to delete the initial state. If you want to perform this action, first change the initial state and then delete the old one.")
    | elem i (fromList (getAcceptingStates a)) && L.length (getAcceptingStates a) == 1 = error ("You are trying to delete the only accepting state.")
    | otherwise = createAutomata s i' s0' m t
    where s = (M.nrows (getAssociations a)) -1
          i' = toList (getInputs a)
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

changeInitialState :: Automata -> Int -> Automata
changeInitialState t s0' 
    | not (elem s0' (getStates t)) = error ( "This state is not one of the states of the automata." )
    | (getInitialState t) == s0' = error ( "State " ++ show s0' ++ " is already the initial state.")
    | otherwise = createAutomata s' i' s0' m a
        where a = getAcceptingStates t 
              s' = size (getStates t)
              i' = toList (getInputs t)
              m = getAssociations t


addAcceptingState :: Automata -> Int -> Automata
addAcceptingState t a0
    | not (elem a0 (getStates t)) = error ( "This state is not one of the states of the automata." )
    | elem a0 (getAcceptingStates t)  = error ( "State " ++ show a0 ++ " is already one of the accepting states.")
    | otherwise = createAutomata s' i' s0 m a'
    where a = getAcceptingStates t 
          a' = a ++ [a0]
          s' = size (getStates t)
          i' = toList (getInputs t)
          m = getAssociations t
          s0 = getInitialState t
          
    
