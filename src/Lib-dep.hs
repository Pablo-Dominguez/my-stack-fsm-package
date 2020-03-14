{-module Lib
    ( someFunc
    ) where
-}

    


module Lib where

import Data.Set 
import qualified Data.List   as L
import qualified Data.Matrix as M
import qualified Data.Vector as V
    
someFunc :: IO ()
someFunc = putStrLn "someFunc"

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
    
creaAutomata :: Int -> String -> Int -> M.Matrix Int -> [Int] -> Automata
creaAutomata s i s0 m a
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
creaAutomata 4 ['a', 'b', 'c', 'd'] 1 matriz [4]


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

getStates :: Automata -> Set Int 
getStates t = s
    where A (s,i,s0,m,a) = t

getAcceptingStates :: Automata -> Set Int 
getAcceptingStates t = a
    where A (s,i,s0,m,a) = t                           
                           
getInitialState :: Automata -> Int 
getInitialState t = s0
    where A (s,i,s0,m,a) = t 
                           
getInputs :: Automata -> Set Char 
getInputs t = i
    where A (s,i,s0,m,a) = t 

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
          ac = getAcceptingStates a
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
