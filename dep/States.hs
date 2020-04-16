{-# LANGUAGE GADTs #-}

-- Import packages -----------------------------------------

module FSM.States (
    --StatesContents

) where
    
    
import FSM.Automata
import qualified Data.Map as SM
import qualified Data.Matrix as M
import Data.Typeable



--data StateContent = SC1 [Char] deriving Show -- state_things = SM.fromList [("tag_of_the_thing",thing)]
--data StatesContents a = SCS (SM.Map Int (SC a)) --SM.fromList [(state_number,state_things)]

concatString :: [String] -> String
concatString [l] = l
concatString (l:ls) = l ++ concatString ls

properShow a 
    | show (typeOf a) == "Int" = show a
    | show (typeOf a) == "[Char]" = id a
    | otherwise = show a
    
--showTagMap :: SM.Map [Char] [Char] -> String
showTagMap dict = list2
    where list = [id tag ++ properShow value | (tag,value) <- zip (SM.keys dict) (SM.elems dict)]
          list2 = concatString list

--showStateMap :: [Int] -> SM.Map Int (SM.Map [Char] a) -> String
showStateMap [l] b = id "Variables in the state " ++show l ++ id ":" ++ id "\n" ++ (showTagMap key_map) 
    where (Just key_map) = SM.lookup l b
showStateMap (l:ls) b = id "Variables in the state " ++show l ++ id ":" ++ id "\n" ++ (showTagMap key_map) ++ showStateMap ls b
    where (Just key_map) = SM.lookup l b


data StatesContents a = SCS (SM.Map Int (SM.Map String a)) --deriving Show

instance Show (StatesContents a) where
    show (SCS b) = 
        (id "\n" ++ output)
        where states = SM.keys b
              output = showStateMap states b
              
              
tag_map = SM.singleton "test" "hola"
int_map = SM.singleton 2 tag_map
--test = SCS int_map

{-

createStateContents :: Automata -> Int -> String -> a -> StatesContents a 
createStateContents t = SCS int_map
    where tag_map = SM.singleton tag thing
          int_map = SM.singleton k tag_map

updateStateContents :: Automata -> Int -> String -> a -> StatesContents a 
updateStateContents t k tag thing = SCS int_map
    where tag_map = SM.singleton tag thing
          int_map = SM.singleton k tag_map
          
          -}
mat = M.fromLists [[2,0,0,0],[2,1,4,0],[1,4,0,0],[0,0,0,3]]
tom = createAutomata 4 ['a', 'b', 'c', 'd'] 1 mat [4] 1

