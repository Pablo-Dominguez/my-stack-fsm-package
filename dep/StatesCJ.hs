{-# LANGUAGE GADTs #-}

-- Import packages -----------------------------------------

module FSM.StatesCJ (
    --StatesContents

) where

-- import FSM.Automata
import qualified Data.Map as Map
import Data.Dynamic
import qualified Data.Matrix as M
import Data.Maybe (fromMaybe)

-- type AutomataWithInfo a = (Automata, AutomataInfo a)
-- or
-- data AutomataWithInfo a = {getAutomata :: Automata,
--                            getInfo     :: AutomataInfo a}

createStateContents :: State -> String -> a -> AutomataInfo a
createStateContents s k v = AutomataInfo . Map.singleton s $ Map.singleton k v
-- sc = createStateContents 3 "tag" 45


alterStateInfo :: (Maybe a -> Maybe a) -> String -> StateInfo a -> StateInfo a
alterStateInfo = Map.alter

-- Actualiza el valor de la etiqueta k asociada al estado s.
-- Si la etiqueta k y el estado s existen, el valor será f (Just k)
-- Si la etiqueta k no está definida para el estado s (o el estado s está vacío o no existe),
-- el valor será f Nothing.
-- Un resultado Nothing indicará que se desea eliminar la etiqueta,
-- mientras que un Just y asignará el valor y.
alterAutomataInfo :: (Maybe a -> Maybe a) -> State -> String -> AutomataInfo a -> AutomataInfo a
alterAutomataInfo f s k a = AutomataInfo $ Map.alter f' s (toMap a)
  where f' x = if Map.null (resultado x) -- Si el estado resultante es vacío, lo eliminamos de la información
               then Nothing
               else Just (resultado x)
        resultado x = Map.alter f k (fromMaybe Map.empty x)

-- Cambia el valor de la etiqueta s[k] a y (si y == Just v) o la elimina (si es Nothing)
updateStateContents :: State -> String -> Maybe a -> AutomataInfo a -> AutomataInfo a
updateStateContents s k v a = alterAutomataInfo (const v) s k a
-- updateStateContents 4 "tag" (Just 3) sc

mat = M.fromLists [[2,0,0,0],[2,1,4,0],[1,4,0,0],[0,0,0,3]]
-- tom = createAutomata 4 ['a', 'b', 'c', 'd'] 1 mat [4] 1

prettyShow :: (Show k, Show v) => Map.Map k v -> String
prettyShow = show . Map.toList

type State = Integer
type StateInfo = Map.Map String

-- verboseShowStateInfo :: Show a => StateInfo a -> String
-- verboseShowStateInfo = nicePrint . Map.toAscList
--   where nicePrint ss = concatMap (\(k, v) -> k ++ ": " ++ show v ++ "\n") ss
verboseShowStateInfo :: Show a => StateInfo a -> String
verboseShowStateInfo = concatMap formatter . Map.toAscList
  where formatter (k, v) = concat ["-->",k, ": ","\n", show v, "\n"]


-- verboseShow :: Show a => Map.Map State (StateInfo a) -> String
-- verboseShow xs = verboseShowPair $ Map.toAscList xs
--   where verboseShowPair ss = concatMap (\(s,i) -> "The elements in state " ++ show s ++ " are:\n" ++ verboseShowStateInfo i ++ "\n") ss
verboseShow :: Show a => Map.Map State (StateInfo a) -> String
verboseShow = concatMap formatter . Map.toAscList
  where formatter (s, i) = concat ["=> The elements in state ", show s, " are:\n", verboseShowStateInfo i, "\n"]


newtype AutomataInfo a = AutomataInfo { toMap :: Map.Map State (StateInfo a)}

instance Show a => Show (AutomataInfo a) where
  show = verboseShow . toMap


mm :: Map.Map State (StateInfo String)
mm = Map.fromList [(1,Map.fromList [("tag11","value11"),("tag12","value12")]),(2,Map.fromList [("tag21","value21"),("tag22","value22")])]

aim :: AutomataInfo String
aim = AutomataInfo mm
