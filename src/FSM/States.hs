

module FSM.States (
    AutomataInfo,
    createStateInfo,
    getStatesWithInfo,
    getTagsInState,
    getInfoInState,
    alterStateInfo
    

) where

import qualified Data.Map as Map   


type State = Int
type Tag = String


newtype StateInfo a = StateInfo {tagMap :: Map.Map Tag a} deriving Eq
newtype AutomataInfo a = AutomataInfo { toMap :: Map.Map State (StateInfo a)} deriving Eq

--data UserStateInfo = UserStateInfo { rate :: Float } deriving Show

verboseShowStateInfo :: Show a => Map.Map Tag a -> String
verboseShowStateInfo = concatMap formatter . Map.toAscList
  where formatter (k, v) = concat ["--> [tag] ",k, ": ","\n", show v, "\n"]
        
instance Show a => Show (StateInfo a) where
  show = verboseShowStateInfo . tagMap        
  
verboseShow :: Show a => Map.Map State (StateInfo a) -> String
verboseShow = concatMap formatter . Map.toAscList
  where formatter (s, i) = concat ["=> The elements in state ", show s, " are:\n", verboseShowStateInfo (tagMap i), "\n"]


instance Show a => Show (AutomataInfo a) where
  show = verboseShow . toMap 
  


  
createStateInfo :: State -> Tag -> a -> AutomataInfo a
createStateInfo state tag k = AutomataInfo {toMap = Map.singleton state (StateInfo {tagMap = Map.singleton tag k})}


getStatesWithInfo :: AutomataInfo a -> [State]
getStatesWithInfo (AutomataInfo k) = Map.keys k

getTagsInState :: AutomataInfo a -> State -> [Tag]
getTagsInState (AutomataInfo k) n
    | not (elem n (getStatesWithInfo (AutomataInfo k))) = error ("This state does not contain info.")
    | otherwise = Map.keys (tagMap state_map)
    where (Just state_map) = Map.lookup n k


getInfoInState :: AutomataInfo a -> State -> (Maybe Tag) -> StateInfo a
getInfoInState (AutomataInfo k) n Nothing
    | not (elem n (getStatesWithInfo (AutomataInfo k))) = error ("This state does not contain info.")
    | otherwise = state_map
    where (Just state_map) = Map.lookup n k
getInfoInState (AutomataInfo k) n (Just tag)
    | not (elem n (getStatesWithInfo (AutomataInfo k))) = error ("This state does not contain info.")
    | not (elem tag (getTagsInState (AutomataInfo k) n)) = error ("This state does not contain the given tag.")
    | otherwise = output
    where (Just state_map) = Map.lookup n k
          tag_map = tagMap state_map
          Just tag_info = Map.lookup tag tag_map
          output = StateInfo {tagMap = Map.singleton tag tag_info}


alterStateInfo :: State -> Maybe Tag -> a -> AutomataInfo a -> AutomataInfo a
alterStateInfo state Nothing _ (AutomataInfo info)
    | not (elem state (getStatesWithInfo (AutomataInfo info))) = (AutomataInfo info)
        --error ("This state does not contain info.")
    | otherwise = AutomataInfo {toMap = Map.delete state info}
alterStateInfo state (Just tag) sinf (AutomataInfo info) 
    | elem state (getStatesWithInfo (AutomataInfo info)) = 
        let f _ = Just sinf
            (Just state_map) = Map.lookup state info
            tag_map = tagMap state_map
            new_tag_map = Map.alter f tag tag_map
            g _ = Just (StateInfo {tagMap = new_tag_map})
            new_state_map = Map.alter g state info
        in AutomataInfo {toMap = new_state_map}
    | otherwise = 
        let new_tag_map = StateInfo {tagMap = Map.singleton tag sinf}
            g _ = Just new_tag_map
            new_state_map = Map.alter g state info
        in AutomataInfo {toMap = new_state_map}

