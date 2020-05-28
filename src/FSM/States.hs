
-- |
--
-- = Considerations
--
-- One caveat you should always take into account when using this package is that without some data creation from the user, the use of this package is a bit restricted. This happens because the way it is built the package forbids you to use more than one type of information between states (or inside one), so to work around this, if you want to have multiple types of information inside states, do as follows:
--
-- @
--   data myCustomData = Type1 String | Type2 Int deriving (Show,Eq)
-- @
-- 
-- dont forget about the deriving because otherwise it will conflict with the functions in the package.
  


module FSM.States (
    State,
    Tag,
    StateInfo,
    AutomataInfo,
        
    -- * Creating functions
    createStateInfo,
    fromlsStateInfo,
        
    -- * Accessing functions
    getStateInfo,
    getStatesWithInfo,
    getTagsInState,
    getInfoInState,
    
    -- * Editing functions
    alterStateInfo,
    unionStateInfo
    

) where

import qualified Data.Map as Map   
import qualified Data.List as L

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

  
-- Creating functions ------------------------

-- | This function takes a State, a Tag and a value and creates an AutomataInfo object containing only the given State with the value and the tag associated to it.
-- E.g.:
--
-- > createStateInfo 4 "tag" 25
-- 
-- If you created your own data type, you can do as follows:
--
-- > my_info = createStateInfo 4 "tag" (Type2 Int)
--
createStateInfo :: State -> Tag -> a -> AutomataInfo a
createStateInfo state tag k = AutomataInfo {toMap = Map.singleton state (StateInfo {tagMap = Map.singleton tag k})}

-- | This function takes a State, a list of (Tag,value) and Maybe AutomataInfo and returns the AutomataInfo updated with the list of tags given. Please notice that if Nothing is given, it will return the created AutomataInfo while if a (Just AutomataInfo) object is given, it will update the tags in the given state.
-- E.g. (notice that we are using @my_info@ from the previous example)
--
-- > fromlsStateInfo 4 [("foo", Type1 "on"),("bar", Type2 0)] Nothing
-- > fromlsStateInfo 4 [("foo", Type1 "on"),("bar", Type2 0)] (Just my_info)
--
fromlsStateInfo :: Eq a => State -> [(Tag,a)] -> Maybe (AutomataInfo a) -> AutomataInfo a
fromlsStateInfo state (l:ls) Nothing 
    | ls /= [] = fromlsStateInfo state ls (Just first_info)
    | otherwise = first_info
    where (tag,value) = l
          first_info = createStateInfo state tag value
fromlsStateInfo state (l:ls) (Just (AutomataInfo info)) 
    | ls /= [] = fromlsStateInfo state ls (Just new_aut_info)
    | otherwise = new_aut_info
    where (tag,value) = l
          new_info = createStateInfo state tag value
          new_aut_info = unionStateInfo new_info (AutomataInfo info)
          
{-
-- quiero hacer una funcion que une la info con dos AutomataInfos (unión de conjuntos)
-- y otra funcion que a partir de un (State,[(Tag,a)]) crea el AutomataInfo
fromListAux :: Int -> [(State,[(Tag,a)])] -> AutomataInfo a
fromListAux 0 (l:ls) = 
    where (state,tag_list) = l
          


fromListStateInfo :: [(State,[(Tag,a)])] -> AutomataInfo a
fromListStateInfo ls = fromListAux 0 ls
-}

-- Accessing functions ------------------------

getStateInfo :: StateInfo a -> Map.Map Tag a
getStateInfo (StateInfo k) = k


-- | This function returns the states of the given AutomataInfo that currently contain some information
--
getStatesWithInfo :: AutomataInfo a -> [State]
getStatesWithInfo (AutomataInfo k) = Map.keys k

-- | This function returns the tags that a given state contains inside the AutomataInfo
--
getTagsInState :: AutomataInfo a -> State -> [Tag]
getTagsInState (AutomataInfo k) n
    | not (elem n (getStatesWithInfo (AutomataInfo k))) =  error ("This state does not contain info.")
    | otherwise = Map.keys (tagMap state_map)
    where (Just state_map) = Map.lookup n k


-- | This function returns the information contained in the given state. If @Nothing@ is given, then it returns all the information in the state while if @Just tag@ is given, it will return only the information inside the given tag.
-- E.g:
-- 
-- > getInfoInState my_info 4 Nothing
-- > getInfoInState my_info 4 (Just "foo")
--
getInfoInState :: AutomataInfo a -> State -> (Maybe Tag) -> StateInfo a
getInfoInState (AutomataInfo k) n Nothing
    | not (elem n (getStatesWithInfo (AutomataInfo k))) = StateInfo Map.empty
        --error ("This state does not contain info.")
    | otherwise = state_map
    where (Just state_map) = Map.lookup n k
getInfoInState (AutomataInfo k) n (Just tag)
    | not (elem n (getStatesWithInfo (AutomataInfo k))) = StateInfo Map.empty
       -- error ("This state does not contain info.")
    | not (elem tag (getTagsInState (AutomataInfo k) n)) = StateInfo Map.empty
        --error ("This state does not contain the given tag.")
    | otherwise = output
    where (Just state_map) = Map.lookup n k
          tag_map = tagMap state_map
          Just tag_info = Map.lookup tag tag_map
          output = StateInfo {tagMap = Map.singleton tag tag_info}

-- Editing functions ---------------

-- | This function takes a State, Maybe Tag, a value and an AutomataInfo object and updates the value of the Tag in the given State. Please note that if if Nothing is given, it will delete the State.
-- E.g:
--
-- > alterStateInfo 3 (Just "foo") (Type2 45) my_info
--
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

{-
emptyState :: State -> AutomataInfo a
emptyState state = AutomataInfo {toMap = Map.singleton state output}
        where output = StateInfo {tagMap = Map.empty}-}

-- unionStateAux :: AutomataInfo a -> AutomataInfo a -> AutomataInfo a -> [State] -> AutomataInfo a
-- unionStateAux (AutomataInfo info1) (AutomataInfo info2) (AutomataInfo output) [l] 
--     | (elem l (getStatesWithInfo (AutomataInfo info1))) && (not (elem l (getStatesWithInfo (AutomataInfo info2)))) = (AutomataInfo output)
--     | (not (elem l (getStatesWithInfo (AutomataInfo info1)))) && (elem l (getStatesWithInfo (AutomataInfo info2))) = 
--         let tag_map2 = tagMap (getInfoInState (AutomataInfo info2) l Nothing)
--             tag_output = StateInfo {tagMap = tag_map2}
--             state_output = Map.singleton l tag_output
--         in (AutomataInfo (Map.union state_output output))
--     | otherwise = 
--         let tag_map1 = tagMap (getInfoInState (AutomataInfo info1) l Nothing)
--             tag_map2 = tagMap (getInfoInState (AutomataInfo info2) l Nothing)
--             tag_output = StateInfo {tagMap = Map.union tag_map1 tag_map2}
--             state_output = Map.singleton l tag_output
--         in (AutomataInfo (Map.union state_output output))
unionStateAux (AutomataInfo info1) (AutomataInfo info2) (AutomataInfo output) [] = (AutomataInfo output)
unionStateAux (AutomataInfo info1) (AutomataInfo info2) (AutomataInfo output) (l:ls)
    | (elem l (getStatesWithInfo (AutomataInfo info1))) && (not (elem l (getStatesWithInfo (AutomataInfo info2)))) = unionStateAux (AutomataInfo info1) (AutomataInfo info2) (AutomataInfo output) ls
    | (not (elem l (getStatesWithInfo (AutomataInfo info1)))) && (elem l (getStatesWithInfo (AutomataInfo info2))) = 
        let tag_map2 = tagMap (getInfoInState (AutomataInfo info2) l Nothing)
            tag_output = StateInfo {tagMap = tag_map2}
            state_output = Map.singleton l tag_output
        in unionStateAux (AutomataInfo info1) (AutomataInfo info2) (AutomataInfo (Map.union state_output output)) ls
    | otherwise = 
        let tag_map1 = tagMap (getInfoInState (AutomataInfo info1) l Nothing)
            tag_map2 = tagMap (getInfoInState (AutomataInfo info2) l Nothing)
            tag_output = StateInfo {tagMap = Map.union tag_map1 tag_map2}
            state_output = Map.singleton l tag_output
        in unionStateAux (AutomataInfo info1) (AutomataInfo info2) (AutomataInfo (Map.union state_output output)) ls
            
-- | This function takes the left-biased union of t1 and t2. It prefers t1 when duplicate keys are encountered. Works similarly to <http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Map-Strict.html#g:12 Data.Map.union>.
--
{-unionStateInfo :: AutomataInfo a -> AutomataInfo a -> AutomataInfo a
unionStateInfo (AutomataInfo info1) (AutomataInfo info2) = unionStateAux (AutomataInfo info1) (AutomataInfo info2) (AutomataInfo info1) ls
    where ls = L.union (getStatesWithInfo (AutomataInfo info1)) (getStatesWithInfo (AutomataInfo info2))-} 
          
unionStateInfo :: AutomataInfo a -> AutomataInfo a -> AutomataInfo a
unionStateInfo (AutomataInfo info1) (AutomataInfo info2) =
  AutomataInfo (Map.unionWith (\ (StateInfo sti1) (StateInfo sti2) ->
                                 (StateInfo (Map.union sti1 sti2)))
                info1 info2)
