module Algorithm
  ( shortestPathTable
  , getStatesR
  , conBDS
  , PathFinder
  , StateGen
  ) where

import           Control.Concurrent
import           Data.Hashable
import qualified Data.HashMap.Lazy  as H

-- |general type of search algorithm, state->action->states map
-- action has type : state -> H.HashMap state w
type Search state w
   = state -> (state -> H.HashMap state w) -> H.HashMap state state

-- |general type of search algorithm, IO version
type IOSearch state w
   = MVar state -> MVar (H.HashMap state state) -> MVar (H.HashMap state state) -> state -> (state -> H.HashMap state w) -> IO ()


type IOPathFinder state = state -> state -> IO [state]

type PathFinder state = state -> state -> [state]

type StateGen state w = state -> H.HashMap state w


-- |dijkstra search algorithm
dijkstra ::
     (Ord w, Num w, Bounded w, Hashable state, Eq state, Show state, Show w)
  => (state, w)
  -> state
  -> H.HashMap state (w, state)
  -> (state -> H.HashMap state w)
  -> H.HashMap state state
  -> H.HashMap state state
dijkstra (state, sw) target open stateGen close
  | H.null open'' = close
  | H.member target close = close
  | otherwise = dijkstra (bestState, bestW) target open' stateGen close'
  where
    close' = H.insert bestState preState close
    open' = H.delete bestState open''
    (bestState, preState, bestW) =
      H.foldlWithKey'
        (\a@(_, _, rbw) rst (bw, pst) ->
           if rbw > bw
             then (rst, pst, bw)
             else a)
        (state, state, maxBound)
        open''
    open'' =
      H.unionWith
        (\a@(rsw, _) b@(rsw', _) ->
           if rsw' > rsw
             then a
             else b)
        open
        nexts
    nexts = H.map (\w -> (w + sw, state)) $stateGen state `H.difference` close

-- |get hash table of shortest path
shortestPathTable ::
     (Ord w, Num w, Bounded w, Hashable state, Eq state, Show state, Show w)
  => state->Search state w
shortestPathTable target initSt stateGen =
  dijkstra (initSt, 0 ) target open stateGen $ H.singleton initSt initSt
  where
    open = H.map (\w -> (w, initSt)) $stateGen initSt

-- |helper function to get list of states
getStatesR ::
     (Hashable state, Eq state, Show state)
  => H.HashMap state state
  -> state
  -> [state]
getStatesR h st =
  case H.lookup st h of
    Just st' ->
      if st' == st
        then []
        else st' : getStatesR h st'
    Nothing -> []

-- |get hash table of shortest path, IO version
shortestPathTableIO ::
     (Ord w, Num w, Bounded w, Hashable state, Eq state, Show state, Show w)
  => IOSearch state w
shortestPathTableIO key box1 box2 initSt stateGen =
  conDijkstra key box1 box2 (initSt, 0) open stateGen $
  H.singleton initSt initSt
  where
    open = H.map (\w -> (w, initSt)) $stateGen initSt

-- |concurrent bidirectional search, based on concurrent version dijkstra
conBDS ::
     (Ord w, Num w, Bounded w, Hashable state, Eq state, Show state, Show w)
  => StateGen state w
  -> StateGen state w
  -> IOPathFinder state
conBDS genS genT s t = do
  box1 <- newEmptyMVar :: IO (MVar (H.HashMap state state))
  box2 <- newEmptyMVar :: IO (MVar (H.HashMap state state))
  key <- newEmptyMVar :: IO (MVar state)
  flag <- newEmptyMVar :: IO (MVar Bool)
  forkIO $ do
    shortestPathTableIO key box1 box2 s genS
    putMVar flag True
  shortestPathTableIO key box2 box1 t genT
  takeMVar flag
  skey <- tryReadMVar key
  case skey of
    Just sk -> do
      Just htbl1 <- tryReadMVar box1
      Just htbl2 <- tryReadMVar box2
      return $ reverse (getStatesR htbl2 sk) ++ [sk] ++ getStatesR htbl1 sk
    Nothing -> do
      Just htbl1 <- tryReadMVar box1
      return $ t : getStatesR htbl1 t

-- |concurrent version dijkstra
conDijkstra ::
     (Ord w, Num w, Bounded w, Hashable state, Eq state, Show state, Show w)
  => MVar state
  -> MVar (H.HashMap state state)
  -> MVar (H.HashMap state state)
  -> (state, w)
  -> H.HashMap state (w, state)
  -> (state -> H.HashMap state w)
  -> H.HashMap state state
  -> IO ()
conDijkstra key mbox nbox (state, sw) open stateGen close
  | H.null open'' = do
    tryTakeMVar mbox
    tryPutMVar mbox close
    return ()
  | otherwise = do
    skey <- tryReadMVar key
    case skey of
      Just _ -> return ()
      Nothing -> do
        box <- tryReadMVar nbox
        tryTakeMVar mbox
        tryPutMVar mbox close
        case box of
          Just cont ->
            if H.null $ H.intersection cont close -- need fix
              then conDijkstra
                     key
                     mbox
                     nbox
                     (bestState, bestW)
                     open'
                     stateGen
                     close'
              else do
                tryPutMVar key state
                return ()
          Nothing ->
            conDijkstra key mbox nbox (bestState, bestW) open' stateGen close'
  where
    close' = H.insert bestState preState close
    open' = H.delete bestState open''
    (bestState, preState, bestW) =
      H.foldlWithKey'
        (\a@(_, _, rbw) rst (bw, pst) ->
           if rbw > bw
             then (rst, pst, bw)
             else a)
        (state, state, maxBound)
        open''
    open'' =
      H.unionWith
        (\a@(rsw, _) b@(rsw', _) ->
           if rsw' > rsw
             then a
             else b)
        open
        nexts
    nexts = H.map (\w -> (w + sw, state)) $stateGen state `H.difference` close
