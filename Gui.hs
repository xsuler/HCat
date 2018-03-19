--------------------------
-- 2017.12
-- @ sule
-- @ aeonsule@163.com
--------------------------
--------------------------
-- remember to fix speed
--------------------------
{-# LANGUAGE RankNTypes #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import qualified Data.HashMap.Lazy               as H
import qualified Data.IntMap                     as IM
import           Data.IORef
import           Data.Matrix
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Graphics.QML
import           Graphics.QML.Objects.ParamNames
import           GridMap

-- |convert point to index in a map matrix
point2int :: GridMap -> Point -> Int
point2int mp pt = (fst pt - 1) * ncols mp + snd pt - 1

-- |convert index to point in a map matrix
int2point :: GridMap -> Int -> Point
int2point mp inx = (((inx + 1) `div` ncols mp) + 1, (inx + 1) `mod` ncols mp)

guess :: IM.IntMap [Int] -> Int -> GridMap -> IO (IM.IntMap [Int])
guess vec ms mp = return $ hunt cats (int2point mp ms) mp
  where
    cats = IM.map (int2point mp . head) vec

-- |cats find paths to hunt mouse
hunt :: IM.IntMap Point -> Point -> GridMap -> IM.IntMap [Int]
hunt cats ms mp =
  snd $ IM.mapAccum (\rmp pt -> (unsafeSet Stone pt mp,reverse $ map (point2int mp) $
  let res=shortestPath rmp pt ms in if null res then [pt] else res)) mp cats


calcPath :: TVar (IM.IntMap [Int]) -> Int -> GridMap -> IO ()
calcPath vecM ms mp = do
  mvec <- atomically $ readTVar vecM
  res <- guess mvec ms mp
  atomically $ writeTVar vecM res

-- |main thread to handle the behavior of cats
handleCat ::
     ObjRef ()
  -> SignalKey (IO ())
  -> SignalKey (IO ())
  -> TChan (Int, Int)
  -> GridMap
  -> TVar (IM.IntMap [Int])
  -> IORef T.Text
  -> IO ()
handleCat obj catIK skey pend mp vecM catI = do
  mvec <- atomically $ readTVar vecM
  (x, l) <- atomically $ readTChan pend
  if x == (-1)
    then return ()
    else do
      let memt = IM.lookup x mvec
      case memt of
        Just mem
          | null mem -> handleCat obj catIK skey pend mp vecM catI
          | head mem == l -> handleCat obj catIK skey pend mp vecM catI
          | otherwise -> do
            catio <- T.strip <$> readIORef catI
            writeIORef catI $
              T.concat
                [ catio
                , T.pack " "
                , T.pack $show x
                , T.pack " "
                , T.pack $show $head mem
                ]
            fireSignal catIK obj
            fireSignal skey obj
            handleCat obj catIK skey pend mp vecM catI
        Nothing ->
          atomically (writeTVar vecM (IM.insert x [l] mvec)) >>
          handleCat obj catIK skey pend mp vecM catI

clearTChan::TChan a->STM ()
clearTChan ch=  do
  test<-isEmptyTChan ch
  if test then return () else  readTChan ch>>clearTChan ch

main :: IO ()
main = do
  pend <- newTChanIO :: IO (TChan (Int, Int))
  mpM <- newIORef $ fromList 0 0 [] :: IO (IORef GridMap)
  catI <- newIORef $ T.pack ""
  gameOver <- newIORef False :: IO (IORef Bool)
  govK <- newSignalKey::IO (SignalKey (IO ()))
  vecM <- newTVarIO IM.empty
  moveK <- newSignalKey
  catIK <- newSignalKey
  clazz <-
    newClass
      [ defMethod'
          "init"
          (\obj raw -> do
             mp <- readMap raw
             writeIORef catI $ T.pack ""
             writeIORef gameOver False
             atomically $ writeTVar vecM IM.empty
             atomically $ clearTChan pend
             writeIORef mpM mp
             forkIO $ handleCat obj catIK moveK pend mp vecM catI
             return ())
      , defMethod'
          "pending"
          (\_ cat lc -> atomically $ writeTChan pend (cat, lc))
      , defMethod'
          "updateMouse"
          (\_ ms -> do
             mp <- readIORef mpM
             calcPath vecM ms mp
             return ())
      , defMethod'
          "moveOn"
          (\obj ms -> do
             mvec <- atomically $ readTVar vecM
             writeIORef gameOver $
               IM.foldl' (||) False $
               IM.map (\x -> not (null x) && (head x == ms)) mvec
             fireSignal govK obj
             atomically  $
               writeTVar
                 vecM
                 (IM.map
                    (\x ->
                       if length x <= 1
                         then x
                         else tail x)
                    mvec)
             return ())
      , defPropertyRO' "self" return
      , defSignal "move" moveK
      , defPropertySigRW'
          "catI"
          catIK
          (\_ -> readIORef catI)
          (\_ v -> writeIORef catI v)
      , defPropertySigRO' "gameOver" govK (\_ -> readIORef gameOver)
      ]
  ctx <- newObject clazz ()
  runEngineLoop
    defaultEngineConfig
    { initialDocument = fileDocument "Gui.qml"
    , contextObject = Just $ anyObjRef ctx
    }
