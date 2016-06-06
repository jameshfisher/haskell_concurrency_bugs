module DeadlockEmpire where

import qualified Data.IORef as IORef
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Control.Monad.Extra as MonadExtra
import Control.Monad.Extra ((&&^))

type Counter = IORef.IORef Int

incrementCounter :: Counter -> IO ()
incrementCounter c = IORef.modifyIORef' c succ

criticalSection :: IO ()
criticalSection = do
  putStrLn "Entering critical section ..."
  putStrLn "Leaving critical section."

progSimpleCounter :: IO ()
progSimpleCounter = do
  counter <- IORef.newIORef 0
  let process n = do
        _ <- Concurrent.forkIO $
          Monad.forever $ do
            incrementCounter counter
            MonadExtra.whenM (fmap (==n) (IORef.readIORef counter)) criticalSection
        return ()
  process 5
  process 3

progConfusedCounter :: IO ()
progConfusedCounter = do
  first <- IORef.newIORef 0
  second <- IORef.newIORef 0
  Concurrent.forkIO $ do
    incrementCounter first
    incrementCounter second
    MonadExtra.whenM
      (fmap (==2) (IORef.readIORef second) &&^ fmap (/=2) (IORef.readIORef first))
      undefined  -- TODO assert false
  Concurrent.forkIO $ do
    incrementCounter first
    incrementCounter second
  return ()
