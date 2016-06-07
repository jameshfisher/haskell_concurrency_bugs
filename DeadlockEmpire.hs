module DeadlockEmpire where

import qualified Data.IORef as IORef
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
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

progDeadlock :: IO ()
progDeadlock = do
  m1 <- Concurrent.newMVar ()
  m2 <- Concurrent.newMVar ()
  Monad.void $ Async.concurrently (thread1 m1 m2) (thread2 m1 m2)
  where
    thread1 m1 m2 = do
      Concurrent.takeMVar m1
      Concurrent.takeMVar m2
      criticalSection
      Concurrent.putMVar m1 ()
      Concurrent.putMVar m2 ()
    thread2 m1 m2 = do
      Concurrent.takeMVar m2
      Concurrent.takeMVar m1
      criticalSection
      Concurrent.putMVar m2 ()
      Concurrent.putMVar m1 ()

progCountdownEvent :: IO ()
progCountdownEvent = do
  progress <- IORef.newIORef 0
  event <- Concurrent.newQSem 3
  Monad.void $ Async.concurrently (thread1 progress event) (thread2 progress event)
  where
    thread1 progress event = do
      IORef.modifyIORef' progress (+ 20)
      MonadExtra.whenM ((>= 20) <$> IORef.readIORef progress)
                       $ Concurrent.signalQSem event
      Concurrent.waitQSem event

    thread2 progress event = do
      IORef.modifyIORef' progress (+ 30)
      MonadExtra.whenM ((>= 30) <$> IORef.readIORef progress)
                       $ Concurrent.signalQSem event

      IORef.modifyIORef' progress (+ 50)
      MonadExtra.whenM ((>= 80) <$> IORef.readIORef progress )
                       $ Concurrent.signalQSem event

      Concurrent.waitQSem event
