module SimpleCounter where

import qualified Data.IORef as IORef
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad

type Counter = IORef.IORef Int

incrementCounter :: Counter -> IO ()
incrementCounter c = IORef.modifyIORef' c succ

loop :: IO () -> IO ()
loop body = body >> loop body

if_ :: IO Bool -> IO () -> IO ()
if_ cond body = do
  exec <- cond
  Monad.when exec body

criticalSection :: IO ()
criticalSection = do
  putStrLn "Entering critical section ..."
  putStrLn "Leaving critical section."

prog :: IO ()
prog = do
  counter <- IORef.newIORef 0
  let process n = do
        _ <- Concurrent.forkIO $
          loop $ do
            incrementCounter counter
            if_ (fmap (==n) (IORef.readIORef counter)) criticalSection
        return ()
  process 5
  process 3

