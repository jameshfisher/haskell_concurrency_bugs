module SimpleCounter where

import qualified Data.IORef as IORef
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Control.Monad.Extra as MonadExtra

type Counter = IORef.IORef Int

incrementCounter :: Counter -> IO ()
incrementCounter c = IORef.modifyIORef' c succ

criticalSection :: IO ()
criticalSection = do
  putStrLn "Entering critical section ..."
  putStrLn "Leaving critical section."

prog :: IO ()
prog = do
  counter <- IORef.newIORef 0
  let process n = do
        _ <- Concurrent.forkIO $
          Monad.forever $ do
            incrementCounter counter
            MonadExtra.whenM (fmap (==n) (IORef.readIORef counter)) criticalSection
        return ()
  process 5
  process 3
