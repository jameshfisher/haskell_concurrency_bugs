module Monitor where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Monad as Monad

data Monitor = Monitor (Chan.Chan ()) (Concurrent.MVar ())

newMonitor :: IO Monitor
newMonitor = Monitor <$> Chan.newChan <*> Concurrent.newMVar ()

dupMonitor :: Monitor -> IO Monitor
dupMonitor (Monitor c m) = do
  dupC <- Chan.dupChan c
  return $ Monitor dupC m

enter :: Monitor -> IO ()
enter (Monitor _ m) = Concurrent.takeMVar m

wait :: Monitor -> IO ()
wait (Monitor c m) = do
  Concurrent.putMVar m ()
  Monad.void $ Chan.readChan c

exit :: Monitor -> IO ()
exit (Monitor _ m) = Concurrent.putMVar m ()

pulseAll :: Monitor -> IO ()
pulseAll (Monitor c _) = Chan.writeChan c ()
