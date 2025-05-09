module Waveracer where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.IORef
import Data.Maybe
import Data.Sequence qualified as S
import Data.Text qualified as T
import Data.Traversable
import GHC.Generics
import System.Mem
import Waveracer.Raw

data TraceEnv = TraceEnv
  { waveform :: Waveform,
    timeIndices :: [TimeIndex],
    queuedVars :: IORef (S.Seq VarRef)
  }

newtype Trace a = Trace (ReaderT TraceEnv IO a) deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

newtype Inspect a = Inspect (ReaderT TimeIndex Trace a) deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

getWaveformT :: Trace Waveform
getWaveformT = (.waveform) <$> Trace ask

getTimeIndicesT :: Trace [TimeIndex]
getTimeIndicesT = (.timeIndices) <$> Trace ask

getTimeIndexI :: Inspect TimeIndex
getTimeIndexI = Inspect ask

loadQueue :: Trace ()
loadQueue = do
  env <- Trace ask
  queue <- liftIO $ atomicModifyIORef' env.queuedVars (\q -> (S.empty, q))
  liftIO $ loadVars env.waveform (toList queue)
  pure ()

getWaveformI :: Inspect Waveform
getWaveformI = Inspect $ lift getWaveformT

sampleAt :: [TimeIndex] -> Trace a -> Trace a
sampleAt indices (Trace m) = Trace $ local (\env -> env {timeIndices = indices}) m

sampleOn :: Inspect Bool -> Trace a -> Trace a
sampleOn inspect trace = do
  indices <- findIndices inspect
  sampleAt indices trace

runInspect :: Inspect a -> Trace [a]
runInspect (Inspect m) = do
  timeIndices <- getTimeIndicesT
  loadQueue
  traverse (runReaderT m) timeIndices

findIndices :: Inspect Bool -> Trace [TimeIndex]
findIndices (Inspect m) = do
  indices <- getTimeIndicesT
  boolIndices <- traverse (\index -> runReaderT m index) indices
  pure $ fmap snd (filter fst (zip boolIndices indices))

runTrace :: Waveform -> Trace a -> IO a
runTrace waveform (Trace m) = do
  timeIndices <- getTimeIndices waveform
  queueRef <- newIORef S.empty
  runReaderT m (TraceEnv waveform timeIndices queueRef)

-- newtype Load a = Load (ReaderT Waveform (StateT (S.Seq VarRef) (ExceptT String IO)) a) deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

load :: String -> Trace Signal
load string = do
  env <- Trace ask
  maybeVarRef <- liftIO $ lookupVar env.waveform string
  case maybeVarRef of
    Just a -> do
      liftIO $ modifyIORef' env.queuedVars (S.|> a)
      pure $ Signal a
    Nothing -> fail $ "Signal " ++ string ++ " not found"

loadMany :: (Traversable t) => t String -> Trace (t Signal)
loadMany = traverse load

inspect :: Signal -> Inspect SignalValue
inspect signal = do
  waveform <- getWaveformI
  ti <- getTimeIndexI
  liftIO $ getSignal waveform signal ti

testNew :: IO ()
testNew = do
  wf <- loadFile "/home/simon/Downloads/ics-edu-rv32i-sc-2c8950d9a30b.vcd"
  runTrace wf $ do
    clk <- load "testbench.dut.clk"
    aluSrc <- load "testbench.dut.rvsingle.c.md.alusrc"
    values <- runInspect $ do
      clkValue <- inspect clk
      aluSrcValue <- inspect aluSrc
      pure (clkValue, aluSrcValue)
    liftIO $ print values
  performGC
  -- wf <- getWaveformT
  -- liftIO $ unloadVars wf [coerce signal]

  pure ()

test :: IO ()
test = do
  wf <- loadFile "/home/simon/Downloads/ics-edu-rv32i-sc-2c8950d9a30b.vcd"
  Just signals <- loadSignals wf ["testbench.dut.clk"]
  x <- traverse (getSignal wf (Prelude.head signals)) $ fmap TimeIndex [0 .. 40]
  print x
  pure ()

-- data TraceTime
--   = MicroSeconds Integer
--   | Nanoseconds Integer
--   | Picoseconds Integer
--   deriving (Show, Eq, Ord, Generic)
