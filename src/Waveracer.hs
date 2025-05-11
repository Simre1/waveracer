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
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Traversable
import Debug.Trace
import GHC.Generics
import System.Mem
import Waveracer.Raw

data TraceEnv = TraceEnv
  { waveform :: Waveform,
    timeIndices :: S.Set TimeIndex
  }

newtype Trace a = Trace (ReaderT TraceEnv IO a) deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

newtype Inspect a = Inspect (ReaderT TimeIndex Trace a) deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

getWaveformT :: Trace Waveform
getWaveformT = (.waveform) <$> Trace ask

getTimeIndicesT :: Trace (S.Set TimeIndex)
getTimeIndicesT = (.timeIndices) <$> Trace ask

getTimeIndexI :: Inspect TimeIndex
getTimeIndexI = Inspect ask

-- loadQueue :: Trace ()
-- loadQueue = do
--   env <- Trace ask
--   queue <- liftIO $ atomicModifyIORef' env.queuedVars (\q -> (S.empty, q))
--   liftIO $ loadVars env.waveform (toList queue)
--   pure ()

getWaveformI :: Inspect Waveform
getWaveformI = Inspect $ lift getWaveformT

sampleAt :: [TimeIndex] -> Trace a -> Trace a
sampleAt indices (Trace m) = Trace $ local (\env -> env {timeIndices = S.fromList indices}) m

sampleOn :: Inspect Bool -> Trace a -> Trace a
sampleOn inspect trace = do
  indices <- findIndices inspect
  sampleAt indices trace

runInspect :: Inspect a -> Trace [a]
runInspect (Inspect m) = do
  timeIndices <- getTimeIndicesT
  waveform <- getWaveformT
  liftIO $ loadQueuedSignals waveform
  traverse (runReaderT m) $ S.toAscList timeIndices

findIndices :: Inspect Bool -> Trace [TimeIndex]
findIndices inspectBool = do
  indices <- S.toAscList <$> getTimeIndicesT
  boolIndices <- runInspect inspectBool
  pure $ fmap snd (filter fst (zip boolIndices indices))

runTrace :: Waveform -> Trace a -> IO a
runTrace waveform (Trace m) = do
  timeIndices <- getTimeIndices waveform
  runReaderT m (TraceEnv waveform (S.fromList timeIndices))

(@+) :: Signal -> Int -> Signal
(@+) (Signal offset ref) plus = Signal (plus + offset) ref

(@-) :: Signal -> Int -> Signal
(@-) (Signal offset ref) plus = Signal (plus - offset) ref

-- newtype Load a = Load (ReaderT Waveform (StateT (S.Seq VarRef) (ExceptT String IO)) a) deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

load :: String -> Trace Signal
load string = do
  env <- Trace ask
  maybeVarRef <- liftIO $ lookupSignal env.waveform string
  case maybeVarRef of
    Just a -> pure a
    Nothing -> fail $ "Signal " ++ string ++ " not found"

loadMany :: (Traversable t) => t String -> Trace (t Signal)
loadMany = traverse load

loadAsMap :: [String] -> Trace (M.Map String Signal)
loadAsMap strings = do
  signals <- loadMany strings
  pure $ M.fromList $ zip strings signals

inspect :: Signal -> Inspect SignalValue
inspect signal = do
  waveform <- getWaveformI
  ti <- getTimeIndexI
  indices <- Inspect $ lift $ getTimeIndicesT
  liftIO $ getSignal waveform signal ti indices

testNew :: IO ()
testNew = do
  wf <- loadFile "/home/simon/Downloads/ics-edu-rv32i-sc-2c8950d9a30b.vcd"
  runTrace wf $ do
    instr <- load "testbench.instr"
    instr2 <- load "testbench.instr"
    clk <- load "testbench.dut.clk"
    sampleOn ((1 ==) <$> inspect clk) $ do
      values <- runInspect $ do
        instrValue <- inspect instr
        instrValue1 <- inspect (instr2 @+ 1)
        pure (instrValue, instrValue1)
      liftIO $ do
        print values
    liftIO performGC
    sampleOn ((1 ==) <$> inspect clk) $ do
      values <- runInspect $ do
        instrValue <- inspect instr
        pure (instrValue)
      liftIO $ do
        print values
  performGC
  pure ()

-- data TraceTime
--   = MicroSeconds Integer
--   | Nanoseconds Integer
--   | Picoseconds Integer
--   deriving (Show, Eq, Ord, Generic)
