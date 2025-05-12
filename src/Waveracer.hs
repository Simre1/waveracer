{-# LANGUAGE PatternSynonyms #-}
module Waveracer
  ( Waveform,
    loadFile,
    Trace,
    runTrace,
    Signal,
    load,
    loadMany,
    loadAsMap,
    (@+),
    (@-),
    findIndices,
    sampleAt,
    sampleOn,
    Inspect,
    runInspect,
    inspect,
    SignalValue(..),
    pattern BitsValue,
    pattern StringValue,
    pattern IntValue,
    inspectTime,
    TraceTime (..),
    Timescale (..),
    TimeUnit (..),
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import System.Mem
import Waveracer.Internal
import Data.Text (Text)
import qualified Data.Text as T

data TraceEnv = TraceEnv
  { waveform :: Waveform,
    timeIndices :: S.Set TimeIndex
  }

newtype Trace a = Trace (ReaderT TraceEnv IO a) deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

newtype Inspect a = Inspect (ReaderT TimeIndex Trace a) deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

getWaveform :: Trace Waveform
getWaveform = (.waveform) <$> Trace ask

getCurrentTimeIndices :: Trace (S.Set TimeIndex)
getCurrentTimeIndices = (.timeIndices) <$> Trace ask

getCurrentTimeIndex :: Inspect TimeIndex
getCurrentTimeIndex = Inspect ask

sampleAt :: [TimeIndex] -> Trace a -> Trace a
sampleAt indices (Trace m) = Trace $ local (\env -> env {timeIndices = S.fromList indices}) m

sampleOn :: Inspect Bool -> Trace a -> Trace a
sampleOn inspect trace = do
  indices <- findIndices inspect
  sampleAt indices trace

runInspect :: Inspect a -> Trace [a]
runInspect (Inspect m) = do
  timeIndices <- getCurrentTimeIndices
  waveform <- getWaveform
  liftIO $ loadQueuedSignals waveform
  traverse (runReaderT m) $ S.toAscList timeIndices

findIndices :: Inspect Bool -> Trace [TimeIndex]
findIndices inspectBool = do
  indices <- S.toAscList <$> getCurrentTimeIndices
  boolIndices <- runInspect inspectBool
  pure $ fmap snd (filter fst (zip boolIndices indices))

runTrace :: Waveform -> Trace a -> IO a
runTrace waveform (Trace m) = do
  timeIndices <- getTimeIndices waveform
  runReaderT m (TraceEnv waveform (S.fromList timeIndices))

(@+) :: Inspect SignalValue -> Int -> Inspect SignalValue
(@+) (Inspect m) offset = do
  indices <- Inspect $ lift $ getCurrentTimeIndices
  currentTimeIndex <- getCurrentTimeIndex
  Inspect $ lift $ go indices currentTimeIndex offset
  where
    go indices timeIndex offset
      | offset > 0 = case S.lookupGT timeIndex indices of
          Just newTimeIndex -> go indices newTimeIndex (pred offset)
          Nothing -> pure $ ErrorValue $ "Cannot step forwards for time index " ++ show timeIndex
      | offset < 0 = case S.lookupLT timeIndex indices of
          Just newTimeIndex -> go indices newTimeIndex (succ offset)
          Nothing -> pure $ ErrorValue $ "Cannot step backwards for time index " ++ show timeIndex
      | otherwise = runReaderT m timeIndex

(@-) :: Inspect SignalValue -> Int -> Inspect SignalValue
(@-) inspect offset = inspect @+ (-offset)

load :: Text -> Trace Signal
load string = do
  env <- Trace ask
  maybeVarRef <- liftIO $ lookupSignal env.waveform string
  case maybeVarRef of
    Just a -> pure a
    Nothing -> fail $ "Signal " ++ T.unpack string ++ " not found"

loadMany :: (Traversable t) => t Text -> Trace (t Signal)
loadMany = traverse load

loadAsMap :: [Text] -> Trace (M.Map Text Signal)
loadAsMap strings = do
  signals <- loadMany strings
  pure $ M.fromList $ zip strings signals

inspect :: Signal -> Inspect SignalValue
inspect signal = do
  waveform <- Inspect $ lift $ getWaveform
  ti <- getCurrentTimeIndex
  liftIO $ getSignal waveform signal ti

inspectTime :: Inspect TraceTime
inspectTime = do
  waveform <- Inspect $ lift $ getWaveform
  ti <- getCurrentTimeIndex
  liftIO $ getTraceTime waveform ti

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
        instrValue1 <- inspect instr2 @+ 1
        time <- inspectTime
        pure (instrValue, instrValue1, time)
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
