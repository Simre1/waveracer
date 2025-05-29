{-# LANGUAGE PatternSynonyms #-}

module Waveracer
  ( Waveform,
    loadWaveformFile,
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
    SignalValue (..),
    decodeInt,
    decodeInteger,
    decodeWord,
    decodeIntegral,
    decodeText,
    decodeDouble,
    decodeBitString,
    encodeBitString,
    encodeInt,
    encodeText,
    encodeDouble,
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
import Data.Text (Text)
import Data.Text qualified as T
import System.Mem
import Waveracer.Internal

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

(@+) :: Inspect SignalValue -> Int -> Inspect (SignalValue)
(@+) (Inspect m) offset = do
  indices <- Inspect $ lift $ getCurrentTimeIndices
  currentTimeIndex <- getCurrentTimeIndex
  Inspect $ lift $ go indices currentTimeIndex offset
  where
    go indices timeIndex offset
      | offset > 0 = case S.lookupGT timeIndex indices of
          Just newTimeIndex -> go indices newTimeIndex (pred offset)
          Nothing -> go indices timeIndex 0
      | offset < 0 = case S.lookupLT timeIndex indices of
          Just newTimeIndex -> go indices newTimeIndex (succ offset)
          Nothing -> go indices timeIndex 0
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
