module Waveracer where

import Control.Monad
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.Text
import Data.Traversable
import GHC.Generics
import GHC.IO.Unsafe (unsafePerformIO)
import Waveracer.Raw qualified as Raw

newtype Tracing a = Tracing (IO a) deriving (Functor, Applicative, Monad)

data Trace = Trace
  { waveform :: Raw.Waveform
  }

data Signal = Signal
  { ref :: Raw.VarRef
  }

data TraceTime
  = MicroSeconds Integer
  | Nanoseconds Integer
  | Picoseconds Integer
  deriving (Show, Eq, Ord, Generic)

data SignalName = SignalName {scopes :: [Text], name :: Text}
  deriving (Show, Eq, Ord, Generic)

loadSignals :: (Traversable t) => Trace -> t String -> IO (Maybe (t Signal))
loadSignals (Trace waveform) names = do
  maybeRefs <- for names $ \name -> do
    Raw.lookupSignal waveform name
  case sequence maybeRefs of
    Just refs -> do
      Raw.loadSignals waveform (toList refs)
      pure $ Just $ Signal <$> refs
    Nothing -> pure Nothing

loadTrace :: FilePath -> IO Trace
loadTrace filepath = Trace <$> Raw.loadFile filepath

sampleAt :: Trace -> TraceTime -> Tracing a -> IO a
sampleAt = undefined

sampleOn :: Trace -> TraceTime -> () -> IO a
sampleOn = undefined

someFunc :: IO ()
someFunc = putStrLn "someFunc"

test :: IO ()
test = do
  trace@(Trace wf) <- loadTrace "/home/simon/Downloads/ics-edu-rv32i-sc-new-01b6f7e339eb.fst"
  -- Raw.loadSignals wf [Raw.VarRef 32864]
  Just signals <- loadSignals trace ["testbench.dut.clk"]
  x <- traverse (Raw.getSignal wf ((Prelude.head signals).ref)) [0 .. 40]
  print x
  pure ()

  pure undefined
