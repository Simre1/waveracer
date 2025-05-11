import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Traversable
import GHC.Generics (Generic)
import Waveracer
import Prelude hiding (log)

main :: IO ()
main = do
  wf <- loadFile "/home/simon/Downloads/ics-edu-rv32i-sc-2c8950d9a30b.vcd"

  values <- runTrace wf $ do
    instr <- load "testbench.dut.rvsingle.dp.instr"
    clk <- load "testbench.dut.clk"
    reset <- load "testbench.dut.reset"
    (_, stages) <- defineStages $ do
      stage "single" $ do
        log "pc" "testbench.dut.rvsingle.dp.pc"
        log "pcnext" "testbench.dut.rvsingle.dp.pcnext"
        log "pcplus4" "testbench.dut.rvsingle.dp.pcplus4"
        log "instr" "testbench.dut.rvsingle.dp.instr"
        log "a1" "testbench.dut.rvsingle.dp.rf.a1"
        log "a2" "testbench.dut.rvsingle.dp.rf.a2"
        log "a3" "testbench.dut.rvsingle.dp.rf.a3"
        log "pcsrc" "testbench.dut.rvsingle.dp.pcsrc"
        log "regwrite" "testbench.dut.rvsingle.dp.regwrite"
        log "resultsrc" "testbench.dut.rvsingle.dp.resultsrc"
        log "memwrite" "testbench.dut.rvsingle.memwrite"
        log "alucontrol" "testbench.dut.rvsingle.dp.alucontrol"
        log "alusrc" "testbench.dut.rvsingle.dp.alusrc"
        log "immsrc" "testbench.dut.rvsingle.dp.immsrc"
        log "immext" "testbench.dut.rvsingle.dp.immext"
        log "srca" "testbench.dut.rvsingle.dp.srca"
        log "srcb" "testbench.dut.rvsingle.dp.srcb"
        log "zero" "testbench.dut.rvsingle.dp.zero"
        log "aluresult" "testbench.dut.rvsingle.dp.aluresult"
        log "readdata" "testbench.dut.rvsingle.dp.readdata"
        log "pctarget" "testbench.dut.rvsingle.dp.pctarget"
        log "result" "testbench.dut.rvsingle.dp.result"
        log "writedata" "testbench.dut.rvsingle.dp.writedata"

    let condition = do
          clkValue <- inspect clk
          resetValue <- inspect reset
          pure $ clkValue == 1 && resetValue /= 1

    sampleOn condition $
      traceProcessor $
        Processor
          { instruction = inspect instr,
            stages = stages
          }

  printJSON $ toJSON values

log :: String -> String -> Stage ()
log export name = do
  signal <- Stage $ lift $ load name
  Stage $ modify (M.insert export (inspect signal))
  pure ()

logExpression :: String -> Inspect SignalValue -> Stage ()
logExpression export expression = do
  Stage $ modify (M.insert export expression)
  pure ()

stage :: String -> Stage a -> DefineStages a
stage name (Stage m) = do
  (a, loggedSignals) <- DefineStages $ lift $ runStateT m M.empty
  DefineStages $ modify $ M.insert name loggedSignals
  pure a

traceProcessor :: Processor Inspect -> Trace (Processor List)
traceProcessor (Processor stages instruction) = do
  results <- runInspect $ do
    stages' <- for stages $ \stage -> for stage $ \sv -> sv
    instruction' <- instruction
    pure (stages', instruction')
  let (stages, instructions) = unzip results
  let stages' = fmap (fmap List) $ fmap mergeMaps $ mergeMaps stages
  pure $
    Processor
      stages'
      (List instructions)

newtype Stage a = Stage (StateT (M.Map String (Inspect SignalValue)) Trace a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

newtype DefineStages a = DefineStages (StateT (M.Map String (M.Map String (Inspect SignalValue))) Trace a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

defineStages :: DefineStages a -> Trace (a, M.Map String (M.Map String (Inspect SignalValue)))
defineStages (DefineStages m) = runStateT m M.empty

data Processor f = Processor
  { stages :: M.Map String (M.Map String (f SignalValue)),
    instruction :: f SignalValue
  }
  deriving (Generic)

newtype List a = List [a]
  deriving (Eq, Ord, Functor, Applicative, Monad, Show, Read, Foldable, Traversable, Generic)

instance ToJSON (List SignalValue) where
  toJSON (List values) = toJSON $ signalValueToJSON <$> values

signalValueToJSON :: SignalValue -> Value
signalValueToJSON v = toJSON $ show v

deriving instance Show (Processor List)

instance ToJSON (Processor List)

printJSON :: Value -> IO ()
printJSON val = BL.putStrLn (encode val)

mergeMaps :: (Ord k) => [M.Map k a] -> M.Map k [a]
mergeMaps = foldl' (M.unionWith (++)) M.empty . map (M.map (: []))
