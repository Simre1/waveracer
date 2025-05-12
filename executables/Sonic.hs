import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable
import GHC.Generics (Generic)
import Waveracer
import Prelude hiding (log)
import qualified Data.Sequence as S
import Data.Foldable (for_)

main :: IO ()
main = do
  wf <- loadFile "/home/simon/Downloads/new-ics-edu-rv32i-sc-cd87a3b1dde9.fst"

  values <- runTrace wf $ do
    instr <- load "testbench.dut.Instr"
    clk <- load "testbench.dut.clk"
    reset <- load "testbench.dut.reset"
    (_, stages) <- defineStages $ do
      stage "single" $ do
        log "pc" "testbench.dut.dp.PC"
        log "pcnext" "testbench.dut.dp.PCNext"
        log "pcplus4" "testbench.dut.dp.PCPlus4"
        log "instr" "testbench.dut.Instr"
        log "a1" "testbench.dut.dp.rf.A1"
        log "a2" "testbench.dut.dp.rf.A2"
        log "a3" "testbench.dut.dp.rf.A3"
        log "pcsrc" "testbench.dut.dp.PCSrc"
        log "regwrite" "testbench.dut.dp.RegWrite"
        log "resultsrc" "testbench.dut.dp.ResultSrc"
        log "memwrite" "testbench.dut.dp.MemWrite"
        log "alucontrol" "testbench.dut.dp.ALUControl"
        log "alusrc" "testbench.dut.dp.ALUSrc"
        log "immsrc" "testbench.dut.dp.ImmSrc"
        log "immext" "testbench.dut.dp.ImmExt"
        log "srca" "testbench.dut.dp.SrcA"
        log "srcb" "testbench.dut.dp.SrcB"
        log "zero" "testbench.dut.dp.Zero"
        log "aluresult" "testbench.dut.dp.ALUResult"
        log "readdata" "testbench.dut.dp.ReadData"
        log "pctarget" "testbench.dut.dp.PCTarget"
        log "result" "testbench.dut.dp.Result"
        log "writedata" "testbench.dut.dp.WriteData"
        for_ [0 .. 31] $ \i -> do
          log ("x" ++ show i) ("testbench.ram_regs." <> T.pack (show i))
        logExpression "realtime" (IntValue . fromIntegral . (.time) <$> inspectTime)

    memorySignals <- loadMany $ M.fromList [(i, "testbench.ram_dmem." <> T.pack (show i)) | i <- [0 .. 2 ^ 14 - 1]]

    let condition = do
          clkValue <- inspect clk
          resetValue <- inspect reset
          pure $ clkValue == 1 && resetValue /= 1

    sampleOn condition $
      traceProcessor $
        Processor
          { instruction = inspect instr,
            stages = stages,
            memory = M.filter (\case (IntValue x) -> x /= 0; _ -> True) <$> traverse inspect memorySignals
          }

  printJSON $ toJSON values

log :: String -> Text -> Stage ()
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
traceProcessor (Processor stages instruction memory) = do
  results <- runInspect $ do
    stages' <- for stages $ \stage -> for stage $ \sv -> sv
    instruction' <- instruction
    memory' <- memory
    pure (stages', instruction', memory')
  let (stages, instructions, memory) = unzip3 results
  let stages' = fmap (fmap List) $ fmap mergeMaps $ mergeMaps stages
  pure $
    Processor
      stages'
      (List instructions)
      (List memory)

newtype Stage a = Stage (StateT (M.Map String (Inspect SignalValue)) Trace a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

newtype DefineStages a = DefineStages (StateT (M.Map String (M.Map String (Inspect SignalValue))) Trace a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

defineStages :: DefineStages a -> Trace (a, M.Map String (M.Map String (Inspect SignalValue)))
defineStages (DefineStages m) = runStateT m M.empty

data Processor f = Processor
  { stages :: M.Map String (M.Map String (f SignalValue)),
    instruction :: f SignalValue,
    memory :: f (M.Map Int SignalValue)
  }
  deriving (Generic)

newtype List a = List [a]
  deriving (Eq, Ord, Functor, Applicative, Monad, Show, Read, Foldable, Traversable, Generic)

instance (ToJSON a) => ToJSON (List a) where
  toJSON (List values) = toJSON values

instance ToJSON SignalValue where
  toJSON value = signalValueToJSON value

signalValueToJSON :: SignalValue -> Value
signalValueToJSON v = toJSON $ show v

deriving instance Show (Processor List)

instance ToJSON (Processor List)

printJSON :: Value -> IO ()
printJSON val = BL.putStrLn (encode val)

mergeMaps :: (Ord k) => [M.Map k a] -> M.Map k [a]
mergeMaps = foldl' (M.unionWith (<>)) M.empty . map (M.map (:[]))
