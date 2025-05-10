{-# LANGUAGE RecursiveDo #-}

module Waveracer.Raw where

-- (Waveform, Signal, loadFile, lookupSignal, loadSignals, getTimes, getSignal)

import Control.Monad (forM, guard, void)
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Unsafe qualified as BS
import Data.Char (intToDigit)
import Data.Foldable (toList)
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Set qualified as S
import Data.Text.Internal.Read
import Data.Traversable (for)
import Data.Vector.Storable qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word
import Debug.Trace
import Foreign (Storable (..))
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal (allocaArray)
import Foreign.Ptr
import GHC.Float (castWord64ToDouble)
import Numeric (readInt, showIntAtBase)
import System.Mem.Weak

foreign import ccall safe "wellen-binding.h load_file" loadFileRaw :: CString -> IO (Ptr Waveform)

foreign import ccall safe "wellen-binding.h load_vars" loadVarsRaw :: Ptr Waveform -> Ptr CUIntPtr -> CUIntPtr -> IO ()

foreign import ccall unsafe "wellen-binding.h unload_vars" unloadVarsRaw :: Ptr Waveform -> Ptr CUIntPtr -> CUIntPtr -> IO ()

foreign import ccall unsafe "wellen-binding.h lookup_var" lookupVarRaw :: Ptr Waveform -> CString -> IO (CIntPtr)

foreign import ccall unsafe "wellen-binding.h get_var" getVarRaw :: Ptr Waveform -> CUIntPtr -> Word32 -> IO (Ptr (SignalResult))

foreign import ccall unsafe "wellen-binding.h get_times" getTimesRaw :: Ptr Waveform -> IO (Ptr (CSlice Word64))

newtype Waveform = Waveform (ForeignPtr Waveform)

data VarRef = VarRef CUIntPtr deriving (Eq, Show, Ord)

data Signal = Signal {offset :: Int, varRef :: VarRef} deriving (Eq, Show, Ord)

newtype TimeIndex = TimeIndex Int deriving (Eq, Show, Ord)

data CSlice a = CSlice
  { csPtr :: Ptr a,
    csLength :: CUIntPtr
  }

data SignalResult = SignalResult
  { signalType :: CIntPtr,
    word :: Word64,
    bytes :: Ptr Word8,
    length :: CUIntPtr
  }
  deriving (Show)

instance (Storable a) => Storable (CSlice a) where
  sizeOf _ = sizeOf (undefined :: Ptr a) + sizeOf (undefined :: CUIntPtr)
  alignment _ = alignment (undefined :: Ptr a)
  peek ptr = do
    p <- peekByteOff ptr 0
    l <- peekByteOff ptr (sizeOf (undefined :: Ptr a))
    return $ CSlice p l
  poke ptr (CSlice p l) = do
    pokeByteOff ptr 0 p
    pokeByteOff ptr (sizeOf (undefined :: Ptr a)) l

loadFile :: FilePath -> IO Waveform
loadFile str = do
  withCString str $ \cString -> do
    waveformPtr <- loadFileRaw cString
    -- fin <- mkFunPtr $ \(_ :: Ptr Waveform) -> putStrLn "Cleaning Waveform"
    fPtr <- newForeignPtr freeWaveformRaw waveformPtr

    -- addForeignPtrFinalizer fin fPtr
    pure $ Waveform fPtr

loadVars :: Waveform -> [VarRef] -> IO ()
loadVars waveform@(Waveform fPtr) signals = do
  let count = length signals
  allocaArray count $ \signalsPtr -> do
    void $ forM (zip [0 ..] signals) $ \(offset, ref@(VarRef cInt)) -> mdo
      weak <- mkWeak ref waveform $ Just $ do
        maybeWaveform <- deRefWeak weak
        case maybeWaveform of
          Just aliveWaveform -> unloadVars aliveWaveform [ref]
          Nothing -> pure ()
      pokeByteOff signalsPtr (offset * sizeOf (undefined :: CUIntPtr)) cInt
    withForeignPtr fPtr $ \ptr -> do
      loadVarsRaw ptr signalsPtr (fromIntegral count)

unloadVars :: Waveform -> [VarRef] -> IO ()
unloadVars (Waveform fPtr) signals = do
  let count = length signals
  allocaArray count $ \signalsPtr -> do
    void $ forM (zip [0 ..] signals) $ \(offset, (VarRef cInt)) -> do
      pokeByteOff signalsPtr (offset * sizeOf (undefined :: CUIntPtr)) cInt
    withForeignPtr fPtr $ \ptr -> do
      unloadVarsRaw ptr signalsPtr (fromIntegral count)
    void $ putStrLn "Unloading done"

loadSignals :: (Traversable t) => Waveform -> t String -> IO (Maybe (t Signal))
loadSignals waveform names = do
  maybeRefs <- for names $ \name -> do
    lookupVar waveform name
  case sequence maybeRefs of
    Just refs -> do
      loadVars waveform (toList refs)
      pure $ Just $ Signal 0 <$> refs
    Nothing -> pure Nothing

lookupVar :: Waveform -> String -> IO (Maybe VarRef)
lookupVar (Waveform fPtr) name = do
  let count = length name
  withCString name $ \cString -> do
    withForeignPtr fPtr $ \ptr -> do
      cInt <- lookupVarRaw ptr cString
      pure $
        if cInt >= 0
          then Just $ VarRef (fromIntegral cInt)
          else Nothing

foreign import ccall "wrapper"
  mkFunPtr :: (Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))

foreign import ccall "&free_waveform"
  freeWaveformRaw :: FunPtr (Ptr Waveform -> IO ())

foreign import ccall "free_waveform"
  freeWaveformRaw2 :: Ptr Waveform -> IO ()

getTimes :: Waveform -> IO [Word64]
getTimes (Waveform fPtr) = do
  withForeignPtr fPtr $ \ptr -> do
    CSlice times count <- getTimesRaw ptr >>= peek
    -- finalizer <- mkFunPtr (touchForeignPtr fPtr)
    -- timesFPtr <- newForeignPtr finalizer times
    -- pure $ VS.unsafeFromForeignPtr0 timesFPtr (fromIntegral count)
    for [0 .. count - 1] $ \i -> do
      peekByteOff times (fromIntegral i * sizeOf (undefined :: Word64))

getTimeIndices :: Waveform -> IO [TimeIndex]
getTimeIndices waveform = do
  TimeIndexRange (TimeIndex min) (TimeIndex max) <- getTimeIndexRange waveform
  pure $ TimeIndex <$> [min .. max]

getTimeIndexRange :: Waveform -> IO TimeIndexRange
getTimeIndexRange (Waveform fPtr) = do
  withForeignPtr fPtr $ \ptr -> do
    CSlice _ count <- getTimesRaw ptr >>= peek
    pure $ TimeIndexRange (TimeIndex 0) (TimeIndex (fromIntegral count))

data TimeIndexRange = TimeIndexRange
  { min :: TimeIndex,
    max :: TimeIndex
  }
  deriving (Eq, Show, Ord)

stepTimeIndex :: TimeIndexRange -> TimeIndex -> Int -> Maybe TimeIndex
stepTimeIndex (TimeIndexRange (TimeIndex minTi) (TimeIndex maxTi)) (TimeIndex ti) s = do
  let ti' = ti + s
  guard $ ti' >= minTi
  guard $ ti' <= maxTi
  pure $ TimeIndex ti'

makeTimeIndex :: TimeIndexRange -> Int -> Maybe TimeIndex
makeTimeIndex (TimeIndexRange (TimeIndex minTi) (TimeIndex maxTi)) ti = do
  guard $ ti >= minTi
  guard $ ti <= maxTi
  pure $ TimeIndex ti

instance Storable SignalResult where
  sizeOf _ =
    sizeOf (undefined :: CIntPtr)
      + sizeOf (undefined :: Ptr Word8)
      + sizeOf (undefined :: CUIntPtr)

  alignment _ = alignment (undefined :: Ptr Word8)

  peek ptr = do
    t <- peekByteOff ptr 0
    w <- peekByteOff ptr (sizeOf (undefined :: CIntPtr))
    b <- peekByteOff ptr (sizeOf (undefined :: CIntPtr) + sizeOf (undefined :: Word64))
    l <- peekByteOff ptr (sizeOf (undefined :: CIntPtr) + sizeOf (undefined :: Word64) + sizeOf (undefined :: Ptr Word8))
    return $ SignalResult t w b l

  poke ptr (SignalResult t w b l) = do
    pokeByteOff ptr 0 t
    pokeByteOff ptr (sizeOf (undefined :: CIntPtr)) w
    pokeByteOff ptr (sizeOf (undefined :: CIntPtr) + sizeOf (undefined :: Word64)) b
    pokeByteOff ptr (sizeOf (undefined :: CIntPtr) + sizeOf (undefined :: Word64) + sizeOf (undefined :: Ptr Word8)) l

data SignalValue
  = ErrorValue String
  | BitsValue States Int BS.ByteString
  | StringValue BS.ByteString
  | RealValue Double

-- TODO: wellen does some special handling of the first byte
-- Maybe there is some special situation which can happen
instance Show SignalValue where
  show (ErrorValue reason) = "ErrorValue " ++ reason
  show (BitsValue states count bs) = decodeByteString states count bs
  show (StringValue str) = BS8.unpack str
  show (RealValue value) = show value

instance Num SignalValue where
  fromInteger n = fromJust $ makeBitsValue Two $ showIntAtBase 2 intToDigit n ""

-- (*) = _
-- abs = _
-- signum = _
-- negate = _

data ArithmeticOp = Plus | Minus | Multiply | Divide

execArithmeticOp :: ArithmeticOp -> Either Int Double -> Either Int Double -> Either Int Double
execArithmeticOp = undefined

intOrDoubleToSignalValue :: Either Int Double -> SignalValue
intOrDoubleToSignalValue (Left int) = fromIntegral int
intOrDoubleToSignalValue (Right double) = RealValue double

performArithmeticOp :: ArithmeticOp -> SignalValue -> SignalValue -> SignalValue
performArithmeticOp _ (ErrorValue reason) _ = ErrorValue reason
performArithmeticOp _ _ (ErrorValue reason) = ErrorValue reason
performArithmeticOp _ (StringValue _) _ = ErrorValue "Cannot perform arithmetic on strings"
performArithmeticOp _ _ (StringValue _) = ErrorValue "Cannot perform arithmetic on strings"
performArithmeticOp arop sv1@(BitsValue {}) sv2@(BitsValue {}) = fromMaybe (ErrorValue "Can only perform arithmetic on fully known strings") $ do
  n1 <- signalValueToInt sv1
  n2 <- signalValueToInt sv2
  pure $ intOrDoubleToSignalValue $ execArithmeticOp arop (Left n1) (Left n2)
performArithmeticOp arop (RealValue n1) sv2@(BitsValue {}) = fromMaybe (ErrorValue "Can only perform arithmetic on fully known strings") $ do
  n2 <- signalValueToInt sv2
  pure $ intOrDoubleToSignalValue $ execArithmeticOp arop (Right n1) (Left n2)
performArithmeticOp arop sv1@(BitsValue {}) (RealValue n2) = fromMaybe (ErrorValue "Can only perform arithmetic on fully known strings") $ do
  n1 <- signalValueToInt sv1
  pure $ intOrDoubleToSignalValue $ execArithmeticOp arop (Left n1) (Right n2)
performArithmeticOp arop (RealValue n1) (RealValue n2) =
  intOrDoubleToSignalValue $ execArithmeticOp arop (Right n1) (Right n2)

decodeByteString :: States -> Int -> BS.ByteString -> String
decodeByteString states count bs =
  foldr (\i s -> makeChar i : s) "" [byte0MissingDigits .. byte0MissingDigits + count - 1]
  where
    digits = digitsPerByte states
    bitsN = bitsForDigit states
    mask = case states of
      Two -> 0b1
      Four -> 0b11
      Nine -> 0b1111
    -- The leftmost byte might not contain the full N bits.
    -- Therefore, the left side might be 0 and we need to adapt the offset
    byte0MissingDigits =
      let x = (count - ((count `quot` digits) * digits))
       in if x == 0 then 0 else digits - x
    makeChar i =
      let word = BS.index bs (i `quot` digits)
          offset = bitsN * (digits - 1 - (i `mod` digits))
       in stateLookup states V.! fromIntegral (shiftR word offset .&. mask)

encodeByteString :: States -> String -> Maybe BS.ByteString
encodeByteString states string = do
  word8s <- startWord8s string
  pure $ BS.pack word8s
  where
    digits = digitsPerByte states
    size = bitsForDigit states
    count = length string
    -- The leftmost byte might not contain the full N bits.
    -- Therefore, we need to pad the left side
    byte0MissingDigits =
      let x = (count - ((count `quot` digits) * digits))
       in if x == 0 then 0 else digits - x
    startWord8s [] = pure []
    startWord8s xs = do
      if byte0MissingDigits > 0
        then makeWord8s $ replicate byte0MissingDigits '0' ++ xs
        else makeWord8s xs
    makeWord8s [] = pure []
    makeWord8s xs = do
      let (current, rest) = splitAt digits xs
      if length current == digits
        then do
          values <- traverse (`M.lookup` reverseStateLookup states) current
          let parts = zipWith (\offset value -> shiftL value offset) [0, size ..] $ reverse values
              w8 = foldl' (.|.) 0 parts
          (w8 :) <$> makeWord8s rest
        else makeWord8s $ xs ++ replicate (digits - length xs) '0'

signalValueToInt :: SignalValue -> Maybe Int
signalValueToInt bitsValue@(BitsValue states count bs) = do
  let bitString = show bitsValue
  guard $ all (`elem` ("01" :: String)) bitString
  pure $ readBinary bitString
  where
    readBinary = fst . head . readInt 2 (const True) digitToInt
signalValueToInt _ = Nothing

instance Eq SignalValue where
  sv1 == sv2 = show sv1 == show sv2

instance Ord SignalValue where
  sv1 `compare` sv2 = show sv1 `compare` show sv2

-- signalValuetoInt :: SignalValue -> Maybe Int
-- signalValuetoInt (sv) = do
--   let bits = show sv
--   guard $ all (\c -> c == '1' || c == '0') bits
--   pure bits

data States = Two | Four | Nine deriving (Eq, Show, Ord)

bitsForDigit :: States -> Int
bitsForDigit states = case states of
  Two -> 1
  Four -> 2
  Nine -> 4

digitsPerByte :: States -> Int
digitsPerByte states = 8 `quot` bitsForDigit states

makeBitsValue :: States -> String -> Maybe SignalValue
makeBitsValue states string = do
  bs <- encodeByteString states string
  pure $ BitsValue states (length string) bs

isTrue :: SignalValue -> Bool
isTrue sv = case show sv of
  ('1' : _) -> True
  _ -> False

isFalse :: SignalValue -> Bool
isFalse sv = case show sv of
  ('0' : _) -> True
  _ -> False

getSignal :: Waveform -> Signal -> TimeIndex -> S.Set TimeIndex -> IO SignalValue
getSignal waveform@(Waveform fPtr) (Signal offset (VarRef ref)) timeIndex@(TimeIndex rawIndex) indices
  | offset > 0 = case S.lookupGT timeIndex indices of
      Just newTimeIndex -> getSignal waveform (Signal (pred offset) (VarRef ref)) newTimeIndex indices
      Nothing -> pure $ ErrorValue $ "Cannot step forwards for time index " ++ show timeIndex
  | offset < 0 = case S.lookupLT timeIndex indices of
      Just newTimeIndex -> getSignal waveform (Signal (succ offset) (VarRef ref)) newTimeIndex indices
      Nothing -> pure $ ErrorValue $ "Cannot step backwards for time index " ++ show timeIndex
  | otherwise = do
      withForeignPtr fPtr $ \ptr -> do
        SignalResult signalType word dataPtr count <- getVarRaw ptr ref (fromIntegral rawIndex) >>= peek
        case signalType of
          -1 -> pure (ErrorValue "The signal does not have a value yet")
          0 -> do
            bs <- BS.unsafePackCStringFinalizer dataPtr (fromIntegral count) (touchForeignPtr fPtr)
            pure $ BitsValue Two (fromIntegral word) bs
          1 -> do
            bs <- BS.unsafePackCStringFinalizer dataPtr (fromIntegral count) (touchForeignPtr fPtr)
            pure $ BitsValue Four (fromIntegral word) bs
          2 -> do
            bs <- BS.unsafePackCStringFinalizer dataPtr (fromIntegral count) (touchForeignPtr fPtr)
            pure $ BitsValue Nine (fromIntegral word) bs
          3 -> do
            bs <- BS.unsafePackCStringFinalizer dataPtr (fromIntegral count) (touchForeignPtr fPtr)
            pure $ StringValue bs
          4 -> pure $ RealValue $ castWord64ToDouble word
          x -> error $ "Unsupported signal value: " ++ show x

stateLookup :: States -> V.Vector Char
stateLookup states = V.fromList $ case states of
  Two -> "01"
  Four -> "01xz"
  Nine -> "01xzhuwl-"

reverseStateLookup :: States -> M.Map Char Word8
reverseStateLookup states = M.fromList $ zip (V.toList $ stateLookup states) [0 ..]

-- Experiments show that loading signals one by one is extremely slow and impractical for real world usecases
test1 :: CUIntPtr -> IO ()
test1 x = loadFile "/home/simon/Downloads/ics-edu-rv32i-sc-new-01b6f7e339eb.fst" >>= \w -> loadVars w (VarRef <$> [5 .. x])

test2 :: CUIntPtr -> IO [()]
test2 x = loadFile "/home/simon/Downloads/ics-edu-rv32i-sc-new-01b6f7e339eb.fst" >>= \w -> traverse (\i -> loadVars w ([VarRef i])) [5 .. x]
