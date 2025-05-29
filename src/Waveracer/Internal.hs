{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ViewPatterns #-}

module Waveracer.Internal where

-- (Waveform, Signal, loadFile, lookupSignal, loadSignals, getTimes, getSignal)

import Control.Monad (forM, guard, join, void, when)
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Internal qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Char (intToDigit, ord)
import Data.Either (partitionEithers)
import Data.Foldable (for_, toList)
import Data.IORef
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Ratio
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Internal.Read
import Data.Text.Internal.StrictBuilder qualified as SB
import Data.Traversable (for)
import Data.Vector.Storable qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word
import Debug.Trace
import Foreign (Storable (..))
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal (alloca, allocaArray)
import Foreign.Ptr
import GHC.Float (castWord64ToDouble)
import Numeric (readInt, showIntAtBase)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.Weak

foreign import ccall safe "wellen-binding.h load_file" loadFileRaw :: CString -> IO (Ptr Waveform)

foreign import ccall safe "wellen-binding.h load_vars" loadVarsRaw :: Ptr Waveform -> Ptr CUIntPtr -> CUIntPtr -> IO ()

foreign import ccall unsafe "wellen-binding.h unload_vars" unloadVarsRaw :: Ptr Waveform -> Ptr CUIntPtr -> CUIntPtr -> IO ()

foreign import ccall unsafe "wellen-binding.h lookup_var" lookupVarRaw :: Ptr Waveform -> Ptr Text -> CUIntPtr -> IO (CIntPtr)

foreign import ccall unsafe "wellen-binding.h get_var" getVarRaw :: Ptr Waveform -> CUIntPtr -> Word32 -> Ptr SignalResult -> IO ()

foreign import ccall unsafe "wellen-binding.h get_times" getTimesRaw :: Ptr Waveform -> IO (Ptr (CSlice Word64))

foreign import ccall unsafe "wellen-binding.h get_timescale" getTimescaleRaw :: Ptr Waveform -> IO CIntPtr

withUtf8 :: Text -> (Ptr Text -> Int -> IO a) -> IO a
withUtf8 txt action =
  let bs = T.encodeUtf8 txt
   in BS.unsafeUseAsCString bs (\ptr -> action (castPtr ptr) (BS.length bs))

data Waveform = Waveform
  { fPtr :: ForeignPtr Waveform,
    activeVars :: IORef (M.Map Text (Weak VarRef)),
    loadQueue :: IORef (S.Set VarRef)
  }

data VarRef = VarRef CUIntPtr deriving (Eq, Show, Ord)

data Signal = Signal {varRef :: VarRef} deriving (Eq, Show, Ord)

newtype TimeIndex = TimeIndex Int deriving (Eq, Show, Ord)

data TimeIndexRange = TimeIndexRange
  { min :: TimeIndex,
    max :: TimeIndex
  }
  deriving (Eq, Show, Ord)

data TimeUnit
  = FemtoSeconds
  | PicoSeconds
  | NanoSeconds
  | MicroSeconds
  | MilliSeconds
  | Seconds
  | Unknown
  deriving (Eq, Ord, Show)

data Timescale = Timescale
  { factor :: Word32,
    unit :: TimeUnit
  }
  deriving (Eq, Ord, Show)

data TraceTime = TraceTime {time :: Word64, scale :: Timescale}
  deriving (Eq, Ord, Show)

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

loadWaveformFile :: FilePath -> IO Waveform
loadWaveformFile str = do
  withCString str $ \cString -> do
    waveformPtr <- loadFileRaw cString
    fPtr <- newForeignPtr freeWaveformRaw waveformPtr
    activeSignals <- newIORef M.empty
    loadQueue <- newIORef S.empty

    pure $ Waveform fPtr activeSignals loadQueue

loadVars :: Waveform -> [VarRef] -> IO ()
loadVars waveform vars = do
  let count = length vars
  allocaArray count $ \signalsPtr -> do
    void $ forM (zip [0 ..] vars) $ \(offset, ref@(VarRef cInt)) -> mdo
      weak <- mkWeak ref waveform $ Just $ do
        maybeWaveform <- deRefWeak weak
        case maybeWaveform of
          Just aliveWaveform -> unloadVars aliveWaveform [ref]
          Nothing -> pure ()
      pokeByteOff signalsPtr (offset * sizeOf (undefined :: CUIntPtr)) cInt
    withForeignPtr waveform.fPtr $ \ptr -> do
      loadVarsRaw ptr signalsPtr (fromIntegral count)

unloadVars :: Waveform -> [VarRef] -> IO ()
unloadVars waveform signals = do
  let count = length signals
  allocaArray count $ \signalsPtr -> do
    void $ forM (zip [0 ..] signals) $ \(offset, (VarRef cInt)) -> do
      pokeByteOff signalsPtr (offset * sizeOf (undefined :: CUIntPtr)) cInt
    withForeignPtr waveform.fPtr $ \ptr -> do
      unloadVarsRaw ptr signalsPtr (fromIntegral count)
    void $ putStrLn "Unloading done"

lookupSignal :: Waveform -> Text -> IO (Maybe Signal)
lookupSignal waveform name = fmap Signal <$> lookupVar waveform name

lookupActiveVar :: Waveform -> Text -> IO (Maybe VarRef)
lookupActiveVar waveform string = do
  activeVars <- readIORef waveform.activeVars
  case M.lookup string activeVars of
    Nothing -> pure Nothing
    Just weak -> do
      maybeVar <- deRefWeak weak
      case maybeVar of
        Nothing -> do
          modifyIORef' waveform.activeVars (M.delete string)
          pure Nothing
        Just var -> pure $ Just var

lookupVar :: Waveform -> Text -> IO (Maybe VarRef)
lookupVar waveform name = do
  activeVar <- lookupActiveVar waveform name
  case activeVar of
    Just var -> pure $ Just var
    Nothing -> do
      withUtf8 name $ \utf8 size -> do
        withForeignPtr waveform.fPtr $ \ptr -> do
          cInt <- lookupVarRaw ptr utf8 (fromIntegral size)
          if cInt >= 0
            then do
              let var = VarRef (fromIntegral cInt)
              enqueueVar waveform var
              pure $ Just var
            else pure Nothing

foreign import ccall "wrapper"
  mkFunPtr :: (Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))

foreign import ccall "&free_waveform"
  freeWaveformRaw :: FunPtr (Ptr Waveform -> IO ())

foreign import ccall "free_waveform"
  freeWaveformRaw2 :: Ptr Waveform -> IO ()

getTraceTime :: Waveform -> TimeIndex -> IO TraceTime
getTraceTime waveform (TimeIndex ti) = do
  scale <- getTimescale waveform
  withForeignPtr waveform.fPtr $ \ptr -> do
    CSlice times count <- getTimesRaw ptr >>= peek
    when (ti >= fromIntegral count) $
      fail "That time index is too great"
    time <- peekByteOff times (ti * sizeOf (undefined :: Word64))
    pure $ TraceTime time scale

-- finalizer <- mkFunPtr (touchForeignPtr fPtr)
-- timesFPtr <- newForeignPtr finalizer times
-- pure $ VS.unsafeFromForeignPtr0 timesFPtr (fromIntegral count)

getTimes :: Waveform -> IO [Word64]
getTimes waveform = do
  withForeignPtr waveform.fPtr $ \ptr -> do
    CSlice times count <- getTimesRaw ptr >>= peek
    -- finalizer <- mkFunPtr (touchForeignPtr fPtr)
    -- timesFPtr <- newForeignPtr finalizer times
    -- pure $ VS.unsafeFromForeignPtr0 timesFPtr (fromIntegral count)
    for [0 .. count - 1] $ \i -> do
      peekByteOff times (fromIntegral i * sizeOf (undefined :: Word64))

getTimescale :: Waveform -> IO Timescale
getTimescale waveform =
  withForeignPtr waveform.fPtr $ \ptr -> do
    timescale_int <- getTimescaleRaw ptr
    let factor = fromIntegral $ shiftR timescale_int 32
        unit_int = timescale_int .&. 0xFFFFFFFF
        unit = case unit_int of
          1 -> FemtoSeconds
          2 -> PicoSeconds
          3 -> NanoSeconds
          4 -> MicroSeconds
          5 -> MilliSeconds
          6 -> Seconds
          _ -> Unknown
    pure $ Timescale {unit, factor}

getTimeIndices :: Waveform -> IO [TimeIndex]
getTimeIndices waveform = do
  TimeIndexRange (TimeIndex min) (TimeIndex max) <- getTimeIndexRange waveform
  pure $ TimeIndex <$> [min .. max]

getTimeIndexRange :: Waveform -> IO TimeIndexRange
getTimeIndexRange waveform = do
  withForeignPtr waveform.fPtr $ \ptr -> do
    CSlice _ count <- getTimesRaw ptr >>= peek
    pure $ TimeIndexRange (TimeIndex 0) (TimeIndex (fromIntegral (count - 1)))

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
  = TextValue !Text
  | RealValue !Double
  | BitsValue States Int !BS.ByteString
  deriving (Eq, Ord)

-- TODO: wellen does some special handling of the first byte
-- Maybe there is some special situation which can happen
instance Show SignalValue where
  show (BitsValue states count bs) = BS8.unpack $ decodeByteStringToUtf8 states count bs
  show (TextValue text) = T.unpack text
  show (RealValue value) = show value

-- instance Num SignalValue where
--   fromInteger n = IntegerValue n
--   sv1 + sv2 = performBinaryOp Plus sv1 sv2
--   sv1 - sv2 = performBinaryOp Minus sv1 sv2
--   sv1 * sv2 = performBinaryOp Multiply sv1 sv2
--   signum sv = performUnaryOp Signum sv
--   negate sv = performUnaryOp Negate sv
--   abs sv = performUnaryOp Abs sv

-- instance Fractional SignalValue where
--   fromRational rational =
--     if denominator rational == 1
--       then fromIntegral (numerator rational)
--       else RealValue (fromRational rational)
--   sv1 / sv2 = performBinaryOp Divide sv1 sv2

data UnaryOp
  = Signum
  | Abs
  | Negate

data BinaryOp
  = Plus
  | Minus
  | Multiply
  | Divide

-- execArithmeticOp :: BinaryOp -> Either Integer Double -> Either Integer Double -> Either Integer Double
-- execArithmeticOp = undefined

-- intOrDoubleToSignalValue :: Either Integer Double -> SignalValue
-- intOrDoubleToSignalValue (Left int) = fromIntegral int
-- intOrDoubleToSignalValue (Right double) = RealValue double

-- performUnaryOp :: UnaryOp -> SignalValue -> SignalValue
-- -- performUnaryOp op (ByteStringValue str) = ErrorValue "Cannot perform arithmetic on strings"
-- performUnaryOp op (ErrorValue reason) = (ErrorValue reason)
-- performUnaryOp op (RealValue double) = case op of
--   Signum ->
--     if
--       | double > 0 -> 1
--       | double < 0 -> -1
--       | otherwise -> 0
--   Abs -> RealValue (abs double)
--   Negate -> RealValue (negate double)
-- performUnaryOp op sv@(IntegerValue int) = IntegerValue $
--   case op of
--     Signum -> signum int
--     Abs -> abs int
--     Negate -> negate int
-- performUnaryOp op sv@(TextValue _) = ErrorValue "Cannot do arithmetic on text"
-- performUnaryOp op sv@(BitsValue _) = ErrorValue "Cannot do arithmetic on partially unknown bitstrings"

-- performBinaryOp :: BinaryOp -> SignalValue -> SignalValue -> SignalValue
-- performBinaryOp _ (ErrorValue reason) _ = ErrorValue reason
-- performBinaryOp _ _ (ErrorValue reason) = ErrorValue reason
-- performBinaryOp arop (IntegerValue n1) (IntegerValue n2) = intOrDoubleToSignalValue $ execArithmeticOp arop (Left n1) (Left n2)
-- performBinaryOp arop (IntegerValue n1) (RealValue n2) = intOrDoubleToSignalValue $ execArithmeticOp arop (Left n1) (Right n2)
-- performBinaryOp arop (RealValue n1) (IntegerValue n2) = intOrDoubleToSignalValue $ execArithmeticOp arop (Right n1) (Left n2)
-- performBinaryOp arop (RealValue n1) (RealValue n2) = intOrDoubleToSignalValue $ execArithmeticOp arop (Right n1) (Right n2)
-- performBinaryOp _ (TextValue _) _ = ErrorValue $ "Cannot do arithmetic on text"
-- performBinaryOp _ _ (TextValue _) = ErrorValue $ "Cannot do arithmetic on text"
-- performBinaryOp _ _ (BitsValue _) = ErrorValue $ "Cannot do arithmetic on partially unknown bitstrings"
-- performBinaryOp _ (BitsValue _) _ = ErrorValue $ "Cannot do arithmetic on partially unknown bitstrings"

-- instance Ord SignalValue where
--   sv1 `compare` sv2 = show sv1 `compare` show sv2
--

decodeInt :: SignalValue -> Maybe Int
decodeInt (BitsValue states count bs) = decodeByteStringToIntegral states count bs
decodeInt _ = Nothing

decodeInteger :: SignalValue -> Maybe Integer
decodeInteger (BitsValue states count bs) = decodeByteStringToIntegral states count bs
decodeInteger _ = Nothing

decodeIntegral :: (Bits a, Integral a) => SignalValue -> Maybe a
decodeIntegral (BitsValue states count bs) = decodeByteStringToIntegral states count bs
decodeIntegral _ = Nothing

decodeWord :: SignalValue -> Maybe Word
decodeWord (BitsValue states count bs) = decodeByteStringToIntegral states count bs
decodeWord _ = Nothing

decodeText :: SignalValue -> Maybe Text
decodeText (TextValue text) = Just text
decodeText _ = Nothing

decodeDouble :: SignalValue -> Maybe Double
decodeDouble (RealValue double) = Just double
decodeDouble _ = Nothing

decodeBitString :: SignalValue -> Maybe Text
decodeBitString (BitsValue states count bs) = Just (decodeByteStringToText states count bs)
decodeBitString _ = Nothing

encodeText :: Text -> SignalValue
encodeText = TextValue

encodeDouble :: Double -> SignalValue
encodeDouble = RealValue

encodeBitString :: Text -> Maybe SignalValue
encodeBitString text = do
  states <- determineEncoding text
  BitsValue states (T.length text) <$> encodeBitstringToByteString states text

-- TODO: VERY SLOW
encodeInt :: Int -> SignalValue
encodeInt i = fromJust $ encodeBitString (T.pack $ showIntAtBase 2 digitToChar i "")
  where
    digitToChar :: Int -> Char
    digitToChar 0 = '0'
    digitToChar 1 = '1'

-- encodeInt :: Int -> SignalValue
-- encodeInt 0 = BitsValue Two 1 (BS.pack [0])
-- encodeInt int =
--   let count = finiteBitSize int - countLeadingZeros int
--       bs = BS.unsafeCreate (sizeOf (undefined :: Int)) $ \ptr -> poke ptr int
--    in BitsValue countTwo count bs

-- decodeInteger :: SignalValue -> Maybe Int
-- decodeInteger (Integer) = decodeByteStringToInt states count bs
-- decodeInteger _ = Nothing

data States = Two | Four | Nine deriving (Eq, Show, Ord)

bitsForDigit :: States -> Int
bitsForDigit states = case states of
  Two -> 1
  Four -> 2
  Nine -> 4

digitsPerByte :: States -> Int
digitsPerByte states = 8 `quot` bitsForDigit states

loadQueuedSignals :: Waveform -> IO ()
loadQueuedSignals waveform = do
  queue <- atomicModifyIORef' waveform.loadQueue (\q -> (S.empty, q))
  loadVars waveform (S.toList queue)

enqueueVar :: Waveform -> VarRef -> IO ()
enqueueVar waveform ref = modifyIORef' waveform.loadQueue (S.insert ref)

getSignal :: Waveform -> Signal -> TimeIndex -> IO SignalValue
getSignal waveform (Signal (VarRef ref)) (TimeIndex rawIndex) =
  withForeignPtr waveform.fPtr $ \ptr -> alloca $ \resultPtr -> do
    getVarRaw ptr ref (fromIntegral rawIndex) resultPtr
    SignalResult signalType word dataPtr count <- peek resultPtr
    case signalType of
      -1 -> fail "The signal does not have a value yet"
      0 -> do
        bs <- BS.unsafePackCStringFinalizer dataPtr (fromIntegral count) (touchForeignPtr waveform.fPtr)
        pure $ BitsValue Two (fromIntegral count) bs
      1 -> do
        bs <- BS.unsafePackCStringFinalizer dataPtr (fromIntegral count) (touchForeignPtr waveform.fPtr)
        pure $ BitsValue Four (fromIntegral count) bs
      2 -> do
        bs <- BS.unsafePackCStringFinalizer dataPtr (fromIntegral count) (touchForeignPtr waveform.fPtr)
        pure $ BitsValue Four (fromIntegral count) bs
      3 -> do
        bs <- BS.unsafePackCStringFinalizer dataPtr (fromIntegral count) (touchForeignPtr waveform.fPtr)
        pure $ TextValue $ T.decodeUtf8 bs
      4 -> pure $ RealValue $ castWord64ToDouble word
      x -> error $ "Unsupported signal value: " ++ show x

-- TODO: 4x speed via Word64
decodeByteStringBitsToInteger :: BS.ByteString -> Integer
decodeByteStringBitsToInteger = BS.foldl' step 0
  where
    step acc byte = (acc `shiftL` 8) .|. toInteger byte

decodeByteStringBitsToIntegral :: (Bits a, Integral a) => BS.ByteString -> a
decodeByteStringBitsToIntegral = BS.foldl' step 0
  where
    step acc byte = (acc `shiftL` 8) .|. fromIntegral byte
{-# INLINE decodeByteStringBitsToIntegral #-}

decodeByteStringToInteger :: States -> Int -> BS.ByteString -> Maybe Integer
decodeByteStringToInteger Two _ bs = Just $ decodeByteStringBitsToInteger bs
decodeByteStringToInteger states count bs =
  foldByteStringRight
    ( \acc c -> case c of
        48 -> (* 2) <$> acc -- 0
        49 -> (\a -> a * 2 + 1) <$> acc -- 1
        _ -> Nothing
    )
    (pure 0)
    states
    count
    bs

decodeByteStringToIntegral :: (Bits a, Integral a) => States -> Int -> BS.ByteString -> Maybe a
decodeByteStringToIntegral Two _ bs = Just $ decodeByteStringBitsToIntegral bs
decodeByteStringToIntegral states count bs =
  foldByteStringRight
    ( \acc c -> case c of
        48 -> (* 2) <$> acc
        49 -> (\a -> a * 2 + 1) <$> acc
        _ -> Nothing
    )
    (pure 0)
    states
    count
    bs

foldByteStringRight :: (b -> Word8 -> b) -> b -> States -> Int -> BS.ByteString -> b
foldByteStringRight f b states count bs =
  foldl' (\b -> f b . makeChar) b [byte0MissingDigits .. byte0MissingDigits + count - 1]
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
{-# INLINE foldByteStringRight #-}

foldByteStringLeft :: (Word8 -> b -> b) -> b -> States -> Int -> BS.ByteString -> b
foldByteStringLeft f b states count bs =
  foldl' (\b i -> f (makeChar i) b) b [byte0MissingDigits + count - 1, byte0MissingDigits + count - 1 .. byte0MissingDigits]
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
{-# INLINE foldByteStringLeft #-}

decodeByteStringToText :: States -> Int -> BS.ByteString -> Text
decodeByteStringToText states count bs = SB.toText $ SB.unsafeFromByteString $ decodeByteStringToUtf8 states count bs

decodeByteStringToUtf8 :: States -> Int -> BS.ByteString -> BS.ByteString
decodeByteStringToUtf8 states count bs =
  BS.unsafeCreate count $ \ptr -> do
    for_ [0 .. count - 1] $ \i -> do
      pokeByteOff ptr i (makeWord8 (i + byte0MissingDigits))
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
    makeWord8 i =
      let word = BS.index bs (i `quot` digits)
          offset = bitsN * (digits - 1 - (i `mod` digits))
       in stateLookup states V.! fromIntegral (shiftR word offset .&. mask)

encodeBitstringToByteString :: States -> Text -> Maybe BS.ByteString
encodeBitstringToByteString states (T.unpack -> string) = do
  word8s <- startWord8s (fmap charToWord8 string)
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
        then makeWord8s $ replicate byte0MissingDigits zeroWord8 ++ xs
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
        else makeWord8s $ xs ++ replicate (digits - length xs) zeroWord8

determineEncoding :: Text -> Maybe States
determineEncoding str
  | T.all (\c -> charToWord8 c `V.elem` (stateLookup Two)) str = pure Two
  | T.all (\c -> charToWord8 c `V.elem` (stateLookup Four)) str = pure Four
  | T.all (\c -> charToWord8 c `V.elem` (stateLookup Nine)) str = pure Nine
  | otherwise = Nothing

stateLookup :: States -> V.Vector Word8
stateLookup states = V.fromList $ case states of
  Two -> charToWord8 <$> "01"
  Four -> charToWord8 <$> "01xz"
  Nine -> charToWord8 <$> "01xzhuwl-"

oneWord8 = charToWord8 '1'

zeroWord8 = charToWord8 '0'

charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

word8ToChar :: Word8 -> Char
word8ToChar = toEnum . fromEnum

reverseStateLookup :: States -> M.Map Word8 Word8
reverseStateLookup states = M.fromList $ zip (V.toList $ stateLookup states) [0 ..]
