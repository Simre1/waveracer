module Waveracer.Raw where

-- (Waveform, VarRef, loadFile, lookupSignal, loadSignals, getTimes, getSignal)

import Control.Monad (forM, void)
import Data.Bits
import Data.Bits (Bits (testBit))
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Coerce (coerce)
import Data.Traversable (for)
import Data.Vector.Storable qualified as V
import Data.Word
import Foreign (Storable (..))
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal (allocaArray)
import Foreign.Marshal.Alloc (finalizerFree)
import Foreign.Ptr
import GHC.Float (castWord64ToDouble)
import System.Mem.Weak

newtype Waveform = Waveform (ForeignPtr Waveform)

newtype VarRef = VarRef CUIntPtr deriving (Eq, Show, Ord)

data CSlice a = CSlice
  { csPtr :: Ptr a,
    csLength :: CUIntPtr
  }

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

foreign import ccall "wellen-binding.h load_file" loadFileRaw :: CString -> IO (Ptr Waveform)

loadFile :: String -> IO Waveform
loadFile str = do
  withCString str $ \cString -> do
    waveformPtr <- loadFileRaw cString
    Waveform <$> newForeignPtr finalizerFree waveformPtr

foreign import ccall "wellen-binding.h load_signals" loadSignalsRaw :: Ptr Waveform -> Ptr CUIntPtr -> CUIntPtr -> IO ()

foreign import ccall "wellen-binding.h unload_signals" unloadSignalsRaw :: Ptr Waveform -> Ptr CUIntPtr -> CUIntPtr -> IO ()

loadSignals :: Waveform -> [VarRef] -> IO ()
loadSignals waveform@(Waveform fPtr) signals = do
  let count = length signals
  allocaArray count $ \signalsPtr -> do
    void $ forM (zip [0 ..] signals) $ \(offset, ref@(VarRef cInt)) -> do
      addFinalizer ref (unloadSignals waveform [ref])
      pokeByteOff signalsPtr (offset * sizeOf (undefined :: CUIntPtr)) cInt
    withForeignPtr fPtr $ \ptr -> do
      loadSignalsRaw ptr signalsPtr (fromIntegral count)

unloadSignals :: Waveform -> [VarRef] -> IO ()
unloadSignals (Waveform fPtr) signals = do
  let count = length signals
  allocaArray count $ \signalsPtr -> do
    void $ forM (zip [0 ..] signals) $ \(offset, (VarRef cInt)) -> do
      poke (signalsPtr `plusPtr` offset) cInt
    withForeignPtr fPtr $ \ptr -> do
      unloadSignalsRaw ptr signalsPtr (fromIntegral count)

foreign import ccall "wellen-binding.h lookup_signal" lookupSignalRaw :: Ptr Waveform -> CString -> IO (CIntPtr)

lookupSignal :: Waveform -> String -> IO (Maybe VarRef)
lookupSignal (Waveform fPtr) name = do
  let count = length name
  withCString name $ \cString -> do
    withForeignPtr fPtr $ \ptr -> do
      cInt <- lookupSignalRaw ptr cString
      pure $
        if cInt >= 0
          then Just $ VarRef (fromIntegral cInt)
          else Nothing

foreign import ccall "wellen-binding.h get_times" getTimesRaw :: Ptr Waveform -> IO (Ptr (CSlice Word64))

getTimes :: Waveform -> IO [Word64]
getTimes (Waveform fPtr) = do
  withForeignPtr fPtr $ \ptr -> do
    CSlice times count <- getTimesRaw ptr >>= peek
    for [0 .. count - 1] $ \i -> do
      peekByteOff times (fromIntegral i * sizeOf (undefined :: Word64))

data SignalResult = SignalResult
  { signalType :: CIntPtr,
    word :: Word64,
    bytes :: Ptr Word8,
    length :: CUIntPtr
  }
  deriving (Show)

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

foreign import ccall "wellen-binding.h get_signal" getSignalRaw :: Ptr Waveform -> CUIntPtr -> Word32 -> IO (Ptr (SignalResult))

data SignalValue
  = EmptyValue
  | TwoValues BS.ByteString Int
  | FourValues BS.ByteString Int
  | NineValues BS.ByteString Int
  | StringValue BS.ByteString
  | RealValue Double
  deriving (Eq, Ord)

instance Show SignalValue where
  show EmptyValue = ""
  show (TwoValues bs count) = foldr (\i s -> makeTwoChar i : s) "" [0 .. count - 1]
    where
      makeTwoChar i =
        let word = BS.index bs (i `quot` 8)
            offset = (i `mod` 8)
         in twoStateLookup V.! fromIntegral (shiftR word offset .&. 0b1)
  show (FourValues bs count) = foldr (\i s -> makeTwoChar i : s) "" [0 .. count - 1]
    where
      makeTwoChar i =
        let word = BS.index bs (i `quot` 4)
            offset = 2 * (i `mod` 4)
         in twoStateLookup V.! fromIntegral (shiftR word offset .&. 0b11)
  show (NineValues bs count) = foldr (\i s -> makeNineChar i : s) "" [0 .. count - 1]
    where
      makeNineChar i =
        let word = BS.index bs (i `quot` 2)
            offset = 4 * (i `mod` 2)
         in nineStateLookup V.! fromIntegral (shiftR word offset .&. 0b1111)

getSignal :: Waveform -> VarRef -> Int -> IO SignalValue
getSignal (Waveform fPtr) (VarRef ref) timeIndex = do
  withForeignPtr fPtr $ \ptr -> do
    SignalResult signalType word dataPtr count <- getSignalRaw ptr ref (fromIntegral timeIndex) >>= peek
    case signalType of
      -1 -> pure EmptyValue
      0 -> do
        bs <- BS.unsafePackCStringFinalizer dataPtr (fromIntegral count) (touchForeignPtr fPtr)
        pure $ TwoValues bs (fromIntegral word)
      1 -> do
        bs <- BS.unsafePackCStringFinalizer dataPtr (fromIntegral count) (touchForeignPtr fPtr)
        pure $ FourValues bs (fromIntegral word)
      2 -> do
        bs <- BS.unsafePackCStringFinalizer dataPtr (fromIntegral count) (touchForeignPtr fPtr)
        pure $ NineValues bs (fromIntegral word)
      3 -> do
        bs <- BS.unsafePackCStringFinalizer dataPtr (fromIntegral count) (touchForeignPtr fPtr)
        pure $ StringValue bs
      4 -> pure $ RealValue $ castWord64ToDouble word
      x -> error $ "Unsupported signal value: " ++ show x

twoStateLookup :: V.Vector Char
twoStateLookup = V.fromList "01"

fourStateLookup :: V.Vector Char
fourStateLookup = V.fromList "01xz"

nineStateLookup :: V.Vector Char
nineStateLookup = V.fromList "01xzhuwl-"

-- Experiments show that loading signals one by one is extremely slow and impractical for real world usecases
test1 :: CUIntPtr -> IO ()
test1 x = loadFile "/home/simon/Downloads/ics-edu-rv32i-sc-new-01b6f7e339eb.fst" >>= \w -> Waveracer.Raw.loadSignals w (VarRef <$> [5 .. x])

test2 :: CUIntPtr -> IO [()]
test2 x = loadFile "/home/simon/Downloads/ics-edu-rv32i-sc-new-01b6f7e339eb.fst" >>= \w -> traverse (\i -> Waveracer.Raw.loadSignals w ([VarRef i])) [5 .. x]
