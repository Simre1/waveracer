{-# LANGUAGE RecursiveDo #-}

module Waveracer.Raw where

-- (Waveform, Signal, loadFile, lookupSignal, loadSignals, getTimes, getSignal)

import Control.Monad (forM, void)
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Traversable (for)
import Data.Vector.Storable qualified as V
import Data.Word
import Foreign (Storable (..))
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal (allocaArray)
import Foreign.Ptr
import GHC.Float (castWord64ToDouble)
import System.Mem.Weak
import Data.Foldable (toList)
import qualified Data.Vector.Storable as VS

foreign import ccall safe "wellen-binding.h load_file" loadFileRaw :: CString -> IO (Ptr Waveform)
foreign import ccall safe "wellen-binding.h load_vars" loadVarsRaw :: Ptr Waveform -> Ptr CUIntPtr -> CUIntPtr -> IO ()
foreign import ccall unsafe "wellen-binding.h unload_vars" unloadVarsRaw :: Ptr Waveform -> Ptr CUIntPtr -> CUIntPtr -> IO ()
foreign import ccall unsafe "wellen-binding.h lookup_var" lookupVarRaw :: Ptr Waveform -> CString -> IO (CIntPtr)
foreign import ccall unsafe "wellen-binding.h get_var" getVarRaw :: Ptr Waveform -> CUIntPtr -> Word32 -> IO (Ptr (SignalResult))
foreign import ccall unsafe "wellen-binding.h get_times" getTimesRaw :: Ptr Waveform -> IO (Ptr (CSlice Word64))

newtype Waveform = Waveform (ForeignPtr Waveform)

data VarRef = VarRef CUIntPtr deriving (Eq, Show, Ord)

newtype Signal = Signal VarRef deriving (Eq, Show, Ord)

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
    fPtr <- newForeignPtr freeWaveformRaw  waveformPtr  
       
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
    putStr "Unloading: "
    print signals
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
      pure $ Just $ Signal <$> refs
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

data TimeIndexRange = TimeIndexRange {
  min :: TimeIndex,
  max :: TimeIndex
} deriving (Eq, Show, Ord)

stepTimeIndex :: TimeIndexRange -> TimeIndex -> Int -> TimeIndex
stepTimeIndex (TimeIndexRange (TimeIndex minTi) (TimeIndex maxTi)) (TimeIndex ti) s =
  TimeIndex $ max minTi $ min maxTi (ti + s)

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
  = EmptyValue
  | TwoValues BS.ByteString Int
  | FourValues BS.ByteString Int
  | NineValues BS.ByteString Int
  | StringValue BS.ByteString
  | RealValue Double
  deriving (Eq, Ord)

-- TODO: wellen does some special handling of the first byte
-- Maybe there is some special situation which can happen
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
  show (StringValue str) = show str
  show (RealValue value) = show value

getSignal :: Waveform -> Signal -> TimeIndex -> IO SignalValue
getSignal (Waveform fPtr) (Signal (VarRef ref)) (TimeIndex timeIndex) = do
  withForeignPtr fPtr $ \ptr -> do
    SignalResult signalType word dataPtr count <- getVarRaw ptr ref (fromIntegral timeIndex) >>= peek
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
test1 x = loadFile "/home/simon/Downloads/ics-edu-rv32i-sc-new-01b6f7e339eb.fst" >>= \w -> loadVars w (VarRef <$> [5 .. x])

test2 :: CUIntPtr -> IO [()]
test2 x = loadFile "/home/simon/Downloads/ics-edu-rv32i-sc-new-01b6f7e339eb.fst" >>= \w -> traverse (\i -> loadVars w ([VarRef i])) [5 .. x]
