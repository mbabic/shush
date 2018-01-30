{-# LANGUAGE OverloadedStrings #-}

module Shush where

import Control.Concurrent(threadDelay)
import Control.Monad (when)
import Control.Monad.Catch (Exception(..), MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimState, PrimMonad)
import Data.Attoparsec.ByteString
import Data.Primitive.Addr (Addr(..))
import Data.Text hiding (length)
import Data.Typeable (Typeable(..))
import Data.Word (Word8)
import Debug.Trace
import GHC.Ptr (Ptr(..))
import Foreign.C.Types (CInt)
import Sound.OpenAL

import Shush.Parser (Parser, parse)
import Shush.Parser.Wav (wav)
import Shush.Noise
import Shush.Sample

import qualified Data.ByteString          as BS
import qualified Data.Primitive.ByteArray as BA
import qualified Sound.OpenAL.AL.Buffer   as ALB
import qualified Sound.OpenAL.ALC.Context as ALC
import qualified Sound.OpenAL.ALC.Device  as ALD
import qualified Sound.OpenAL.AL.Errors   as ALE
import qualified Sound.OpenAL.AL.Source   as ALS

newtype ErrorMsg =
  ErrorMsg
    { unErrorMsg :: Text }
  deriving (Show, Typeable)

instance Exception ErrorMsg

-- todo: inspect OpenAL error buffer
example :: IO ()
example = do
  device <- ALD.openDevice Nothing `orThrow` ErrorMsg "Could not open default device"
  ctx    <- ALC.createContext device [] `orThrow` ErrorMsg "Could not create context"
  ALC.currentContext $= Just ctx
  -- Source is a GeneratableObjectName
  src    <- genObjectName :: IO ALS.Source
  ALS.loopingMode src $= ALS.Looping
  ALS.sourceGain  src  $= 0.5

  -- "I don't know how to manage arrays and memory in Haskell yet"-strategy,
  -- with a running count of the number of array copyings that happen.
  --  - read in .wav as ByteString
  --  - parse into Sample data object                   (1 copy)
  --  - unpack ByteString into a [Word8]                (2 copies, at least)
  --  - allocate a PinnedByteArray
  --  - copy unpacked ByteString into PinnedByteArray   (3 copies)
  --  - create MemoryRegion from ptr to PinnedByteArray
  --  - create BufferData from MemoryRegion
  --  - pass BufferData to Buffer, attach Buffer to Source, play Source
  buf    <- genObjectName :: IO ALB.Buffer
  bs     <- BS.readFile "/Users/markobabic/src/shush/data/sample.wav"
  -- sample <-
  --   case parse wav bs of
  --     Done _ r     -> return r
  --     Fail _ _ msg -> fail $ "Failed. Msg: " ++ msg
  sample <- return $ whiteNoise 10000

  bytes  <- return $ BS.unpack $ sampleData sample
  ba     <- BA.newPinnedByteArray $ fromIntegral $ sampleSize sample
  fillArray ba bytes

  let (Addr addr) = BA.mutableByteArrayContents ba
      mem         = ALB.MemoryRegion (Ptr addr) (sampleSize sample)
      bd          = ALB.BufferData mem (sampleFormat sample) (sampleFrequency sample)

  bufferData buf $= bd
  ALS.buffer src $= Just buf

  play [src]
  waitWhilePlaying src

  closed <- ALD.closeDevice device
  return ()

fillArray :: PrimMonad m => BA.MutableByteArray (PrimState m) -> [Word8] -> m ()
fillArray mba words = fillArray' 0 words
  where
    -- fillArray' :: PrimMonad m => Int -> [Word8] -> m ()
    fillArray' offset (w : ws) = do
      BA.fillByteArray mba offset 1 w
      fillArray' (offset + 1) ws
    fillArray' _      []       = return ()

infixr 7 `orThrow`

-- | Sketchy convenience method to fail fast given a Maybe
orThrow :: MonadThrow m => m (Maybe a) -> ErrorMsg -> m a
orThrow mMbA msg = do
  mbA <- mMbA
  case mbA of
    Just a  -> return a
    Nothing -> throwM msg

-- | Wait until given Source is no longer in Playing state
waitWhilePlaying :: Source -> IO ()
waitWhilePlaying src = do
  state <- get (ALS.sourceState src)
  when (state == ALS.Playing) $ do
    -- 100 ms delay
    threadDelay 100000
    waitWhilePlaying src
