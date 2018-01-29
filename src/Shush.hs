{-# LANGUAGE OverloadedStrings #-}

module Shush where

import           Control.Concurrent (threadDelay)
import           Control.Monad (when)
import           Control.Monad.Catch (Exception(..), MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Primitive (PrimState, PrimMonad)
import           Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import           Data.Primitive.Addr (Addr(..))
import qualified Data.Primitive.ByteArray as BA
import           Data.Text hiding (length)
import           Data.Typeable (Typeable(..))
import           Data.Word (Word8)
import           Debug.Trace
import           GHC.Ptr (Ptr(..))
import           Foreign.C.Types (CInt)
import           Sound.OpenAL
import qualified Sound.OpenAL.AL.Buffer   as ALB
import qualified Sound.OpenAL.ALC.Context as ALC
import qualified Sound.OpenAL.ALC.Device  as ALD
import qualified Sound.OpenAL.AL.Errors   as ALE
import qualified Sound.OpenAL.AL.Source   as ALS
import           Shush.Parser.ByteParser  (Parser, parse)
import           Shush.Parser.WavParser

newtype ErrorMsg =
  ErrorMsg
    { unErrorMsg :: Text }
  deriving (Show, Typeable)

instance Exception ErrorMsg

-- todo: you should really be inspecting the error buffer on Nothing and just randomly
-- throwing a string, but that will come with time
example :: IO ()
example = do
  -- Open handle to the default device
  -- openDevice :: MonadIO m => Maybe String -> m (Maybe Device)
  device <- ALD.openDevice Nothing `orThrow` ErrorMsg "Could not open default device"
  traceM $ show device
  -- In order to render an audio scene, we need to create a context and
  -- initialize a context
  -- createContext :: MonadIO m => Device -> [ContextAttribute] -> m (Maybe Context)
  ctx    <- ALC.createContext device [] `orThrow` ErrorMsg "Could not create context"
  traceM $ show ctx
  
  -- currentContext :: StateVar (Maybe Context)
  ALC.currentContext $= Just ctx

  -- At this point we could define a listener, but we'll skip that for now.
  
  -- Source is a newtype around ALuint and has an instance of the typeclass
  -- "GeneratableObjectName" who's implementation defers to C OpenAL FFI to
  -- generate a source (id) for us to refer to and pass around
  -- class GeneratableObjectName a where
  --   genObjectName :: MonadIO m => m a
  -- instance GeneratableObjectName Source where
  --  genObjectNames n = ... some ffi
  src    <- genObjectName :: IO ALS.Source
  -- this is the default position but this is just for demonstration purposes
  -- ALS.sourcePosition src $= Vertex3 0 0 0
  ALS.loopingMode src    $= ALS.Looping
  ALS.sourceGain src     $= 0.5
  
  buf    <- genObjectName :: IO ALB.Buffer
  -- Read file as ByteString and parse out the wav format
  -- byteArrayContents :: ByteArray -> Addr, pass to Ptr constructor
  bs     <- BS.readFile "/Users/markobabic/src/shush/data/sample.wav"
  sample <-
    case parse wav bs of
      Done _ r     -> return r
      Fail _ _ msg -> fail $ "Failed. Msg: " ++ msg

  bytes  <- return $ BS.unpack $ sampleData sample
  -- newPinnedByteArray :: PrimMonad m => Int -> m (MutableByteArray (PrimState m))
  ba     <- BA.newPinnedByteArray $ fromIntegral $ sampleSize sample
  fillArray ba bytes

  let (Addr addr) = BA.mutableByteArrayContents ba
      mem         = ALB.MemoryRegion (Ptr addr) (sampleSize sample)
      bd          = ALB.BufferData mem (sampleFormat sample) (sampleFrequency sample)

  bufferData buf $= bd
  ALS.buffer src $= Just buf

  traceM $ "About to play ..."
  
  play [src]
  waitWhilePlaying src
  
  closed <- ALD.closeDevice device
  return ()

fillArray :: PrimMonad m => BA.MutableByteArray (PrimState m) -> [Word8] -> m ()
fillArray mba words = fillArray' 0 words
  where
    -- | fillArray' :: PrimMonad m => Int -> [Word8] -> m ()
    fillArray' offset (w : ws) = do
      BA.fillByteArray mba offset 1 w
      fillArray' (offset + 1) ws
    fillArray' _      []       = return ()

infixr 7 `orThrow`

-- | Sketchy onvenience method to fail fast on mebbes
orThrow :: MonadThrow m => m (Maybe a) -> ErrorMsg -> m a
orThrow mMbA msg = do
  mbA <- mMbA
  case mbA of
    Just a  -> return a
    Nothing -> throwM msg

waitWhilePlaying :: Source -> IO ()
waitWhilePlaying src = do
  state <- get (ALS.sourceState src)
  when (state == ALS.Playing) $ do
    -- 100 ms delay
    threadDelay 100000
    waitWhilePlaying src
