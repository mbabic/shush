{-# LANGUAGE OverloadedStrings #-}
module Shush.Parser.Wav where

import Prelude hiding (take)

import Control.Monad (fail, when)
import Data.ByteString (ByteString)
import Debug.Trace
import Numeric.Extra (intToFloat)
import Sound.OpenAL.AL.BasicTypes (ALsizei)

import Shush.Parser (expect, Parser, parse, string, take)
import Shush.Parser.Bytes (int16le, int32le)
import Shush.Sample

import qualified Sound.OpenAL.AL.Buffer   as ALB
import qualified Sound.OpenAL.ALC.Context as ALC

wav :: Parser Sample
wav = do
  _               <- string "RIFF"
  fsize           <- int32le
  _               <- string "WAVE"
  _               <- string "fmt "
  chunkSize       <- int32le
  -- Compression code. Expect and only support PCM.
  _               <- expect ((==) 1) (fromIntegral <$> int16le)
  channels        <- fromIntegral <$> int16le
  sampleFrequency <- fromIntegral <$> int32le
  avgBytesPerSec  <- fromIntegral <$> int32le
  bytesPerSlice   <- fromIntegral <$> int16le
  bitsPerSample   <- expect (\w -> (mod w 8) == 0 && w <= 64) (fromIntegral <$> int16le)
  _               <- string "data"
  dataSize        <- fromIntegral <$> int32le
  bytes           <- take dataSize
  sampleFormat    <- maybe (formatError channels bitsPerSample) return (mkFormat channels bitsPerSample)


  if (  avgBytesPerSec == sampleFrequency * bytesPerSlice
     && bytesPerSlice  == (div bitsPerSample 8) * channels
     )
    then return $
      Sample
        { sampleFormat    = trace ("format: " ++ show sampleFormat) sampleFormat
        , sampleFrequency = trace ("frequency: " ++ show sampleFrequency) intToFloat sampleFrequency
        , sampleSize      = trace ("size: " ++ show dataSize) fromIntegral dataSize
        , sampleData      = bytes
        }
    else fail $ "Invalid .wav header"

mkFormat :: Int -> Int -> Maybe ALB.Format
mkFormat 1  8 = Just ALB.Mono8
mkFormat 2  8 = Just ALB.Stereo8
mkFormat 1 16 = Just ALB.Mono16
mkFormat 2 16 = Just ALB.Stereo16
mkFormat _ _  = Nothing

formatError :: Int -> Int -> Parser a
formatError channels bitsPerSample =
  fail $
    "Unknown format. channels: " ++ show channels ++ ",  bit depth: " ++
    show bitsPerSample
