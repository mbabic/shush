{-# LANGUAGE OverloadedStrings #-}

module Shush.Noise where

import Control.Monad.Random
import Data.ByteString      (ByteString, pack)
import Data.Word

import Shush.Sample

import qualified Sound.OpenAL.AL.Buffer as ALB

-- | Generate white noise.
-- Whitenoise by definition contains all frequencies at a constant amplitude.
-- To generate whitenoise is very simple in theory: each sample of whitenoise
-- is a uniformly random value in the range from [-1.0 .. 1.0).
-- E.g., output[i] = randfloat() * 2.0 - 1.0
-- todo: interface should not be string of primitives
-- http://peabody.sapp.org/class/dmp2/lab/whitenoise/
whiteNoise
  :: Int         -- ^ Number of values to generate
  -> Sample
whiteNoise n =
  -- todo: actual seed
  let (sample, _) = runRand (genWhiteNoise n) (mkStdGen 123) in
  Sample
    { sampleData      = sample
    , sampleFormat    = ALB.Mono8
    -- frequency is probably your key to float vs. word 8 problem
    , sampleFrequency = 1000
    , sampleSize      = fromIntegral n
    }

genWhiteNoise :: RandomGen g => Int -> Rand g ByteString
genWhiteNoise n = do
  ns <- sequence (replicate n genRandom)
  return $ pack ns
  where
    genRandom :: RandomGen g => Rand g Word8
    genRandom = getRandomR (0, 255)
