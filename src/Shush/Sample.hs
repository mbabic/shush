module Shush.Sample
  ( Sample(..)
  ) where

import Data.ByteString (ByteString)
import Sound.OpenAL.AL.BasicTypes (ALsizei)

import qualified Sound.OpenAL.AL.Buffer   as ALB
import qualified Sound.OpenAL.ALC.Context as ALC

-- todo: this will choke on big files, lots of copying happening
-- todo: is this really not going to be polymorphic at all?
-- todo: depend on naked OpenAL types?
data Sample
  = Sample
  { sampleData      :: ByteString    -- ^ The data associated with the sample
  , sampleFormat    :: ALB.Format    -- ^ Sound format. E.g., Mono8
  , sampleFrequency :: ALC.Frequency -- ^ Frequency for mixing output buffer in Hz.
  , sampleSize      :: ALsizei       -- ^ Sample size in bytes.
  }
