module Shush.Parser.Bytes
  ( int16le
  , int32le
  , expect
  ) where

import           Control.Monad (fail)
import qualified Data.Attoparsec.ByteString as P
import           Data.Bits ((.|.), shiftL)
import qualified Data.ByteString            as BS
import           Data.Int (Int16, Int32)
import           Data.Word (Word16, Word32)

type Parser = P.Parser

expect :: (Show a, Eq a) => (a -> Bool) -> Parser a -> Parser a
expect pred p= do
  result <- p
  if pred result
    then return result
    else fail $ "'" ++ show result ++ "' failed to satisfy predicate."

-- 16 bit combinators ----------------------------------------------------------
-- | Read 16 bit word in little endian format
word16le :: Parser Word16
word16le = do
  bytes <- P.take 2
  let b0 =  fromIntegral (bytes `BS.index` 0)
      b1 = (fromIntegral (bytes `BS.index` 1)) `shiftL` 8
  return $! b1 .|. b0

-- | Read 16 bit integer in little endian format
int16le :: Parser Int16
int16le = word16le >>= return . fromIntegral

-- 32 bit combinators ----------------------------------------------------------
-- | Read 32 bit word in little endian format
word32le :: Parser Word32
word32le = do
  bytes <- P.take 4
  let b0 =  fromIntegral (bytes `BS.index` 0)
      b1 = (fromIntegral (bytes `BS.index` 1)) `shiftL` 8
      b2 = (fromIntegral (bytes `BS.index` 2)) `shiftL` 16
      b3 = (fromIntegral (bytes `BS.index` 3)) `shiftL` 24
  return $! b3 .|. b2 .|. b1 .|. b0

-- | Read 32 bit integer in little endian format
int32le :: Parser Int32
int32le = word32le >>= return . fromIntegral
