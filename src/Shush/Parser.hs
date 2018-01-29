module Shush.Parser
  ( APS.IResult(..)
  , APS.parse
  , APS.Parser
  , APS.Result
  , APS.string
  , APS.take
  , expect
  ) where

import qualified Data.Attoparsec.ByteString as APS

expect :: (Show a, Eq a) => (a -> Bool) -> APS.Parser a -> APS.Parser a
expect pred p= do
  result <- p
  if pred result
    then return result
    else fail $ "'" ++ show result ++ "' failed to satisfy predicate."
