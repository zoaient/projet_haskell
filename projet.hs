module Main where

import Debug.Trace
import GHC.Generics
import Generic.Random
import Test.QuickCheck
import Data.Word (Word8,Word16)
import Data.Aeson (Value(Null))


main :: IO ()
main = do
  putStrLn "TP3 is running"
