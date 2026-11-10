-- Build me with: cabal build TP3.hs
-- Execute me with: cabal run -v0 TP3.hs
-- Load me in the REPL with: cabal repl TP3.hs, then use :r to reload the code upon changing

{- HLINT ignore -}

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

data IPV4Adress = MkIPV4Adress{
  first_part :: Word8,
  second_part :: Word8,
  third_part :: Word8,
  fourth_part :: Word8
}

data Port  = MkPort{
  port :: Word16
}

data Composed = MkComposed{
  port :: Port,
  ip :: IPV4Adress
}

isWord8 s = readMaybe s :: Maybe Int


readMaybeSafe :: forall a. (Bounded a, Integral a) => String -> Maybe a
readMaybeSafe str = case (readMaybe str :: Maybe Integer) of
  Nothing -> Nothing
  Just integer ->
    let minBoundAsInteger = fromIntegral (minBound :: a)
        maxBoundAsInteger = fromIntegral (maxBound :: a)
     in if integer >= minBoundAsInteger && integer <= maxBoundAsInteger
          then Just (fromIntegral integer)
          else Nothing
 
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim list =
  go delim list []
 where
  go _delim [] collected = [collected]
  go delim (x:xs) collected
    | x == delim = collected : go delim xs []
    | otherwise = go delim xs (collected ++ [x])

parseIPV4 :: String -> Either String IPV4Adress
parseIPV4 str =
  let wStrings = splitOn '.' str 
    in case (traverse (readMaybeSafe @Word8) wStrings) of
            Nothing -> Left "Some conversion failed"
            Just [x1,x2,x3,x4] -> Right $ IPV4Adress x1 x2 x3 x4 
            Just _ -> Left "Not the right number of Word8s"

parseIPV42 :: String -> Either String IPV4Adress
parseIPV42 str =
  let wStrings = splitOn '.' str 
      in case fmap (readMaybeSafe @Word8) wStrings of 
        [Just x1, Just x2 , Just x3 , Just x4] -> Right $ IPV4Adress x1 x2 x3 x4 
        _ -> Left "awouuuu"

parsePort :: String -> Either String Port
parsePort str = case readMaybeSafe str of
  Nothing -> Left "Port has invalid format"
  Just port -> Right $ Port port

parseIpv4Port :: String -> Either String Composed
parseIpv4Port str = case splitOn ':' str of 
  [ipStr, portStr] -> case parseIPV4 ipStr of
    Right ip -> case parsePort portStr of
      Right port -> Right $ Ipv4Port{.. ip , port}
      Left err -> Left("Port parsing failed " ++ err)
      _ -> Left "Invalid ip:port format"

instance Show IPV4Adress

parseIPV4Adress :: String -> Either String IPV4Adress
parseIPV4Adress s case (map readMaybeSafe (splitOn '.' s)) of
    |[Nothing,_,_,_] -> Left "A pas marché"
    |[_,Nothing,_,_] -> Left "A pas marché"
    |[_,_,Nothing,_] -> Left "A pas marché"
    |[_,_,_,Nothing] -> Left "A pas marché"    
    |[first,second,third,fourth] -> Right MkIPV4Adress(first,second,third,fourth)
    |_ -> Left "A po marché"



-- 1/ Define a type representing URLs you can enter in a browser address bar,
--    i.e. strings of the form:
--    - http://www.google.fr
--    - https://github.com/dmjio/miso/pulls
--    - http://reddit.com/r/haskell
-- 2/ Write a parser from String to your URL type. Its return type must be
--    Either String MyType. The 'Left' case is the error message. Write tests.
-- 3/ We now want to do URL filtering, to grant/forbid access to an URL
--    based on a policy. A filter is a string of the form:
--    - lemonde.fr/emploi
--    - reddit.com/**
--    - reddit.com/*/haskell/**
--    The second filter rules out all URLs of the form reddit.com/suffix, for any 'suffix'
--    The third filter rules out all URLs of the form reddit.com/whatever/haskell/suffix,
--    for any 'whatever' and any 'suffix'. In other words, '*' matches any single
--    segment of the URL while '**' matches all possibles suffixes.
--    word8 c style
--    Write a type for filters
-- 4/ Write a function that taskes an URL and a filter, and returns whether
--    the URL passes the filter. Write tests.
--
-- Use https://hoogle.haskell.org/ to find the functions you need, for example splitOn:
-- https://hackage.haskell.org/package/split-0.2.5/docs/Data-List-Split.html#v:splitOn