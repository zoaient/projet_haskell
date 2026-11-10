
import Debug.Trace
import GHC.Generics
import Generic.Random
import Test.QuickCheck
import Data.Word (Word8,Word16)
import Data.Aeson (Value(Null))


data Domain = 
    Root String |
    Node String Domain
    deriving (Show)

data Path = 
    Root String |
    Node String Path
    deriving (Show)

data KeyVal = KeyVal String String
deriving (Show)

data Request = 
    Root KeyVal |
    Node KeyVal Request
 deriving (Show)


data URL MkURL {
    protocole :: String,
    domain :: Domain
    path :: Maybe Path
    request :: Maybe Request
    fragment :: Maybe String
}
deriving (Show)

main :: IO ()
main = putStrLn "Hello, Haskell!"

