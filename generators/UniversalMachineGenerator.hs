import System.Environment
import System.IO
import qualified Data.ByteString.Lazy as B
import Data.Yaml

-- module Main where

newtype Opts = Opts {
    hasAlphabet :: [String]
} deriving Show

skellFile = "tm5_skel.yml"

defaultAlphabet = (:[]) <$> ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0'.. '9']

makeReciprocal :: Functor f => f String -> f String
makeReciprocal = fmap ('~':)

-- getOpts :: Bool -> [String] -> IO Opts
-- getOpts [] = []
-- getOpts hasOpt (l:ls) = 

main = do
   dump <- B.readFile skellFile
   let eitherTm5 = parseEither ? dump
   B.putStrLn dump
