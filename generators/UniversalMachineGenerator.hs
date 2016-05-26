import System.Environment
import System.IO
import Control.Monad.Reader

module Main where

newtype Opts = Opts {
    hasAlphabet :: [String]
} deriving Show

skellFile = "machines/tm5_skel.yml"

defaultAlphabet = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0'.. '9']

makeReciprocal = fmap ('~':)

getOpts :: Bool -> [String] -> IO Opts
getOpts [] = []
getOpts hasOpt (l:ls) = 

main = do
    args <- getArgs >>= getOpts False
    let opts = 
