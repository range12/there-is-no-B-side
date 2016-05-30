module Main where

import System.Environment
import System.IO
import qualified Data.ByteString as B
import Control.Applicative
import Data.Yaml
import GenTM5Parser

import Data.IORef
import System.IO.Unsafe

refTM5Doc :: IORef TM5Doc
refTM5Doc = unsafeDupablePerformIO $ newIORef undefined

getDoc = unsafeDupablePerformIO $ readIORef refTM5Doc


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
    let eitherTm5 = decodeEither dump :: Either String TM5Doc
    case eitherTm5 of
        Left err -> putStrLn err
        Right doc -> writeIORef refTM5Doc doc >> print getDoc
