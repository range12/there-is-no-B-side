module Main where

import Turing
import Data.ByteString.Lazy as B(readFile)
import System.Environment
import Data.Aeson as Aeson
import qualified Data.Sequence as Seq

genInputSeq :: String -> Seq.Seq String
genInputSeq = undefined

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
    then do
        putStrLn "Error. Parameters!!"
        return ()
    else do
        myJson <- B.readFile $ args !! 0
        let parsed = Aeson.eitherDecode myJson :: Either String Machine
        case parsed of
            Right x -> return ()
            otherwise -> return ()
        return ()
