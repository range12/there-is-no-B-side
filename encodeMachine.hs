module Main where

import Turing
import qualified Data.ByteString.Lazy as B(readFile)
import qualified Data.Aeson as Aeson
import System.Environment(getArgs)
import Prelude hiding (read)
import qualified Data.Map.Strict as Map


encodeTransitions :: Map.Map String [Transition] -> String
encodeTransitions = Map.foldrWithKey encodeTransLst  "" where
    encodeTransLst state ts acc =
        foldr (\x y -> y ++ "&TRANS" ++ state ++ read x ++ to_state x ++ write x) acc ts

encodeMachine :: Machine -> String -> String
encodeMachine m input =
    "&TAPE_START" ++ encodeTransitions (transitions m) ++ "&INIT" ++ initial m
    ++ "&INPUT" ++ input ++ "&EOI"

readJsonFile :: FilePath -> String -> IO ()
readJsonFile file input = do
    myJson <- B.readFile file
    let parsed = Aeson.eitherDecode myJson :: Either String Machine
    case parsed of
        Right x -> putStrLn $ encodeMachine x input
        Left y -> putStrLn y

main :: IO ()
main = do
    args <- getArgs
    case args of
        (x:(y:_)) -> readJsonFile x y
        _ -> putStrLn "Error: parameters\n Usage: ./program machine.json \"input\""
