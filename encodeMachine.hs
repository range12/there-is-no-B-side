module Main where

import Turing
import qualified Data.ByteString.Lazy as B(readFile)
import qualified Data.Aeson as Aeson
import System.Environment(getArgs)
import Prelude hiding (read)
import qualified Data.Map.Strict as Map


getMove :: Transition -> String
getMove t
    | tAction == "RIGHT" = "!RSH"
    | otherwise          = "!LSH"
    where
        tAction = action t


-- Escape the ! mark because the shell wants to run something when it sees it
esc :: String -> String
esc s
    | head s == '!' = "\\" ++ s
    | otherwise     = s

encodeTransitions :: Map.Map String [Transition] -> String
encodeTransitions =
    let encodeTransLst state ts acc = foldr (\x y -> y ++ "&TRANS" ++ esc state ++ esc (read x)
            ++ esc (to_state x) ++ esc (write x) ++ esc (getMove x)) acc ts
    in Map.foldrWithKey encodeTransLst ""

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
