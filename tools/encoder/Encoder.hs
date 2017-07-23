module Main where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as B (readFile)
import           Data.List            (maximumBy)
import qualified Data.Map.Strict      as Map
import           Prelude              hiding (read)
import           System.Environment   (getArgs)
import           Turing


getMove :: Transition -> Bool -> String
getMove t isFinal
    | isFinal && tAction == "RIGHT" = "!RSH_HALT"
    | isFinal && tAction == "LEFT"  = "!LSH_HALT"
    | tAction == "RIGHT"            = "!RSH"
    | otherwise                     = "!LSH"
    where
        tAction = action t


-- Escape the ! mark because the shell wants to run something when it sees it
esc :: String -> String
esc s
    | head s == '!' = "\\" ++ s
    | otherwise     = s

encodeTransitions :: Map.Map String [Transition] -> [String] -> Bool -> String
encodeTransitions transMap finalStates True  =
    let encodeTransLst state ts acc = foldr (\x y -> y ++ "&TRANS" ++ esc state
            ++ esc (read x) ++ esc (to_state x) ++ esc (write x)
            ++ esc (getMove x $ to_state x `elem` finalStates)) acc ts
    in Map.foldrWithKey encodeTransLst "" transMap
encodeTransitions transMap finalStates False =
    let encodeTransLst state ts acc = foldr (\x y -> y ++ "&TRANS" ++ state
            ++ read x ++ to_state x ++ write x ++ getMove x (to_state x `elem` finalStates)) acc ts
    in Map.foldrWithKey encodeTransLst "" transMap

encodeMachine :: Machine -> String -> Bool -> String
encodeMachine m input e =
    "&TAPE_START" ++ encodeTransitions (transitions m) (finals m) e ++ "&INIT"
    ++ initial m ++ "&INPUT" ++ input ++ "&EOI"

legitElements :: [String] -> String -> [(String, Int)] -> [(String, Int)]
legitElements [] _ tokens = tokens
legitElements (x:xs) input legitTokens
    | x == take len input = legitElements xs input $ (x, len) : legitTokens
    | otherwise           = legitElements xs input legitTokens
    where
        len = length x

isGoodInput :: [String] -> String -> Bool
isGoodInput _ ""          = True
isGoodInput symbols input =
    not (null legitToks) && isGoodInput symbols (drop (snd goodSym) input)
    where
        legitToks = legitElements symbols input []
        goodSym   = maximumBy (\ (_, x) (_, y) -> compare x y) legitToks

showResult :: Machine -> String -> Bool -> String
showResult m input isEscaped
    | isGoodInput symbols input = encodeMachine m input isEscaped
    | otherwise                 = "Wrong input"
    where
        symbols = filter (\y -> y /= blank m) $ alphabet m

readJsonFile :: FilePath -> String -> Bool -> IO ()
readJsonFile file input isEscaped = do
    myJson <- B.readFile file
    let parsed = Aeson.eitherDecode myJson :: Either String Machine
    case parsed of
        Right x -> putStrLn $ showResult x input isEscaped
        Left y -> putStrLn y

main :: IO ()
main = do
    args <- getArgs
    let isEscaped = "-e" `elem` args
    case filter (/= "-e") args of
        (x:(y:_)) -> readJsonFile x y isEscaped
        _ -> do
            putStrLn "Error: parameters"
            putStrLn "Usage: ./program ex.json \"input\" [-e]"
            putStrLn "-e: Escape the '!'. Use this option if your shell needs it."
