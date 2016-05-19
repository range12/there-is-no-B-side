module Main where

import Turing
import Data.ByteString.Lazy as B(readFile)
import System.Environment
import Data.Aeson as Aeson
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map


goodElement :: [String] -> String -> Maybe (String, Int)
goodElement [] _ = Nothing
goodElement (x:xs) input
    | x == take len input = Just (x, len)
    | otherwise           = goodElement xs input
    where
        len = length x

genInputSeq :: String -> [String] -> Seq.Seq String -> Seq.Seq String
genInputSeq "" _ acc          = acc
genInputSeq input symbols acc = case goodElement symbols input of
    Just (symbol, len) -> genInputSeq (drop len input) symbols (acc Seq.|> symbol)
    Nothing            -> Seq.empty

printTransitions :: Map.Map String [Transition] -> IO ()
printTransitions ts
    | Map.size ts == 0 = return ()
    | otherwise        = do
        let tmp = Map.elemAt 0 ts
        putStr $ fst tmp ++ ": "
        print $ snd tmp
        printTransitions $ Map.delete (fst tmp) ts

showMachine :: Machine -> IO ()
showMachine x = do
    putStrLn (name x)
    putStrLn "********\n"
    putStr "Aphabet: "
    print $ alphabet x
    putStrLn ("Blank: " ++ blank x)
    putStr "States: "
    print $ states x
    putStrLn $ "Initial: " ++ initial x
    putStr "Finals: "
    print $ finals x
    putStrLn ""
    printTransitions (transitions x)
    return ()

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
    then do
        putStrLn "Error. Parameters!!"
        return ()
    else do
        myJson <- B.readFile $ head args
        let parsed = Aeson.eitherDecode myJson :: Either String Machine
        case parsed of
            Right x -> showMachine x
            _       -> return ()
        return ()
