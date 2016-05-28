module Main where

import Turing
import qualified Data.ByteString.Lazy as B(readFile)
import System.Environment
import qualified Data.Aeson as Aeson
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.List (find)


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

computeHelper :: Maybe Transition -> Seq.Seq String -> (Int -> Int) -> Int -> (String, Seq.Seq String, Int -> Int)
computeHelper Nothing _ f _  = ("", Seq.empty, f)
computeHelper (Just t) input f i = (to_state t, Seq.update i (write t) input, f)

-- transitions -> current state -> input -> index
compute :: Map.Map String [Transition] -> String -> Seq.Seq String -> Int -> (String, Seq.Seq String, Int -> Int)
compute tsMap state input i = computeHelper goodTrans input dir i where
    currentSymbol = Seq.index input i
    ts = tsMap Map.! state
    goodTrans = find (\x -> Turing.read x == currentSymbol) ts -- returns THE Transition
    dir = case goodTrans of
        Just y -> if action y == "RIGHT" then (+) 1 else (\x -> x - 1)
        Nothing -> (-) 99999999

putTape :: Seq.Seq String -> Int -> IO ()
putTape input current = let
    symbolsIOs = Seq.foldrWithIndex putSymbol [] input where
        putSymbol i sym result
            | i == current && i == Seq.length input  - 1 =  putStr  ("<" ++ sym ++ ">") : result
            | i == current = putStr ("<" ++ sym) : result
            | i == current + 1 = putStr (">" ++ sym) : result
            | otherwise = putStr (" " ++ sym) : result
    in sequence_ symbolsIOs

showResult :: Machine -> String -> Seq.Seq String -> Int -> IO ()
showResult x currentState input i
    | currentState `elem` finals x = do putStrLn "The END"; print input
    | otherwise = do
        putStrLn $ "Current state: " ++ currentState ++ ".   Index: " ++ show i
        putTape input i
        putStrLn ""
        showResult x newState newInput (if newDir i < 0 then -1 else newDir i)
    where
        computed
            | i == -1 = compute (transitions x) currentState (blank x Seq.<| input) 0
            | i <= Seq.length input - 1 = compute (transitions x) currentState input i
            |otherwise = compute (transitions x) currentState (input Seq.|> blank x) i
        newState = case computed of (a, _, _) -> a
        newInput = case computed of (_, a, _) -> a
        newDir = case computed of (_, _, op) -> op


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
    putStrLn ""

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
    then putStrLn "Error. Parameters!!"
    else do
        myJson <- B.readFile $ head args
        let parsed = Aeson.eitherDecode myJson :: Either String Machine
        case parsed of
            Right x -> do
                showMachine x
                let mySeq = genInputSeq (args !! 1) (filter (\y -> y /= blank x) (alphabet x)) Seq.empty
                putStr "Test seq: "
                print mySeq
                showResult x (initial x) mySeq 0
            Left y -> putStrLn y
