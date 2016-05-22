{-# LANGUAGE OverloadedStrings #-}
-- Generate a palindrom JSON machine from a provided alphabet.

module Main where


import System.Environment
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B (readFile)
import qualified Data.Text.Lazy as T (Text)
import Text.Printf (printf)
import Turing

type Symbol = String
type TState = String
type Alphabet = [Symbol]

buildReadFirstCharSt :: Alphabet -> (Symbol, Int) -> Transition -> [Transition]
buildReadFirstCharSt [] _ _ = []
buildReadFirstCharSt a:as syms@(iniChr, skSymLen) skelTrans =
    let Transition _ toSt _ act = skelTrans
        aGoLastSt = (a : drop skSymLen toSt) in
    Transition a aGoLastSt iniChr act :
    buildReadFirstCharSt as syms skelTrans

buildXGoLastSt :: Alphabet -> (Symbol, Symbol, Int) -> (Transition, Transition) -> [Transition]
buildXGoLastSt [] (sym, endSym, blank, skSymLen) (_, chkLastSkTr) =
    let Transition _ toSt _ act = chkLastSkTr in
    [Transition endSym (sym : drop skSymLen toSt) blank act]
buildXGoLastSt a:as syms@(sym, _, _, skSymLen) skTrans@(goLastSkTr, _) =
    let Transition _ toSt _ act = goLastSkTr in
    Transition endSym (sym : drop skSymLen toSt) blank act :
    buildXGoLastSt as syms skTrans

buildXCheckLastSt :: Alphabet -> Symbol -> (Transition, Transition) -> [Transition]
buildXCheckLastSt [] xSym (_, Transition _ toGoFirst endSym act) =
    [Transition xSym toGoFirst endSym act]
buildXCheckLastSt a:as xSym trans@(skelBadReadTr, _) = if a == xSym
    then nextOne
    else Transition a toWrNSt a act : nextOne
    where
        Transition _ toWrNSt _ act = skelBadReadTr
        nextOne = buildXCheckLastSt as xSym trans

buildGoFirstSt :: Alphabet -> (Transition, Transition) -> [Transition]
buildGoFirstSt [] (_, fromStartTr) =
    let Transition startSym toChkEnd0St _ act = fromXTr in
    [Transition startSym toChkEnd0St startSym act]
buildGoFirstSt a:as trans@(fromXTr, _) =
    let Transition _ toGoFirstSt _ act = fromXTr in
    Transition a toGoFirstSt a act :
    buildGoFirstSt as trans

        
-- builds check_end0 and check_end1 transition sets.
buildCheckEndYSt :: Alphabet -> (Transition, Transition) -> [Transition]
buildCheckEndYSt [] (_, atEndTr) = [atEndTr]
buildCheckEndYSt a:as trans@(fromXTr, _) =
    let Transition _ toXStateSt _ act = fromXTr in
    Transition a toXStateSt a act :
    buildCheckEndYSt as trans

buildGoFirstOkSt :: Alphabet -> (Transition, Transition) -> [Transition]
buildGoFirstOkSt [] (_, atStartTr) = atStartTr:[]
buildGoFirstOkSt a:as trans@(fromXTr, _) =
    let Transition _ toGoFirstSt _ act = fromXTr in
    Transition a toGoFirstSt a act :
    buildGoFirstOkSt as trans

-- build write_y and write_n transition sets.
buildWriteYesNoTr :: Alphabet -> (Transition, [Transition]) -> [Transition]
buildWriteYesNoTr [] (_, constantBundle) = constantBundle
buildWriteYesNoTr a:as trans@(fromXTr, _) =
    let Transition _ toWrNSt _ act = fromXTr in
    Transition a toWrNSt a act :
    buildWriteYesNoTr as trans
        

buildTrans _ _ [] = []
buildTrans blank e@(i,f) a:as =  : buildTrans blank e as
	where
		state_seek_end =  a ++ "_seek_end"
		state_find_last = a ++ "_find_last"
		seek_end_trans = Transition f state_find_last f "LEFT" : 
		states =

buildMachine :: Symbol -> Alphabet -> Machine -> Machine 
buildMachine blank alpha = Machine "" [alpha, blank] blank statesM inialM finalsM transitionsM
	where
		customTrans = buildStates blank (head alpha, last alpha) (init $ tail $ alpha)
		statesM = "init" : (fmap fst customTrans ++) "HALT": [] 
		initialM = "init"
		finalsM = ["HALT"]
		transitionsM = fmap snd customTrans

usage = do
	p <- getProgName
	fail $ printf "Usage: %s <Blank> <Blank-separated Init-Alphabet-End>\n" p


readSkeleton :: IO Machine
readSkeleton = do
    dump <- B.readFile "generators/palindrome_skel.json"
    let parsed = Aeson.eitherDecode dump :: Either String Machine
        failure = (fail $ head $ lefts parsed) >> undefined
      in either failure (return (head $ rights parsed))

main = do
    skelMachine <- readSkeleton
	[blank, rawAlpha] <- getArgs >>= \as -> case length as of
												2 -> return (T.pack <$> as)
												_ -> usage >> undefined
	let alphabet = T.unpack <$> T.splitOn blank rawAlpha
	buildMachine (T.unpack blank) alphabet skelMachine
