{-# LANGUAGE OverloadedStrings #-}
-- Generate a palindrom JSON machine from a provided alphabet.

module Main where


import System.Environment
import qualified Data.Text as T (Text)
import Text.Printf (printf)
import Turing

type Symbol = String
type Alphabet = [Symbol]

buildSymSeekEndTrans (i,f) st sym

buildTrans _ _ [] = []
buildTrans blank e@(i,f) a:as =  : buildTrans blank (i,f) as
	where
		state_seek_end =  a ++ "_seek_end"
		state_find_last = a ++ "_find_last"
		seek_end_trans = Transition f state_find_last f "LEFT" : 
		states =

buildMachine :: Symbol -> Alphabet -> Machine 
buildMachine blank alpha = Machine "" [alpha, blank] blank statesM inialM finalsM transitionsM
	where
		customTrans = buildStates blank (head alpha, last alpha) (init $ tail $ alpha)
		statesM = "init":(fmap fst customTrans ++):"HALT":[] 
		initialM = "init"
		finalsM = ["HALT"]
		transitionsM = fmap snd customTrans

usage = do
	p <- getProgName
	fail $ printf "Usage: %s <Blank> <Blank-separated Init-Alphabet-End>\n" p


main = do
	[blank, rawAlpha] <- getArgs >>= \as -> case length as of
												2 -> return (fmap T.pack as)
												_ -> usage >> undefined
	let alphabet = fmap T.unpack $ T.splitOn blank rawAlpha
	buildMachine blank alphabet
