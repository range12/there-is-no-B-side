{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import System.IO
import System.Exit
import qualified Data.ByteString as B (readFile)
import qualified Data.ByteString.Lazy as BL (writeFile, append)
import Control.Applicative
import qualified Data.Yaml as Y
import Data.Aeson.Encode.Pretty
import GenTM5Parser
import Control.Monad.Reader
import qualified Data.Text as T

import Control.Lens

import Data.IORef
import System.IO.Unsafe

import GenTM5Data


skellFile = "tm5_skel.yml"
outputFile = "TM5.json"

defaultAlphabet = fmap T.pack $ (:[]) <$> ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0'.. '9']

makeReciprocal :: [T.Text] -> [T.Text]
makeReciprocal = fmap (T.cons '~')

constructDoc :: Reader TM5Doc TM5Doc
constructDoc = do
    let getAlpha = view freeSymbols . view alphabet
        getRCP = view freeSymbolsRCP . view alphabet
        getTags = view hostTags  . view alphabet
        getTapeSyms = view tapeActSyms . view alphabet
        setRcp = over alphabet . set freeSymbolsRCP
        setCollection = over alphabet . set collection
    alpha <- asks getAlpha
    rcp <- asks getRCP
    doc <- ask
    if null alpha then
        let setAlpha = over alphabet . set freeSymbols
            in local (setAlpha defaultAlphabet) constructDoc
    else do
    let gatherers = [getTags, getTapeSyms, getAlpha, getRCP]
        collec = \apRcp -> concat (gatherers <*> pure doc) ++ apRcp
        setterM = return . if null rcp
        then let genRCP = makeReciprocal alpha
                 in setCollection (collec genRCP) . setRcp genRCP
        else setCollection (collec [])
        in ask >>= setterM


main = do
    dump <- B.readFile skellFile
    let eitherTm5 = Y.decodeEither dump :: Either String TM5Doc
    case eitherTm5 of
        Left err -> putStrLn err >> exitFailure
        Right doc -> writeIORef refTM5Doc (runReader constructDoc doc)
    BL.writeFile outputFile (encodePretty instantiateDoc `BL.append` "\n")
    putStrLn $ "Success ! File " ++ outputFile ++ " has been written."
