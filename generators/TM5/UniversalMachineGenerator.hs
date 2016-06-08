module Main where

import System.Environment
import System.IO
import System.Exit
import qualified Data.ByteString as B (readFile)
import qualified Data.ByteString.Lazy as BL (writeFile)
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
    alpha <- asks getAlpha
    rcp <- asks getRCP
    collec <- return . concat . (<*>) [getRCP, getTags, getTapeSyms] . pure =<< ask
    if null alpha then
        let setAlpha = over alphabet . set freeSymbols
            in local (setAlpha defaultAlphabet) constructDoc
    else if null rcp then do
        let setRcp = over alphabet . set freeSymbolsRCP
            setCollection = over alphabet . set collection
            rcp = makeReciprocal alpha
            in ask >>= return . setCollection collec . setRcp rcp
    else ask


main = do
    dump <- B.readFile skellFile
    let eitherTm5 = Y.decodeEither dump :: Either String TM5Doc
    case eitherTm5 of
        Left err -> putStrLn err >> exitFailure
        Right doc -> writeIORef refTM5Doc (runReader constructDoc doc)
           >> print getDoc
    BL.writeFile outputFile $ encodePretty instantiateDoc
    putStrLn $ "Success ! File " ++ outputFile ++ " has been written."
