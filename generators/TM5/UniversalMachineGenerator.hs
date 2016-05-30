module Main where

import System.Environment
import System.IO
import System.Exit
import qualified Data.ByteString as B
import Control.Applicative
import qualified Data.Yaml as Y
import GenTM5Parser
import Control.Monad.Reader
import qualified Data.Text as T

import Control.Lens

import Data.IORef
import System.IO.Unsafe

refTM5Doc :: IORef TM5Doc
refTM5Doc = unsafeDupablePerformIO $ newIORef undefined

getDoc = unsafeDupablePerformIO $ readIORef refTM5Doc



skellFile = "tm5_skel.yml"

defaultAlphabet = fmap T.pack $ (:[]) <$> ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0'.. '9']

makeReciprocal :: [T.Text] -> [T.Text]
makeReciprocal = fmap (T.cons '~')

constructDoc :: Reader TM5Doc TM5Doc
constructDoc = do
    let getAlpha = view freeSymbols . view alphabet
    alpha <- asks getAlpha
    if null alpha then
        let setAlpha = over alphabet . set freeSymbols
            in local (setAlpha defaultAlphabet) constructDoc
    else do
        let setRcp = over alphabet . set freeSymbolsRCP
        let rcp = makeReciprocal alpha
        ask >>= return . (setRcp rcp)


main = do
    dump <- B.readFile skellFile
    let eitherTm5 = Y.decodeEither dump :: Either String TM5Doc
    case eitherTm5 of
        Left err -> putStrLn err >> exitFailure
        Right doc -> writeIORef refTM5Doc (runReader constructDoc doc)
       >> print getDoc
