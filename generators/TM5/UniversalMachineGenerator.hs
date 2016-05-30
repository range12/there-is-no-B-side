module Main where

import System.Environment
import System.IO
import qualified Data.ByteString as B
import Control.Applicative
import Data.Yaml
import GenTM5Parser
import Control.Monad.Reader
import qualified Data.Text as T

import Data.IORef
import System.IO.Unsafe

refTM5Doc :: IORef TM5Doc
refTM5Doc = unsafeDupablePerformIO $ newIORef undefined

getDoc = unsafeDupablePerformIO $ readIORef refTM5Doc



skellFile = "tm5_skel.yml"

defaultAlphabet = (:[]) <$> ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0'.. '9']

makeReciprocal :: Functor f => f String -> f String
makeReciprocal = fmap ('~':)

constructDoc :: Reader TM5Doc TM5Doc
constructDoc = do
    doc@(TM5Doc alpha ta tp tr fs) <- ask
    if null $ freeSymbols alpha then do
        let (ADoc hb ht ght ts gts _ gfs rfs grfs ga v) = alpha
        let frSym = T.pack <$> defaultAlphabet
        let rFrSym = fmap T.pack $ makeReciprocal defaultAlphabet
            in let nuAl = ADoc hb ht ght ts gts frSym gfs rFrSym grfs ga (v ++ frSym)
                in return (TM5Doc nuAl ta tp tr fs)
    else do
        return doc



main = do
    dump <- B.readFile skellFile
    let eitherTm5 = decodeEither dump :: Either String TM5Doc
    case eitherTm5 of
        Left err -> putStrLn err
        Right doc -> writeIORef refTM5Doc (runReader constructDoc doc)
        >> print getDoc
