{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GenTM5Parser where

import GHC.Generics
import Data.Yaml
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.Monad
import Control.Applicative

import Control.Lens

import Data.IORef
import System.IO.Unsafe

data AlphabetDoc = ADoc {
    _hostBlank :: Text
    , _hostTags :: [Text]
    , _globHostTags :: Text
    , _tapeActSyms :: [Text]
    , _globTapeActions :: Text
    , _freeSymbols :: [Text]
    , _globFreeSymbols :: Text
    , _freeSymbolsRCP :: [Text]
    , _globFreeSymbolsRCP :: Text
    , _globAnyInput :: Text
    , _collection :: [Text]
} deriving (Show, Generic)

$(makeLenses ''AlphabetDoc)

data GenTemplates = GenTp {
    _readPat :: Text
    , _inheritedNth :: Text
    , _reciprocal :: Text
    , _currentState :: Text
} deriving (Show, Generic)

$(makeLenses ''GenTemplates)

data M5Transition = M5Trans {
    _inputOuput :: [(Text, Text)]
    , _toStatePattern :: Text
    , _toStateParams :: [Text]
    , _tapeAction :: Text
} deriving (Show, Generic)

$(makeLenses ''M5Transition)

data TM5Doc = TM5Doc {
    _alphabet :: AlphabetDoc
    , _tapeActions :: (Text, Text)
    , _templatePatterns :: GenTemplates
    , _transitions :: HashMap Text [M5Transition]
    , _intialState :: Text
    , _finalStates :: [Text]
} deriving (Show, Generic)

$(makeLenses ''TM5Doc)

refTM5Doc :: IORef TM5Doc
refTM5Doc = unsafeDupablePerformIO $ newIORef undefined

getDoc = unsafeDupablePerformIO $ readIORef refTM5Doc


valueToText :: Value -> Parser Text
valueToText = withText "Text" return

arrayOfText :: Array -> Parser [Text]
arrayOfText a = mapM valueToText (V.toList a)

instance FromJSON AlphabetDoc where
    parseJSON (Object o) = ADoc
        <$> o .: "Host_blank"
        <*> o .: "Host_tags"
        <*> o .: "Glob_host_tags"
        <*> o .: "Guest_shift"
        <*> o .: "Glob_tape_actions"
        <*> o .: "Guest_free_symbols"
        <*> o .: "Glob_guest_free_symbols"
        <*> o .: "Reciprocal_to_free_symbols"
        <*> o .: "Glob_reciprocal_to_free_symbols"
        <*> o .: "Glob_any"
        <*> ((o .: "Collection") >>= parseCollection)
        where
            parseCollection :: Value -> Parser [Text]
            parseCollection = withArray "array" $
                \a -> liftM concat (mapM flattenAry (V.toList a))
            flattenAry  :: Value -> Parser [Text]
            flattenAry (Array a) = arrayOfText a
            flattenAry (String t) = return [t]
            flattenAry w = typeMismatch "flattenAry:" w
 
    
    parseJSON w = typeMismatch "!object representing AlphabetDoc!" w


instance FromJSON M5Transition where
    parseJSON (Object o) = M5Trans
        <$> ((o .: "if_reads_then_writes") >>= parsePairs)
        <*> o .: "pattern"
        <*> o .: "params"
        <*> o .: "action"
        where
            parsePairs :: Value -> Parser [(Text,Text)]
            parsePairs = withArray "Text:Text pairs" $ \a ->
                (`mapM` V.toList a) $ withObject "Text:Text" $ \h ->
                    head $ (<$> HM.toList h) $ \(l, r) ->
                        liftM2 (,) (return l) (valueToText r)

    parseJSON w = typeMismatch "!object representing M5Transition!" w
        

instance FromJSON TM5Doc where
    parseJSON (Object o) = TM5Doc
        <$> o .: "Alphabet" 
        <*> (o .: "Actions" >>= seqOfTwo)
        <*> o .: "Patterns"
        <*> o .: "Transitions"
        <*> ((o .: "Special_states") >>= (.: "initial"))
        <*> ((o .: "Special_states") >>= (.: "finals"))
            where
                seqOfTwo = withArray "Sequence of 2" $ \a ->
                    if V.length a /= 2
                        then mzero
                        else liftM2 (,) (valueToText$ a V.! 0) (valueToText$ a V.! 1)
                                        

    parseJSON w = typeMismatch "!object representing TM5Doc!" w


instance FromJSON GenTemplates where
    parseJSON (Object o) = GenTp
        <$> o .: "Repeat_read"
        <*> o .: "Inherited_nth"
        <*> o .: "Symbol_reciprocal"
        <*> o .: "Repeat_current_state_pattern"

    parseJSON w = typeMismatch "!object representing GenTemplates!" w
