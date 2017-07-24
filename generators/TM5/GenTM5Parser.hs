{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

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

data AlphabetDoc = ADoc {
    hostBlank :: Text
    , hostTags :: [Text]
    , globHostTags :: Text
    , tapeActSyms :: [Text]
    , globTapeActions :: Text
    , freeSymbols :: [Text]
    , globFreeSymbols :: Text
    , freeSymbolsRCP :: [Text]
    , globFreeSymbolsRCP :: Text
    , globAnyInput :: Text
    , collection :: [Text]
} deriving (Show, Generic)

data GenTemplates = GenTp {
    readPat :: Text
    , inheritedNth :: Text
    , reciprocal :: Text
    , currentState :: Text
} deriving (Show, Generic)

data M5Transition = M5Trans {
    inputOuput :: [(Text, Text)]
    , toStatePattern :: Text
    , toStateParams :: [Text]
    , tapeAction :: Text
} deriving (Show, Generic)

data TM5Doc = TM5Doc {
    alphabet :: AlphabetDoc
    , tapeActions :: (Text, Text)
    , templatePatterns :: GenTemplates
    , transitions :: HashMap Text [M5Transition]
    , finalStates :: [Text]
} deriving (Show, Generic)

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
        <*> o .: "Glob_any"
        <*> o .: "Glob_reciprocal_to_free_symbols"
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
