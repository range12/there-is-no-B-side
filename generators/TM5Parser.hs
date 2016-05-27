{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Data.Yaml
import GHC.Generics
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Control.Monad (join)

module TM5Parser where

newtype AlphabetDoc = ADoc {
    hostBlank :: Text
    , hostTags :: [Text]
    , globHostTags :: Text
    , tapeActions :: [Text]
    , globTapeActions :: Text
    , freeSymbols :: [Text]
    , globFreeSymbols :: Text
    , freeSymbolsRCP :: [Text]
    , globFreeSymbolsRCP :: Text
    , globAnyInput :: Text
    , collection :: [Text]
} deriving (Show, Generic)

newtype GenTemplates = GenTp {
    readPat :: Text
    , inheritedNth :: Text
    , reciprocal :: Text
    , currentState :: Text
} deriving (Show, Generic)

newtype M5Transition = M5Trans {
    inputOuput :: [(Text, Text)]
    , toStatePattern :: Text
    , toStateParams :: [Text]
    , tapeAction :: Text
} deriving (Show, Generic)

newtype TM5Doc = TM5Doc {
    alphabet :: AlphabetDoc
    , tapeActions :: (Text, Text)
    , templatePatterns :: GenTemplates
    , transitions :: Map Text M5Transition
    , finalStates :: [Text]
} deriving (Show, Generic)

--data AlphabetDoc = ADoc {
--    hostBlank :: Text
--    , hostTags :: [Text]
--    , globHostTags :: Text
--    , tapeActions :: [Text]
--    , globTapeActions :: Text
--    , freeSymbols :: [Text]
--    , globFreeSymbols :: Text
--    , freeSymbolsRCP :: [Text]
--    , globFreeSymbolsRCP :: Text
--    , globAnyInput :: Text
--    , collection :: [Text]
--} deriving (Show, Generic)

valueToText :: Value -> Parser Text
valueToText = withText "Text" return

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
        <*> (o .: "Collection") >>= withArray "Heterogenous array" (\a ->
                (`mapM` (V.toList a)) \v -> case v of Array a -> mapM valueToText (V.toList a)
                                                      v -> [] <$> valueToText v
                                >>= liftM join lt
    
    parseJSON _ = mzero


instance FromJSON M5Transition where
    parseJSON (Object o) = M5Trans
        <$> ((o .: "if_reads_then_writes") >>= parsePairs)
        <*> o .: "pattern"
        <*> o .: "params"
        <*> o .: "action"
        where
            parsePairs :: Value -> Parser [(Text,Text)]
            parsePairs = withArray "Text:Text pairs" \a -> mapM (\h -> return (HM.toList h)) a

    parseJSON _ = mzero
        


instance FromJSON TM5Doc where
    parseJSON _ = mzero


instance ToJSON TM5Doc
instance ToJSON AlphabetDoc
instance ToJSON M5Transition
