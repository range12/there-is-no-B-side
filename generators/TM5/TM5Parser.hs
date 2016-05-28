{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module TM5Parser where

import GHC.Generics
import Data.Yaml
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import qualified Data.Vector as V
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
    , transitions :: Map Text M5Transition
    , finalStates :: [Text]
} deriving (Show, Generic)

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
        <*> (o .: "Collection") >>= withArray "Heterogeneous array" $ \a ->
                (`mapM` (V.toList a)) $ \v -> case v of Array a -> mapM valueToText (V.toList a)
                                                        v -> [] <$> valueToText v
                                >>= liftM join
    
    parseJSON _ = error "Failed to parse AlphabetDoc"


instance FromJSON M5Transition where
    parseJSON (Object o) = M5Trans
        <$> ((o .: "if_reads_then_writes") >>= parsePairs)
        <*> o .: "pattern"
        <*> o .: "params"
        <*> o .: "action"
        where
            parsePairs :: Value -> Parser [(Text,Text)]
            parsePairs = withArray "Text:Text pairs" $ \a ->
                (`mapM` (V.toList a)) $ withObject "Text:Text pairs" $ \h ->
                    (`mapM` ((unzip . HM.toList) h)) $ \(l, r) ->
                        liftM2 zip (return l) (mapM valueToText r)

    parseJSON _ = error "Failed to parse M5Transition"
        


instance FromJSON TM5Doc where
    parseJSON (Object o) = TM5Doc
        <$> o .: "Alphabet" 
        <*> (o .: "Actions") >>= seqOfTwo
        <*> o .: "Patterns"
        <*> o .: "Transitions"
        <*> (o .: "Special_states" .: "finals")
            where
                seqOfTwo = withArray "Sequence of 2" $ \a ->
                    case length a == 2 of   False -> mzero
                                            _ -> mapM valueToText (V.toList a)
                                        

    parseJSON _ = error "Failed to parse TM5Doc"

instance FromJSON GenTemplates where
    parseJSON (Object o) = GenTp
        <$> o .: "Repeat_read"
        <*> o .: "Inherited_nth"
        <*> o .: "Symbol_reciprocal"
        <*> o .: "Repeat_current_state_pattern"

    parseJSON _ = error "Failed to parse GenTemplates"
