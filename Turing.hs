{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Turing where

import Data.Aeson
import GHC.Generics
import Data.Text.Lazy.Encoding as E
import Data.Text.Lazy as L
import Data.ByteString.Lazy as B
import qualified Data.Map.Strict as Map
import Control.Monad(mzero)

string2ByteString :: String -> B.ByteString
string2ByteString = E.encodeUtf8 . L.pack


data Transition = Transition { read :: String
                             , to_state :: String
                             , write :: String
                             , action :: String } deriving (Show, Generic)

instance FromJSON Transition where
    parseJSON (Object v) =
        Transition <$> v .: "read"
                   <*> v .: "to_state"
                   <*> v .: "write"
                   <*> v .: "action"
    parseJSON _ = mzero

-- data Transitions =
--     Transistions (Map.Map String [Transition]) deriving (Show, Generic)
--
-- instance FromJSON Transitions

data Machine = Machine { name :: String
                       , alphabet :: [String]
                       , blank :: String
                       , states :: [String]
                       , initial :: String
                       , finals :: [String]
                       , transitions :: Map.Map String [Transition] }
                       deriving (Show, Generic)

instance FromJSON Machine where
    parseJSON (Object v) =
        Machine <$> v .: "name"
                <*> v .: "alphabet"
                <*> v .: "blank"
                <*> v .: "states"
                <*> v .: "initial"
                <*> v .: "finals"
                <*> v .: "transitions"
    parseJSON _ = mzero
