{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GenTM5Types where

import Data.Aeson
import Data.Aeson.TH

import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap as HM
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Control.Monad.Reader
import Data.List as L
import GenTM5Parser (getDoc, TM5Machine)
import qualified GenTM5Parser as P
import Prelude hiding (read)

import Control.Lens

data TM5Machine = TM5 {
    name :: Text
    , alphabet :: [Text]
    , blank :: Text
    , states :: [Text]
    , initial :: Text
    , finals :: [Text]
    , transitions :: HashMap Text (Seq TM5ConcreteTrans)
} deriving (Show)

$(deriveJSON defaultOptions ''TM5Machine)

data TM5ConcreteTrans = M5CTrans {
    read :: Text
    , to_state :: Text
    , write :: Text
    , action :: Text
} deriving (Show)

$(deriveJSON defaultOptions ''TM5ConcreteTrans)

data StateInstance = SI {
    nameSI :: Text
    , paramsSI :: [Text]
} deriving (Show)

type SkelInstance = StateInstance

getPlaceHolder = getDoc ^. templatePatterns ^. inheritedNth

makeInstance :: Reader SkelInstance StateInstance
makeInstance =
    (SI templName params) <- ask
    let bits = T.splitOn getPlaceHolder templName
    case bits of
        [single] -> return (SI single [])
        _ ->  return $ SI (T.concat $ L.transpose $ [bits, params]) params


-- ForEach starting state template
--      ForEach (R,W)
--          encode (R,W) transition
--              <=> instantiate . encode $ nextState

instantiateDoc :: State TM5Machine ()
instantiateDoc = do
    let doc = getDoc
    let alphaDoc = doc ^. P.alphabet
    let collec = alphaDoc ^. P.collection
    let iniState = doc ^. P.initialState
    let finals = doc ^. P.finalStates
    let initTM5 =  TM5 "UniversalMachine" collec (alphaDoc ^. P.blank) finals iniState finals HM.empty
    put initTM5
    gets (HM.toList $ doc ^. P.transitions) >>=


instantiateTrans :: Reader [Text] (HashMap Text (Seq TM5ConcreteTrans))
instantiateTrans = do
    collec <- ask
