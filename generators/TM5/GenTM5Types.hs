{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}

module GenTM5Types where

import Data.Aeson
import Data.Aeson.TH

import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap as HM
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Reader
import Data.List as L
import GenTM5Parser (getDoc, TM5Machine)
import qualified GenTM5Parser as P
import Prelude hiding (read)
import qualified Prelude as Pre (read)

import Data.Maybe
import Data.Either

import System.Exit (exitFailure)
import System.IO.Unsafe (unsafeDupablePerformIO)
import System.IO (hPutStrLn, stderr)

import Control.Lens

data TM5Machine = TM5 {
    name :: Text
    , alphabet :: [Text]
    , blank :: Text
    , states :: [Text]
    , initial :: Text
    , finals :: [Text]
    , transitions :: HashMap Text [TM5ConcreteTrans]
} deriving (Show)

$(deriveJSON defaultOptions ''TM5Machine)

data TM5ConcreteTransition = TM5CTrans {
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

exitError :: String -> a
exitError s = unsafeDupablePerformIO $ hPutStrLn stderr s >> exitFailure >> return undefined

myThrow :: a -> b
myThrow _ = undefined

lookupOrDie :: (?deathMessage :: String) => k -> HashMap k v -> v
lookupOrDie = HM.lookupDefault (exitError ?deathMessage)

getPlaceHolder = getDoc ^. P.templatePatterns ^. P.inheritedNth
getRCPOf = getDoc ^. P.templatePatterns ^. P.reciprocal
getSameAsRead = getDoc ^. P.templatePatterns ^. P.readPat
getGlobAny = getDoc ^. P.alphabet ^. P.globAnyInput
getGlobFree = getDoc ^. P.alphabet ^. P.globFreeSymbols
getFreeSyms = getDoc ^. P.alphabet ^. P.freeSymbols
getAllSyms = getDoc ^. P.alphabet ^. P.collection

makeState :: Reader SkelInstance StateInstance
makeState =
    (SI templName params) <- ask
    let bits = T.splitOn getPlaceHolder templName
      in case bits of
          [single] -> return (SI single [])
          _ ->  return $ SI (T.concat $ L.transpose $ [bits, params]) params


-- The caller manages the symbol collection: for each transition, drop given symbols.
-- Caller passes its !!specially ordered!! resolved params to prepend on comprehension.
comprehendStates :: SkelInstance -> [Text] -> [StateInstance]
comprehendStates sk [] = sk
comprehendStates (SI tName existingParams) collection =
    let construct = \p -> SI tName (existingParams ++ [p])
      in runMakeState = makeState . runReader . construct
        in [runMakeState param | param <- collection]


indexFromSelector :: Text -> Either Int Int
indexFromSelector sel =
    let stripRcp = T.stripPrefix getRCPOf sel
    let doRead = (Pre.Read :: String -> Int)
        . T.unpack . fromJust
        . (T.stripPrefix getPlaceHolder)
    case stripRcp of
        Just t -> Left$ doRead t 
        Nothing -> Right$ doRead sel

-- From state instance params, a selector string: return the targeted parameter.
paramFromSelector :: ([Text], Text) -> Text
paramFromSelector (params, sel) = params !! fromRight$ indexFromSelector sel
     
rcpFromSelector :: ([Text], Text) -> Text
rcpFromSelector (params, sel) =
    let sym = params !! fromLeft$ indexFromSelector sel
        in let iRcp = elemIndex sym (getDoc ^. P.alphabet ^. P.freeSymbols)
            in (getDoc ^. P.alphabet ^. P.freeSymbols) !! fromJust iRcp

-- Draw obtained syms from the pool !
-- Update env: => accum' <- ((accum `union` gotSyms) `inter` pool)
--                pool' <- (pool `diff` (pool `inter` accum'))
--             => Env pool' accum'
-- A starting pool must include reciprocal-free-syms !            
data Env = Env { symPool :: Set Text, accumulator :: Set Text, stateParams :: [Text], readEntry :: Text }
poolSyms :: [Text] -> Reader Env (Set Text)
poolSyms [] = asks accumulator
poolSyms (sym:ls) = do
    params <- asks stateParams
    readEnt <- asks readEntry
    let sel s = Set.singleton$ paramFromSelector (params, s)
    let selRCP s = Set.singleton$ rcpFromSelector (params, s)
    let gotSyms = case sym of
        getGlobFree ->  Set.fromList getFreeSyms
        getGlobAny -> Set.fromList getAllSyms
        getSameAsRead -> Set.singleton readEnt
        sym ->  | T.concat getRCPOf getPlaceHolder `T.isPrefixOf` sym = selRCP s
                | getPlaceHolder `T.isPrefixOf` sym = sel sym
                | otherwise = Set.singleton sym
    let accum' = \uSyms -> pool `Set.intersection` (accum `Set.union` uSyms)
        pool' = \uSyms -> pool `Set.difference` (pool `Set.intersection` accum' uSyms)
        in updateEnv uSyms (Env lp la pms r) = Env (pool' uSyms) (accum' uSyms) pms r
            in local (updateEnv gotSyms) (poolSyms ls)
-- FIXME: resulting pool might be used by the caller. => go State ?


-- ForEach I:O couple
--  comprehend template I:O couples
--      instantiate State:
--      comprehend template States
--          makeTransition

-- makeTransitionInstance :: StateInstance -> SkelTransition -> ConcreteTransition
-- makeTransitionInstance :: key -> HashMap TemplateState [SkelTransitions]
makeTransitions :: (StateInstance, Text)                            -- First / current (StateInstance, skeletonKey)
                -> Reader   (HashMap Text [P.M5Transition])         -- Reference Skeleton HashMap for recurring lookup
                            (HashMap Text [TM5ConcreteTransition])  -- Assembled concrete result
makeTransitions (SI state params, skelKey) =
    let ?deathMessage = "makeTransitions: could not find state: " ++ T.unpack skelKey
    let skelMap = getDoc ^. P.transitions
        in let skTrans = lookupOrDie skelKey skelMap
    let collection = Set.fromList $ getDoc ^. P.alphabet ^. P.collection
    let symSubset = 
    let lsStates = comprehendStates (SI skState PARAMS) SUBSET
    let result = HM.fromList $ zip (repeat skelKey) (nameSI <$> lsStates)
--
--    let recurse st sk = evalState $ makeTransitions (st, sk) HM.empty
--    mapM (evalState . (makeTransitions)(`HM.union` result)
--        $ evalState
--        $ makeTransitions (
--    where
--
--
--        getSubset :: [P.M5Transition] -> Set Text
--        getSubset [] = mempty
--        getSubset (P.M5Trans ioLs _ pms _ :ts) = 
    





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
    let staticFinals = doc ^. P.finalStates
    let initTM5 =  TM5
        "UniversalMachine"
        collec
        (alphaDoc ^. P.blank)
        finals
        iniState
        staticFinals
        HM.empty
    put initTM5
    gets (HM.toList $ doc ^. P.transitions) >>=


