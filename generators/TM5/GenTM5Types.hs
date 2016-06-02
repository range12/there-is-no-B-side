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
getRCPFree = getDoc ^. P.alphabet ^. P.freeSymbolsRCP
getAllSyms = getDoc ^. P.alphabet ^. P.collection

makeState   :: StateInstance -- a template name and a list of concrete template params.
            -> StateInstance -- the resulting concrete name and list of concrete params.
makeState skelInstance =
    let (SI templName params) = skelInstance
    let bits = T.splitOn getPlaceHolder templName
      in case bits of
          [single] -> return (SI single [])
          _ ->  return $ SI (T.concat $ L.transpose $ [bits, params]) params


-- The caller manages the symbol collection: for each transition, drop given symbols.
-- Caller passes its !!specially ordered!! resolved params to prepend on comprehension.
comprehendStates :: StateInstance -> [Text] -> [StateInstance]
comprehendStates sk [] = sk
comprehendStates (SI tName existingParams) collection =
    let construct = \p -> SI tName (existingParams ++ [p])
      in runMakeState = makeState . construct
        in [runMakeState param | param <- collection]


-- Takes a selector
-- The selector can be either (rcp to Nth sym) "~~%%N" or "%%N" (Nth sym)
-- Returns the index data borne by the selector,
--  as either Left iRcp or Right i.
-- Non-selector or malformed selector will raise a deadly exception
--  through the use of fromJust.
indexFromSelector :: Text -> Either Int Int
indexFromSelector sel =
    let stripRcp = T.stripPrefix getRCPOf sel
    let doRead = (Pre.read :: String -> Int)
        . T.unpack . fromJust
        . (T.stripPrefix getPlaceHolder)
    case stripRcp of
        Just t -> Left$ doRead t 
        Nothing -> Right$ doRead sel

-- Provided a bare, litteral sym from the `freeSyms` set
-- -- May throw, provided a non-bare, non-freeSym symbol.
resolveRCP :: Text -> Text
resolveRCP t =  getRCPFree !! fromJust miRcp
    where miRcp = elemIndex (params !! i) getFreeSyms

-- From state instance params, a selector string: return the targeted parameter.
-- A malformed selector,
-- An improper (OOB index) selector,
-- A bad freeSyms <=> RCP mapping
--      will raise a deadly exception.
paramFromSelector :: ([Text], Text) -> Text
paramFromSelector (params, sel) = 
    case indexFromSelector sel of
        Right i -> params !! i
        Left i -> resolveRCP (params !! i)
     
-- Draw obtained syms from the pool !
-- Update env: => accum' <- ((accum `union` gotSyms) `inter` pool)
--                pool' <- (pool `diff` (pool `inter` accum'))
--             => Env pool' accum'
-- Starting and reference pools must include reciprocal-free-syms !
data Env = Env { availablePool :: Set Text, stateParams :: [Text], readEntry :: Text }
gatherSyms :: [Text] -> State Env (Set Text)
gatherSyms [] = gets availablePool >>= return$ Set.difference (Set.fromList getAllSyms)
gatherSyms (sym:ls) = do
    params <- gets stateParams
    readEnt <- gets readEntry
    e <- get
    let gotSyms = case sym of
        getGlobFree ->  Set.fromList getFreeSyms
        getGlobAny -> Set.fromList getAllSyms
        sym ->  let isRCP = not . T.null . snd$ T.breakOn getRCPOf sym
                    rcpStripped = spliceOut getRCPOf sym 
                    in let resolve = resolveSelector isRCP rcpStripped
                       in Set.singleton$ runReader resolve e
    let pool' = \uSyms -> pool `Set.difference` uSyms
        in updateEnv uSyms (Env lp pms r) = Env (pool' uSyms) pms r
            in modify (updateEnv gotSyms) >> gatherSyms ls
    where -- -- -- -- -- Helpers -- -- -- -- -- 
        spliceOut exon txt = T.concat$ T.splitOn exon txt
        resolveSelector :: Bool -> Text -> Reader Env Text
        resolveSelector isRcp remainder =
            readEnt <- asks readEntry
            params <- asks stateParams
            let morphRcp = if isRcp then resolveRcp else id
            case remainder of
                getSameAsRead -> morphRcp asRead
                remainder -> | T.null$ snd$ T.breakOn getPlaceHolder remainder =
                                    morphRcp$ paramFromSelector (params, remainder)
                             | otherwise = morphRcp remainder


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


