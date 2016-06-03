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
import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Sequence as Sq
import Control.Monad.Reader
import Data.List as L
import GenTM5Parser (getDoc, TM5Machine)
import qualified GenTM5Parser as P
import Prelude hiding (read)
import qualified Prelude as Pre (read)

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader

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

data RichCTransition = RCTrans {
    cTrans :: TM5ConcreteTransition
    , paramsRCT :: [Text]
} deriving Show

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
     
-- CAVEAT: 'action' field is %%'ed for return_* states !
-- CAVEAT²: 'action' field is %%'ed with non-symbols !
--
--
-- Draw obtained syms from the pool !
-- Update env: => accum' <- ((accum `union` gotSyms) `inter` pool)
--                pool' <- (pool `diff` (pool `inter` accum'))
--             => Env pool' accum'
-- Starting and reference pools must include reciprocal-free-syms !
data Env = Env { availablePool :: Set Text, stateParams :: [Text], readEntry :: Text }
gatherSyms :: [Text] -> State Env [Text]
gatherSyms [] = return []
gatherSyms (sym:ls) = do
    e@(Env pool params readEnt) <- get
    let gotSyms = case sym of
        getGlobFree ->  Set.fromList getFreeSyms
        getGlobAny -> Set.fromList getAllSyms
        sym ->  let isRCP = T.isInfixOf getRCPOf sym
                    rcpStripped = spliceOut getRCPOf sym 
                    in let resolve = resolveSelector isRCP rcpStripped
                       in Set.singleton$ runReader resolve e
    let pool' = \uSyms -> pool `Set.difference` uSyms
        in let updateEnv uSyms (Env lp pms r) = Env (pool' uSyms) pms r
            in modify (updateEnv gotSyms)
    let gathered = Set.toList$ pool `Set.intersection` gotSyms
        in  return . mappend gathered =<< gatherSyms ls
    where -- -- -- -- -- -- -- Helpers -- -- -- -- -- -- --
        spliceOut exon txt = T.concat$ T.splitOn exon txt
        resolveSelector :: Bool -> Text -> Reader Env Text
        resolveSelector isRcp remainder =
            readEnt <- asks readEntry
            params <- asks stateParams
            let morphRcp = if isRcp then resolveRcp else id
            case remainder of
                getSameAsRead -> morphRcp asRead
                remainder -> | not$ T.isInfixOfn getPlaceHolder remainder =
                                    morphRcp$ paramFromSelector (params, remainder)
                             | otherwise = morphRcp remainder


instantiateTrans   :: [(Text, Text)]                           -- Template IO couples
                   -> StateT   (Set Text)                      -- Allowed Sym pool to draw from
                               (Reader (StateInstance, P.M5Transition)) -- reference concrete state, template transition
                               [RichCTransition]                -- Resulting concrete transitions
instantiateTrans [] = return []
instantiateTrans ((is,os):lio) =
    (SI _ concreteParams
        , P.M5Trans _ toSt tpms act)  <- lift . ask
    symPool <- get
    let (iConcreteSyms, Env iRemPool _ _) = runState (gatherSyms [is]) (Env symPool concreteParams is)
    let resolveSyms = \el poo rs -> evalState (gatherSyms el) (Env poo concreteParams rs)
        collection = Set.fromList getAllSyms
        oConcreteSyms = mconcat$ resolveSyms [os] collection <$> iConcreteSyms
        pConcretePrms = resolveSyms tpms collection <$> iConcreteSyms
        cActs = if T.isInfixOf getPlaceHolder act
            then \ps -> paramFromSelector (ps,act) <$> pConcretePrms
            else repeat act
        lsStates = makeState <$> SI toSt <$> pConcretePrms
      in let serialCTrans = zipWith4 TM5CTrans
                                iConcreteSyms
                                (nameSI <$> lsStates)
                                oConcreteSyms
                                cActs
        in let cTrans = zipWith RCTrans
                                serialCTrans
                                (paramsSI <$> lsStates)
    put iRemPool
    return . mappend cTrans =<< instantiateTrans lio

-- ForEach I:O couple
--  comprehend template I:O couples
--      instantiate State:
--      comprehend template States
--          makeTransition

-- makeTransitionInstance :: StateInstance -> SkelTransition -> ConcreteTransition
-- makeTransitionInstance :: key -> HashMap TemplateState [SkelTransitions]
makeTransitions :: StateInstance
                -> P.M5Transition
                -> State (Set Text) [TM5ConcreteTrans]
makeTransitions (SI state params, skelKey) ptrans =
    let ?deathMessage = "makeTransitions: could not find state: " ++ T.unpack skelKey
    let skToState = ptrans ^. toStatePattern
    let skToStatePms = ptrans ^. toStateParams
    let ((iS, oS):lio) = skelTrans ^. P.inputOutput
    providedPool <- get
    let (iSubset, Env remPool _ _) = runState (gatherSyms inputTSyms) (Env providedPool params READ?)
    let (oSubset, _) = runState (gatherSyms inputTSyms) (Env providedPool params READ?)
    let (toStSubset, _) = runState (gatherSyms inputTSyms) (Env providedPool params READ?)
    let lsToStates = comprehendStates (SI skToState params) symSubset
    


-- (StateInstance, SkelState) -> SkelTrans -> State [Syms] [ConcreteStates]
-- makeTransition....

--  


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


