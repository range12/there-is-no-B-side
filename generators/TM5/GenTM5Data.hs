{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}

module GenTM5Data (
    instantiateDoc
) where

import Data.Aeson
import Data.Aeson.TH

import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Sequence as Sq
import Control.Monad.Reader
import Data.List as L
import GenTM5Parser (getDoc)
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

data TM5ConcreteTransition = TM5CTrans {
    read :: Text
    , to_state :: Text
    , write :: Text
    , action :: Text
} deriving (Show)

$(deriveJSON defaultOptions ''TM5ConcreteTransition)

data TM5Machine = TM5 {
    name :: Text
    , alphabet :: [Text]
    , blank :: Text
    , states :: [Text]
    , initial :: Text
    , finals :: [Text]
    , transitions :: HashMap Text [TM5ConcreteTransition]
} deriving (Show)

$(deriveJSON defaultOptions ''TM5Machine)

-- Rich transitions: embeds instantiation meta data along serializable structure.
data RichCTransition = RCTrans {
    cTransRCT :: TM5ConcreteTransition -- toJSON
    , skellNameRCT :: Text -- template name, i.e. HM key to children template trans.
    , paramsRCT :: [Text] -- resolved params for this trans., used by children
} deriving Show

data StateInstance = SI {
    nameSI :: Text
    , paramsSI :: [Text]
} deriving (Show)

exitError :: String -> a
exitError s = unsafeDupablePerformIO $ hPutStrLn stderr s >> exitFailure >> return undefined

myThrow :: a -> b
myThrow _ = undefined

lookupOrDie :: (?deathMessage :: ShowS, Show k) => k -> HashMap k v -> v
lookupOrDie = HM.lookupDefault (exitError $ ?deathMessage $ show k)

getPlaceHolder = getDoc ^. P.templatePatterns ^. P.inheritedNth
getRCPOf = getDoc ^. P.templatePatterns ^. P.reciprocal
getSameAsRead = getDoc ^. P.templatePatterns ^. P.readPat
getGlobAny = getDoc ^. P.alphabet ^. P.globAnyInput
getGlobFree = getDoc ^. P.alphabet ^. P.globFreeSymbols
getFreeSyms = getDoc ^. P.alphabet ^. P.freeSymbols
getRCPFree = getDoc ^. P.alphabet ^. P.freeSymbolsRCP
getAllSyms = getDoc ^. P.alphabet ^. P.collection
getAllSymsSet = Set.fromList getAllSyms 

makeState   :: StateInstance -- a template name and a list of concrete template params.
            -> StateInstance -- the resulting concrete name and list of concrete params.
makeState skelInstance =
    let (SI templName params) = skelInstance
        bits = T.splitOn getPlaceHolder templName
      in case bits of
          [single] -> return (SI single [])
          _ ->  return $ SI (T.concat $ L.transpose $ [bits, params]) params


-- UNUSED ATM.
-- -- The caller manages the symbol collection: for each transition, drop given symbols.
-- -- Caller passes its !!specially ordered!! resolved params to prepend on comprehension.
-- comprehendStates :: StateInstance -> [Text] -> [StateInstance]
-- comprehendStates sk [] = sk
-- comprehendStates (SI tName existingParams) collection =
--     let construct = \p -> SI tName (existingParams ++ [p])
--       in runMakeState = makeState . construct
--         in [runMakeState param | param <- collection]


-- Takes a selector
-- The selector can be either (rcp to Nth sym) "~~%%N" or "%%N" (Nth sym)
-- Returns the index data borne by the selector,
--  as either Left iRcp or Right i.
-- Non-selector or malformed selector will raise a deadly exception
--  through the use of fromJust.
indexFromSelector :: Text -> Either Int Int
indexFromSelector sel =
    let stripRcp = T.stripPrefix getRCPOf sel
        doRead = (Pre.read :: String -> Int)
            . T.unpack . fromJust
            . (T.stripPrefix getPlaceHolder)
        in case stripRcp of
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
        in  return . (++) gathered =<< gatherSyms ls
    where -- -- -- -- -- -- -- Helpers -- -- -- -- -- -- --
        spliceOut exon txt = T.concat$ T.splitOn exon txt
        resolveSelector :: Bool -> Text -> Reader Env Text
        resolveSelector isRcp remainder = do
            readEnt <- asks readEntry
            params <- asks stateParams
            let morphRcp = if isRcp then resolveRcp else id
            case remainder of
                getSameAsRead -> morphRcp asRead
                remainder | not$ T.isInfixOfn getPlaceHolder remainder ->
                                 morphRcp$ paramFromSelector (params, remainder)
                          | otherwise -> morphRcp remainder





instantiateTrans   :: [(Text, Text)]                           -- Template IO couples
                   -> StateT   (Set Text)                      -- Allowed Sym pool to draw from
                               (Reader (StateInstance, P.M5Transition)) -- reference concrete state, template transition
                               [RichCTransition]                -- Resulting concrete transitions
instantiateTrans [] = return []
instantiateTrans ((is,os):lio) = do
    (SI _ concreteParams
        , P.M5Trans _ skToSt tpms act)  <- lift ask
    symPool <- get
    let (iConcreteSyms, Env iRemPool _ _) = runState (gatherSyms [is])
                                                     (Env symPool concreteParams is)
    let resolveSyms = \el poo rs -> evalState (gatherSyms el)
                                              (Env poo concreteParams rs)
        collection = getAllSymsSet
        oConcreteSyms = concat$ resolveSyms [os] collection <$> iConcreteSyms
        pConcretePrms = resolveSyms tpms collection <$> iConcreteSyms
        cActs = if T.isInfixOf getPlaceHolder act
            then \ps -> paramFromSelector (ps,act) <$> pConcretePrms
            else repeat act
        lsStates = makeState <$> SI skToSt <$> pConcretePrms
         in let serialCTrans = zipWith4 TM5CTrans
                                        iConcreteSyms
                                        (nameSI <$> lsStates)
                                        oConcreteSyms
                                        cActs
            in let cTrans = zipWith2 RCTrans
                                     serialCTrans
                                     (paramsSI <$> lsStates)
                                     (repeat skToSt)
                in do
    put iRemPool
    return . (++) cTrans =<< instantiateTrans lio


-- ForEach I:O couple
--  comprehend template I:O couples
--      instantiate State:
--      comprehend template States
--          makeTransition

makeTransitions :: StateInstance -- Previous concrete state, whence the transition is starting from.
                -> [P.M5Transition] -- Associated template transitions
                -> State (Set Text) -- Track consumed symbols as a State
                    (HashMap Text [RichCTransition]) -- fold resulting concrete transitions.
makeTransitions si@(SI parentState _) lSkellTr =
    let foldingLRCTr = HM.insertWith (++) parentState
        in flip . flip foldM$ HM.empty lSkellTr$ \accuHM ->
                                                 \skellTr -> do
            pool <- get
            let iol = inputOutput ^. skellTr
                in let (lRichTr,remPool) =
                            flip runReader (si, skellTr)
                            $ runStateT (instantiateTrans iol) pool
                    in do
            put remPool
            return$ foldr foldingLRCTr accuHM ((:[]) <$> lRichTr)


type ConcreteTransitions = HashMap Text [TM5ConcreteTransition]
dispatchInstantiation :: [(Text,[RichCTransition])]
                      -> ReaderT ConcreteTransitions
                         (State [Text])                    -- final states instances
                         ()
dispatchInstantiation [] = return ()
dispatchInstantiation ((cState, lRCTr):ls) = do
    let skellHM = getDoc ^. P.transitions
        collec = getAllSymsSet
        localUpdate = \hm -> HM.insertWith (++) cState cTr hm
    finalList <- lift get
    let (laterTasks,moreFinals) = flip runState finalList$ return . HM.assocs =<< foldM stateFold HM.empty lRCTr
      in lift (put moreFinals) >> local localUpdate (dispatchInstantiation$ ls ++ laterTasks)
    where -----------------------------------------------------------------------------
        fetchSkTr el =
            let ?deathMessage = (++) "dispatchInstantiation: could not find state: "
                in lookUpOrDie (skellNameRCT el) skellHM 
            where
                skellKey = skellNameRCT el

        stateFold :: HashMap Text [RichCTransition]
                     -> RichCTransition
                     -> State [Text] ConcreteTransitions
        stateFold = \accHM -> \el -> do
            let skSt = skellNameRCT el
            let callMkTrans = \si -> \lSkTr -> collec `evalState` makeTransitions si lSkTr
            let hmRich = callMkTrans (SI skST$ paramsRCT el) (fetchSkTr el)
            when (skSt `elem` skellFinals)$
                get >>= put . (++ [cState])
            return$ HM.unionWith (++) accHM hmRich
    

-- ForEach starting state template
--      ForEach (R,W)
--          encode (R,W) transition
--              <=> instantiate . encode $ nextState

instantiateDoc :: TM5Machine
instantiateDoc = do
    let ?deathMessage = (++) "instantiateDoc: could not find state: "
    let doc = getDoc
        alphaDoc = doc ^. P.alphabet
        iniState = doc ^. P.initialState
        staticFinals = doc ^. P.finalStates
        skellHM = doc ^. P.transitions
        iniTrans = lookupOrDie iniState skellHM
        bootstrapInstance = HM.assocs $ evalState 
            (makeTransitions (SI iniState []) (lookupOrDie iniState skellHM))
            getAllSymsSet
        (cFinals, concreteTrans) = runState (runReaderT (reader id$ dispatchInstantiation bootstrapInstance) HM.empty) []
        in TM5
            "UniversalMachine"
            getAllSyms
            (alphaDoc ^. P.blank)
            (HM.keys concreteTrans)
            iniState
            cFinals
            concreteTrans
