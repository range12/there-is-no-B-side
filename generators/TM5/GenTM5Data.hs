{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE CPP #-}

module GenTM5Data (
    instantiateDoc
) where

import Data.Aeson
import Data.Aeson.TH

import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List as L
import GenTM5Parser (getDoc)
import qualified GenTM5Parser as P
import Prelude hiding (read)
import qualified Prelude as Pre (read)

import Control.Applicative
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader

import Data.Maybe
import Data.Either

import System.Exit (exitFailure)
import System.IO.Unsafe (unsafeDupablePerformIO)
import System.IO (hPutStrLn, stderr)

import Text.Printf

import Control.Lens

-- DBG --
import System.IO.Unsafe

wildShow :: (Show a) => a -> a
wildShow showMe = unsafePerformIO $ print showMe >> return showMe

-- DBG -- 

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

lookupOrDie :: (?deathMessage :: ShowS, Show k, Hashable k, Eq k) => k -> HashMap k v -> v
lookupOrDie key hm = HM.lookupDefault (exitError $ ?deathMessage $ show key) key hm

getPlaceHolder = getDoc ^. P.templatePatterns ^. P.inheritedNth
getRCPOf = getDoc ^. P.templatePatterns ^. P.reciprocal
getSameAsRead = getDoc ^. P.templatePatterns ^. P.readPat
getSameAsState = getDoc ^. P.templatePatterns ^. P.currentState
getGlobAny = getDoc ^. P.alphabet ^. P.globAnyInput
getGlobFree = getDoc ^. P.alphabet ^. P.globFreeSymbols
getFreeSyms = getDoc ^. P.alphabet ^. P.freeSymbols
getRCPFree = getDoc ^. P.alphabet ^. P.freeSymbolsRCP
getAllSyms = getDoc ^. P.alphabet ^. P.collection
getAllSymsSet = Set.fromList getAllSyms 



makeState  :: StateInstance -- The current concrete state and its params (for recursive transitions)
           -> StateInstance  -- The a template state to instantiate with concrete params.
           -> StateInstance -- the resulting concrete name and list of concrete params.
makeState currentState (SI templName params) =
    if templName == getSameAsState then currentState else
    let  bits = T.splitOn getPlaceHolder templName :: [Text]
         in case bits of
          [single] -> SI single []
          _ ->  SI (T.concat$ L.concat$ L.transpose$ [bits, params]) params

#define CALLJUSTORDIE(tailInfo, param) (let ?dbgInfo = ?dbgInfo ++ tailInfo in justOrDie param)
justOrDie :: (?dbgInfo :: String) => Maybe a -> a
justOrDie = fromMaybe (let ?dbgInfo = "justOrDie: DYING -> " ++ ?dbgInfo in exitError ?dbgInfo)

-- Takes a selector
-- The selector can be either (rcp to Nth sym) "~~%%N" or "%%N" (Nth sym)
-- Returns the index data borne by the selector,
--  as either Left iRcp or Right i.
-- Non-selector or malformed selector will raise a deadly exception
--  through the use of fromJust.
indexFromSelector :: (?dbgInfo :: String) => Text -> Either Int Int
indexFromSelector sel =
    let stripRcp = T.stripPrefix getRCPOf sel
--        ?dbgInfo = ?dbgInfo ++ "; indexFromSelector"
        doRead = (Pre.read :: String -> Int)
            . T.unpack . CALLJUSTORDIE("; indexFromSelector",)
            . (T.stripPrefix getPlaceHolder)
        in case stripRcp of
            Just t -> Left$ doRead t 
            Nothing -> Right$ doRead sel

-- Provided a bare, litteral sym from the `freeSyms` set
-- -- May throw, provided a non-bare, non-freeSym symbol.
resolveRCP :: (?dbgInfo :: String) => Text -> Text
resolveRCP t =
    let ?dbgInfo = ?dbgInfo ++ "; resolveRCP"
        in getRCPFree !! justOrDie miRcp
    where miRcp = elemIndex t getFreeSyms

-- From state instance params, a selector string: return the targeted parameter.
-- A malformed selector,
-- An improper (OOB index) selector,
-- A bad freeSyms <=> RCP mapping
--      will raise a deadly exception.
paramFromSelector :: (?dbgInfo :: String) => ([Text], Text) -> Text
paramFromSelector (params, sel) = 
    let ?dbgInfo = ?dbgInfo ++ "; paramFromSelector" in
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
    let gotSyms =
            case sym of
             sym | sym == getGlobFree ->  Set.fromList getFreeSyms       -- Globbed...
                 | sym == getGlobAny -> Set.fromList getAllSyms          -- ...categories.
             _   -> let isRCP = T.isInfixOf getRCPOf sym                 -- Single static|template
                        rcpStripped = spliceOut getRCPOf sym 
                        in let resolve = let ?dbgInfo = "sym:" ++ T.unpack sym in resolveSelector isRCP rcpStripped
                           in Set.singleton$ runReader resolve e
    let pool' = \uSyms -> pool `Set.difference` uSyms
        in let updateEnv uSyms (Env lp pms r) = Env (pool' uSyms) pms r
            in modify (updateEnv gotSyms)
    let gathered = Set.toList$ pool `Set.intersection` gotSyms
        in  return . (++) gathered =<< gatherSyms ls
    where -- -- -- -- -- -- -- Helpers -- -- -- -- -- -- --
        spliceOut exon txt = T.concat$ T.splitOn exon txt
        resolveSelector :: (?dbgInfo :: String) => Bool -> Text -> Reader Env Text
        resolveSelector isRcp remainder = do
            readEnt <- asks readEntry
            params <- asks stateParams
            let morphRcpM = return . if isRcp
                then
                    let ?dbgInfo = ?dbgInfo ++ "->gatherSyms:RCP: " ++ T.unpack remainder in resolveRCP
                else id
            case remainder of
                 rem | rem == getSameAsRead -> morphRcpM readEnt
                     | T.isInfixOf getPlaceHolder rem ->
                            let ?dbgInfo = printf "gatherSyms:Straight: %s" (T.unpack rem)
                                in morphRcpM$ paramFromSelector (params, remainder)
                     | otherwise -> morphRcpM rem





instantiateTrans   :: [(Text, Text)]                           -- Template IO couples
                   -> StateT   (Set Text)                      -- Allowed Sym pool to draw from
                               (Reader (StateInstance, P.M5Transition)) -- reference concrete state, template transition
                               [RichCTransition]                -- Resulting concrete transitions
instantiateTrans [] = return []
instantiateTrans ((is,os):lio) = do
    (curSt@(SI _ concreteParams)
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
            then (\ps -> let ?dbgInfo = "instantiateTrans" in paramFromSelector (ps,act)) <$> pConcretePrms
            else repeat act
        lsStates = makeState curSt <$> SI skToSt <$> pConcretePrms
         in let serialCTrans = zipWith4 TM5CTrans
                                        iConcreteSyms
                                        (nameSI <$> lsStates)
                                        oConcreteSyms
                                        cActs
            in let cTrans = zipWith3 RCTrans
                                     serialCTrans
                                     (repeat skToSt)
                                     (paramsSI <$> lsStates)
                in do
--    return $ seq (wildShow curSt) ()
    put iRemPool
    return . (++) cTrans =<< instantiateTrans lio


-- ForEach I:O couple
--  comprehend template I:O couples
--      instantiate State:
--      comprehend template States
--          makeTransition

type WIPTransitions = HashMap Text [RichCTransition]

makeTransitions :: StateInstance -- Previous concrete state, whence the transition is starting from.
                -> [P.M5Transition] -- Associated template transitions
                -> State    (Set Text) -- Track consumed symbols as a State
                            WIPTransitions -- fold resulting concrete transition maps.
makeTransitions si@(SI parentState pParams) lSkellTr = foldM instantiateCondense HM.empty lSkellTr
    where
    instantiateCondense :: WIPTransitions
                        -> P.M5Transition
                        -> State (Set Text) WIPTransitions
    instantiateCondense accuHM skellTr = do
        pool <- get
        let foldingLRCTrM hm v = return$ HM.insertWith (flip (++)) parentState v hm
        let saneSkellTr = if skellTr ^. P.toStatePattern == getSameAsState
                    then set P.toStatePattern parentState . set P.toStateParams pParams$ skellTr
                    else skellTr
        let iol = P._inputOutput saneSkellTr
            in let (lRichTr,remPool) =
                        flip runReader (si, saneSkellTr)
                        $ runStateT (instantiateTrans iol) pool
                        :: ([RichCTransition], Set Text)
                in do
        put remPool
        foldM foldingLRCTrM accuHM ((:[]) <$> lRichTr)





type ConcreteTransitions = HashMap Text [TM5ConcreteTransition]

dispatchInstantiation :: [(Text,[RichCTransition])]
                      -> ReaderT ConcreteTransitions
                         (State [Text])                    -- final states instances
                         ()
dispatchInstantiation [] = return ()
dispatchInstantiation ((cState, lRCTr):ls) = do
    finalList <- lift get
    let (laterTasks,moreFinals) = flip runState finalList$ return . HM.toList =<< foldM stateFold HM.empty lRCTr
      in lift (put moreFinals) >> local localUpdate (dispatchInstantiation$ ls ++ laterTasks)
    where -----------------------------------------------------------------------------
        skellHM = getDoc ^. P.transitions
        localUpdate = \hm -> HM.insertWith (++) cState (cTransRCT <$> lRCTr) hm
        fetchSkTr el =
            let ?deathMessage = (++) "dispatchInstantiation: could not find state: "
                in lookupOrDie skellKey skellHM 
            where
                skellKey = skellNameRCT el

        stateFold :: WIPTransitions
                     -> RichCTransition
                     -> State [Text] WIPTransitions
        stateFold = \accHM -> \el -> do
            let skSt = skellNameRCT el
            let callMkTrans = \si -> \lSkTr -> makeTransitions si lSkTr `evalState` getAllSymsSet
            let hmRich = callMkTrans (SI cState$ paramsRCT el) (fetchSkTr el)
            when (skSt `elem` P._finalStates getDoc)$
                get >>= put . (++ [cState])
            return$ HM.unionWith (++) accHM hmRich
    

-- ForEach starting state template
--      ForEach (R,W)
--          encode (R,W) transition
--              <=> instantiate . encode $ nextState

instantiateDoc :: TM5Machine
instantiateDoc =
    let dm = (++) "instantiateDoc: could not find state: "
        doc = getDoc
        alphaDoc = doc ^. P.alphabet
        iniState = doc ^. P.initialState
        staticFinals = doc ^. P.finalStates
        skellHM = doc ^. P.transitions
        iniTrans = let ?deathMessage = dm in lookupOrDie iniState skellHM
        --bootstrapInstance = HM.map (\lrct -> cTransRCT<$>lrct)$ evalState 
        bootstrapInstance = HM.toList $ evalState 
            (makeTransitions (SI iniState []) iniTrans)
            getAllSymsSet
        (concreteTrans, concreteFinals) =
--            (,) bootstrapInstance []
              ((dispatchInstantiation bootstrapInstance >> reader id) `runReaderT` HM.empty)
              `runState` []
        in TM5
            "UniversalMachine"
            getAllSyms
            (alphaDoc ^. P.hostBlank)
            (HM.keys concreteTrans)
            iniState
            concreteFinals
            concreteTrans
