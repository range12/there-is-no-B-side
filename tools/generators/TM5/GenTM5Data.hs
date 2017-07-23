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
import Control.Monad.Trans.State
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

unsafePeek :: (Show a) => a -> a
unsafePeek showMe = unsafePerformIO $ print showMe >> return showMe

-- DBG -- 

data TM5ConcreteTransition = TM5CTrans {
    read :: Text
    , to_state :: Text
    , write :: Text
    , action :: Text
} deriving (Show,Eq)

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
} deriving (Show,Eq)

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
getCollection = getDoc ^. P.alphabet ^. P.collection
getExhaustiveSet = Set.fromList$ getCollection ++ getRCPFree



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
        in case elemIndex t getFreeSyms of
                Just i -> getRCPFree !! i
                Nothing -> getFreeSyms !! justOrDie (elemIndex t getRCPFree)

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
                 | sym == getGlobAny -> Set.fromList getCollection          -- ...categories.
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
        spliceOut intron txt = T.concat$ T.splitOn intron txt
        resolveSelector :: (?dbgInfo :: String) => Bool -> Text -> Reader Env Text
        resolveSelector isRcp remainder = do
            readEnt <- asks readEntry
            params <- asks stateParams
            let morphRcpM dbg t = return $ if isRcp
                then
                    let ?dbgInfo = ?dbgInfo ++ dbg++ "->gatherSyms:RCP: [" ++ T.unpack t ++ "]" in resolveRCP t
                else id t
                in do
            case remainder of
                 rem 
                     | T.isInfixOf getPlaceHolder rem ->
                            let dbgInfo = printf "gatherSyms:Straight: %s" (T.unpack rem)
                                in morphRcpM dbgInfo$ paramFromSelector (params, remainder)
                     | T.isInfixOf getSameAsRead rem -> let dbgInfo = printf "READPATTERN: readEnt: %s, rem: %s" (T.unpack readEnt) (T.unpack rem) in morphRcpM dbgInfo readEnt
                     | otherwise -> let dbgInfo = "I AM OTHERWISE" in morphRcpM dbgInfo rem





instantiateTrans   :: [(Text, Text)]                           -- Template IO couples
                   -> StateT   (Set Text)                      -- Allowed Sym pool to draw from
                               (Reader (StateInstance, P.M5Transition)) -- reference concrete state, template transition
                               [RichCTransition]                -- Resulting concrete transitions
instantiateTrans [] = return []
instantiateTrans ((is,os):lio) = do
    (curSt@(SI parentState concreteParams)
        , P.M5Trans _ skToSt tpms act)  <- lift ask
    symPool <- get
    let (iConcreteSyms, Env iRemPool _ _) = runState (gatherSyms [is])
                                                     (Env symPool concreteParams is)
    let resolveSyms = \el poo rs -> evalState (gatherSyms el)
                                              (Env poo concreteParams rs)
        collection = getExhaustiveSet
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
--    return $ seq (unsafePeek curSt) ()
    put iRemPool
    instantiateTrans lio
    >>= return . (++) (filter (\tr -> to_state (cTransRCT tr) /= parentState) cTrans)


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
                      -> StateT WIPTransitions
                         (State (Set Text))                    -- final states instances
                         ConcreteTransitions
dispatchInstantiation [] = return . HM.map (\lrct -> cTransRCT <$> lrct) =<< get
dispatchInstantiation ((cState, lRCTr):ls) = do
    moreTasks <- mapM stateFold lRCTr >>= return . concat
    dispatchInstantiation (ls ++ moreTasks)
    where -----------------------------------------------------------------------------
        skellHM = getDoc ^. P.transitions
        fetchSkTr el =
            let ?deathMessage = (++) "dispatchInstantiation: could not find state: "
                in lookupOrDie skellKey skellHM 
            where
                skellKey = skellNameRCT el

        stateFold :: RichCTransition
                  -> StateT WIPTransitions (State (Set Text)) [(Text,[RichCTransition])]
        stateFold = \el -> do
            let skSt = skellNameRCT el
            let callMkTrans = \si -> \lSkTr -> makeTransitions si lSkTr `evalState` getExhaustiveSet
            let hmRich = callMkTrans (SI cState$ paramsRCT el) (fetchSkTr el)
            when (skSt `elem` P._finalStates getDoc)$
                lift$ modify (Set.insert skSt)
            modify (HM.unionWith (\l r -> union (nub l) r) hmRich)
            return$ HM.toList hmRich




data LocalEnv = LEnv {
    _curCState :: Text
    , _curCParams :: [Text]
    , _curSkellState :: Text
    , _curRead :: Text
    , _usedSyms :: [Text]
    , _transSyms :: [Text]
}
$(makeLenses ''LocalEnv)

data MetaEnv = MEnv {
    _finals :: Set Text
    , _wipTrans :: WIPTransitions
}
$(makeLenses ''MetaEnv)



resolveTemplateSym :: Text
                   -> Reader LocalEnv
                      [Text]
resolveTemplateSym tSym = do
    used <- asks _usedSyms
    readEnt <- asks _curRead
    let inter l = (used \\ getExhaustiveSet) `intersection` l
    return$ case tSym of
        getGlobAny -> inter getCollection
        getGlobFree -> inter getFreeSyms
        _ 
          | T.isInfixOf getPlaceHolder tSym -> inter$ paramFromSelector tSym : []
          | case T.breakOn getSameAsRead tSym ->
                (a,b)| not$ T.null b -> inter$ resolveStatic (T.concat a readEnt) : []
                     | otherwise -> inter$ resolveStatic a : []
    where
        resolveStatic sym = if T.isInfixOf getRCPOf sym then resolveRCP sym else sym


-- depth first...
rebootMakeTrans :: StateT MetaEnv
                   (Reader LocalEnv)
                   ()
rebootMakeTrans = do
    
    


-- ((Initialization))
-- ForEach starting state template
--      ForEach (R,W)
--          log . instantiate: templateTrans -> (curState,R,W,params) -> WipConcreteTransition
-- ((Recursion))
-- given WipConcreteTransitions :
-- ForEach concrete state
--      ForEach (curState,R,W,params,toTemplate)
--          if instantiate (toTemplate,r,w,params) NotMemberOf (log:)WipConcreteTransitions:
--              log . instantiate: templateTrans -> (curState,R,W,params) -> WipConcreteTransition

instantiateDoc :: TM5Machine
instantiateDoc =
    let dm = (++) "instantiateDoc: could not find state: "
        doc = getDoc
        alphaDoc = doc ^. P.alphabet
        iniState = doc ^. P.initialState
        staticFinals = Set.fromList$ doc ^. P.finalStates
        skellHM = doc ^. P.transitions
        iniTrans = let ?deathMessage = dm in lookupOrDie iniState skellHM
        --bootstrapInstance = HM.map (\lrct -> cTransRCT<$>lrct)$ evalState 
        bootstrapInstance = evalState 
            (makeTransitions (SI iniState []) iniTrans)
            getExhaustiveSet
        (concreteTrans, concreteFinals) =
--            (,) bootstrapInstance []
              ((dispatchInstantiation$ HM.toList bootstrapInstance) `evalStateT` bootstrapInstance)
              `runState` staticFinals
        in TM5
            "UniversalMachine"
            (getCollection ++ getRCPFree)
            (alphaDoc ^. P.hostBlank)
            (HM.keys concreteTrans)
            iniState
            (Set.toList concreteFinals)
            concreteTrans
