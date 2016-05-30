
module GenTM5Types where

import qualified Data.Text as T (Text)
import Control.Monad.Reader
import Data.List as L

{- a naive signature:

instantiate :: PlaceHolder
            -> TemplateState
            -> Params
            -> StateInstance
-}

data StateInstance = SI {
    nameSI :: Text
    , paramsSI :: [Text]
} deriving (Show)

type SkelInstance = StateInstance

instantiate :: Text -> Reader SkelInstance StateInstance
instantiate plHolder =
    (SI templName params) <- ask
    let bits = T.splitOn plHolder templName
    case bits of
        [single] -> return (SI single [])
        _ ->  return $ SI (T.concat $ L.transpose $ [bits, params]) params
