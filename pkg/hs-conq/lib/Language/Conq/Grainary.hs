{-# LANGUAGE StandaloneDeriving #-}

module Language.Conq.Grainary where

import Language.Conq.Types
import Language.Conq.ForVal

import ClassyPrelude hiding ((<.>), Left, Right, hash, cons)
import Data.Type.Equality
import Type.Reflection
import Data.Coerce
import GHC.Natural
import Control.Category
import Data.Flat
import Data.Flat.Bits
import Data.Bits
import Data.Vector (generate)
import Data.Monoid.Unicode ((∅))
import Data.List.NonEmpty (NonEmpty(..))

import Control.Lens               ((&))
import Control.Monad.Except       (ExceptT, runExceptT)
import Control.Monad.State        (State, get, put, evalState, runState)
import Control.Monad.Trans.Except (throwE)
import Data.Bits                  ((.|.), shiftL, shiftR)
import System.IO.Unsafe           (unsafePerformIO)
import Text.Show                  (showString, showParen)

import qualified Prelude                as P
import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.ByteString.Base58 as Base58
import qualified Data.ByteString        as B
import qualified Data.ByteString.Unsafe as B


-- Jets ------------------------------------------------------------------------

jetReg :: [(SHA256, Exp, Val -> Val)]
jetReg =
  [ ( SHA256 (encodeBase58 "FWz5mTGmuVz2b4TLNa7yMjTKL7wihsEWakoUD2nzqP6q")
    , ESubj
    , id
    )
  ]

jets :: HashMap SHA256 (Val -> Val)
jets = mapFromList (jetReg <&> \(h,_,f) -> (h,f))

grainsFromJets :: HashMap SHA256 ByteString
grainsFromJets = do
  mapFromList $ jetReg <&> \(h,e,_) -> (h, flat (forVal e))

-- The Grainary ----------------------------------------------------------------

grainery :: IORef (HashMap SHA256 ByteString)
grainery = unsafePerformIO (newIORef grainsFromJets)


-- Utilities -------------------------------------------------------------------

decodeBase58 :: ByteString -> Text
decodeBase58 = decodeUtf8 . Base58.encodeBase58 Base58.bitcoinAlphabet

fromJust (Just x) = x
fromJust _        = error "fromJust: Nothing"

encodeBase58 :: Text -> ByteString
encodeBase58 = fromJust
             . Base58.decodeBase58 Base58.bitcoinAlphabet
             . encodeUtf8


-- Create Grains ---------------------------------------------------------------

putGrain :: Val -> Val
putGrain v = unsafePerformIO $ do
      (bs, k) <- evaluate $ force (hashVal v)
      traceM ("Putting Grain: " <> unpack (decodeBase58 $ unSHA256 k))
      atomicModifyIORef' grainery (\t -> (insertMap k bs t, ()))
      evaluate (VR k)
  where
    hashVal :: Val -> (ByteString, SHA256)
    hashVal x = (bs, SHA256 (SHA256.hash bs))
      where bs = flat x


-- Read Grains -----------------------------------------------------------------

getGrain :: SHA256 -> Val
getGrain k = unsafePerformIO $ do
  traceM ("Getting Grain: " <> unpack (decodeBase58 $ unSHA256 k))

  t <- readIORef grainery

  Just (P.Right v) <- pure (unflat <$> lookup k t)

  pure v


-- Encode/Decode Formulas ------------------------------------------------------

flattenExp :: Exp -> Exp
flattenExp = go
  where
    go (EWith (EWith x y) z) = go (EWith x (EWith y z))
    go (EWith x y)           = EWith x (go y)
    go x                     = x

forVal :: Exp -> Val
forVal = \e ->
    case flattenExp e of
      EWith x y -> V1 $ VP (opkVal x) (forVal y)
      x         -> V0 (opkVal x)
  where
    opkVal :: Exp -> Val
    opkVal = \case
      EWith _ _ -> error "forVal: broken invariant"
      ELeft     -> V0 $ V0 $ V0 VN
      EWrit     -> V0 $ V0 $ V1 VN
      EHead     -> V0 $ V1 $ V0 VN
      ETail     -> V0 $ V1 $ V1 VN
      EPure v   -> V1 $ V0 $ V0 $ V0 v
      ESubj     -> V1 $ V0 $ V0 $ V1 VN
      ELazy     -> V1 $ V0 $ V1 $ V0 $ V0 VN
      EMark     -> V1 $ V0 $ V1 $ V1 $ V0 VN
      EEval     -> V1 $ V1 $ V0 $ V0 VN
      EDist     -> V1 $ V1 $ V0 $ V1 VN
      ECase x y -> V1 $ V1 $ V1 $ V0 $ VP (forVal x) (forVal y)
      ECons x y -> V1 $ V1 $ V1 $ V1 $ VP (forVal x) (forVal y)
      EType _   -> opkVal ESubj
      _         -> undefined

valFor' :: Val -> Maybe Exp
valFor' (V0 l)        = valOpk l
valFor' (VR r)        = valFor' (getGrain r)
valFor' (V1 (VP x y)) = (<>) <$> valFor' y <*> valOpk x
valFor' _             = Nothing

valFor :: Val -> Exp
valFor = maybe (EPure VN) id . valFor'

valOpk :: Val -> Maybe Exp
valOpk (V0 (V0 x))      = valDir x
valOpk (V0 (V1 x))      = valGet x
valOpk (V1 (V0 (V0 x))) = valSim x
valOpk (V1 (V0 (V1 x))) = valHin x
valOpk (V1 (V1 (V0 x))) = valOtr x
valOpk (V1 (V1 (V1 x))) = valPlx x
valOpk _                = Nothing

valDir :: Val -> Maybe Exp
valDir (V0 VN) = pure ELeft
valDir (V1 VN) = pure EWrit
valDir _       = Nothing

valGet :: Val -> Maybe Exp
valGet (V0 VN) = pure EHead
valGet (V1 VN) = pure ETail
valGet _       = Nothing

valSim :: Val -> Maybe Exp
valSim (V0 v)  = pure (EPure v)
valSim (V1 VN) = pure ESubj
valSim _       = Nothing

valOtr :: Val -> Maybe Exp
valOtr (V0 VN) = pure EEval
valOtr (V1 VN) = pure EDist
valOtr _       = Nothing

valPlx :: Val -> Maybe Exp
valPlx (V0 (VP x y)) = pure $ ECase (valFor x) (valFor y)
valPlx (V1 (VP x y)) = pure $ ECons (valFor x) (valFor y)
valPlx _             = Nothing

valHin :: Val -> Maybe Exp
valHin = \case
  V0 (V0 VN) -> pure ELazy
  V1 (V0 VN) -> pure EMark
  _          -> Nothing
