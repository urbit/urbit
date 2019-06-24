{-# LANGUAGE StandaloneDeriving #-}

module Language.Conq.Axe where

import ClassyPrelude hiding ((<.>), Left, Right, hash, cons)

import Language.Conq.Types
import Language.Conq.Exp

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


-- Utils -----------------------------------------------------------------------

lef = ATong (singleton F)
rit = ATong (singleton T)

sut = AAxis []
hed = AAxis [F]
tel = AAxis [T]

showFlatAxe :: Axe -> IO ()
showFlatAxe a = putStrLn $ pack
              $ filter (\x -> x=='0' || x=='1')
              $ show
              $ fmap (\x -> if x then 1 else 0)
              $ toList (bits a)


-- Encode Axes to Vals ---------------------------------------------------------

intVal :: Int -> Val
intVal n | n <= 0 = V0 VN
intVal n          = V1 (intVal (n-1))

typeVal :: TyExp -> Val
typeVal = \case
  TENil     -> V0 $ V0 $ V0 $ VN
  TESum x y -> V0 $ V0 $ V1 $ VP (typeVal x) (typeVal y)
  TETup x y -> V0 $ V1 $ V0 $ VP (typeVal x) (typeVal y)
  TEFor x y -> V0 $ V1 $ V1 $ VP (typeVal x) (typeVal y)
  TEAll x   -> V1 $ V0 $ V0 $ typeVal x
  TEFix x   -> V1 $ V0 $ V1 $ typeVal x
  TERef i   -> V1 $ V1 $ V0 $ intVal i

axeVal :: Axe -> Val
axeVal = \case
    AAxis ds  -> V0 $ V0 $ V0 $ flagsVal ds
    APure v   -> V0 $ V0 $ V1 $ v
    AEval     -> V0 $ V1 $ V0 $ VN
    ATong ts  -> V0 $ V1 $ V1 $ flagsValNE ts
    ACons x y -> V1 $ V0 $ V0 $ VP (axeVal x) (axeVal y)
    ACase x y -> V1 $ V0 $ V1 $ VP (axeVal x) (axeVal y)
    AWith x y -> V1 $ V1 $ V0 $ VP (axeVal x) (axeVal y)
    AHint h   -> V1 $ V1 $ V1 $ hintVal h
  where
    hintVal :: Hint -> Val
    hintVal HLazy     = V0 $ V0 VN
    hintVal HMark     = V0 $ V1 VN
    hintVal (HType t) = V1 $ V0 (typeVal t)
    hintVal HFast     = V1 $ V1 VN

    flagsVal :: [Flag] -> Val
    flagsVal []     = V0 VN
    flagsVal (F:bs) = V1 (VP (V0 VN) (flagsVal bs))
    flagsVal (T:bs) = V1 (VP (V1 VN) (flagsVal bs))

    flagsValNE :: NonEmpty Flag -> Val
    flagsValNE (F :| [])   = V0 (V0 VN)
    flagsValNE (T :| [])   = V0 (V1 VN)
    flagsValNE (F :| b:bs) = V1 (VP (V0 VN) (flagsValNE (b :| bs)))
    flagsValNE (T :| b:bs) = V1 (VP (V1 VN) (flagsValNE (b :| bs)))

axeExp :: Axe -> Exp
axeExp = \case
  AAxis []          -> ESubj
  AAxis [F]         -> EHead
  AAxis [T]         -> ETail
  AAxis (F:ds)      -> axeExp (AAxis ds) <> EHead
  AAxis (T:ds)      -> axeExp (AAxis ds) <> ETail
  APure VN          -> EPure VN
  APure v           -> valExp v
  AEval             -> EEval
  ATong (F :| [])   -> ELeft
  ATong (T :| [])   -> EWrit
  ATong (F :| t:ts) -> axeExp (ATong (t :| ts)) <> ELeft
  ATong (T :| t:ts) -> axeExp (ATong (t :| ts)) <> EWrit
  ACons x y         -> ECons (axeExp x) (axeExp y)
  ACase x y         -> ECase (axeExp x) (axeExp y)
  AWith x y         -> axeExp y <> axeExp x
  AHint HLazy       -> ELazy
  AHint HMark       -> EMark
  AHint (HType ty)  -> EType ty
  AHint HFast       -> EFast

expAxe :: Exp -> Axe
expAxe = \case
  ESubj     -> AAxis []
  EPure v   -> APure v
  EEval     -> AEval
  ELeft     -> ATong (singleton F)
  EWrit     -> ATong (singleton T)
  EHead     -> AAxis (singleton F)
  ETail     -> AAxis (singleton T)
  EDist     -> ACase (expAxe ELeft) (expAxe EWrit)
  EWith x y -> AWith (expAxe x) (expAxe y)
  ECons x y -> ACons (expAxe x) (expAxe y)
  ECase x y -> ACase (expAxe x) (expAxe y) -- TODO Wrong
  EType ty  -> AHint (HType ty)
  EMark     -> AHint HMark
  ELazy     -> AHint HMark -- TODO
  EFast     -> AHint HFast
  EQuot e   -> APure (axeVal (expAxe e)) -- TODO Unquoting
  ETape e   -> expAxe (EQuot (ETape e))
