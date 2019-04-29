module LL.Run where

import ClassyPrelude hiding (succ, union, intersect)
import Control.Lens
import Control.Lens.TH
import Control.Monad.Fix
import Data.Void
import IR.Ty (Ty, Nat, Sym, Hoon, HoonPath)
import LL.Types

import Control.Category ((>>>))

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified IR.Wing as Wing
import qualified Prelude


-- Types -----------------------------------------------------------------------

type Battery a = Map Sym (LL a)

data Val a
    = VAtom !Nat
    | VCell !(Val a) !(Val a)
    | VCore !(Battery a) !(Val a)
  deriving (Eq, Ord, Show)


-- Lenses ----------------------------------------------------------------------

makePrisms ''Val

_VBool :: Prism' (Val a) Bool
_VBool = prism' mk get
  where
    mk True  = VAtom 0
    mk False = VAtom 1

    get (VAtom 1) = pure False
    get (VAtom 0) = pure True
    get _         = Nothing


-- Value Manipulation ----------------------------------------------------------

succ :: Val a -> Either String (Val a)
succ = fmap (review _VAtom . (+1)) . mbErr notAtom . preview _VAtom
  where notAtom = "Can't increment non-atom value."

isCell :: Val a -> Bool
isCell = \case
  VAtom _   -> False
  VCell _ _ -> True
  VCore _ _ -> True  -- bah

get :: LLPath -> Val a -> Either String (Val a)
get = go
  where
    go []           val         = pure val
    go (L:ds)       (VCell l _) = go ds l
    go (R:ds)       (VCell _ r) = go ds r
    go (Ctx:ds)     (VCore _ c) = go ds c
    go _            _           = Left "Failed to lookup LLPath in value"

edit :: LLPath -> Val a -> Val a -> Either String (Val a)
edit = go
  where
    go []       x v                = pure x
    go (L:ds)   x (VCell l r)      = VCell <$> edit ds l x <*> pure r
    go (R:ds)   x (VCell l r)      = VCell <$> pure l <*> edit ds r x
    go (Ctx:ds) x (VCore arms ctx) = VCore <$> pure arms <*> edit ds ctx x
    go _        x _                = Left "LL.Run.edit: Bullshit edit"


-- Interpreter -----------------------------------------------------------------

mbErr :: String -> Maybe a -> Either String a
mbErr err = \case Nothing -> Left err
                  Just a  -> pure a

fire :: (Eq a, Show a) => Sym -> Val a -> Either String (Val a)
fire nm = \case
    k@(VCore batt ctx) -> mbErr noArm (Map.lookup nm batt) >>= runLL k
    _                  -> Left "Attempting to fire arm in non-core value"
  where
    noArm = unpack ("LL.Run.fire: Can't find arm " <> nm)

toBool :: Show a => Val a -> Either String Bool
toBool v = mbErr notBool (v ^? _VBool)
  where
    notBool = "Expected a bool, but got " <> show v

runLL :: (Eq a, Show a) => Val a -> LL a -> Either String (Val a)
runLL sut = r
  where
    r = \case
      LAxis _ p       -> get p sut
      LFire _ a bat   -> fire a sut  -- TODO do we test whether bat is correct?
      LSucc _ h       -> r h >>= succ
      LPair _ x y     -> VCell <$> r x <*> r y
      LAtom _ n       -> pure (VAtom n)
      LEdit _ w x y   -> join (edit w <$> r x <*> r y)
      LWith _ x y     -> r x >>= flip runLL y
      LCore _ b       -> pure (VCore b sut)
      LTest _ v t f   -> r v >>= toBool >>= bool (r t) (r f)
      LCelQ _ v       -> review _VBool . isCell <$> r v
      LEqlQ _ x y     -> review _VBool <$> ((==) <$> r x <*> r y)
