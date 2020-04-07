{-- OPTIONS_GHC -Wall -Werror #-}

module Urbit.Uruk.Dash.DataJet where

import ClassyPrelude hiding (exp, init, many, some, try, elem, Prim)

import Control.Lens hiding (snoc)
import Control.Monad.State.Lazy
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import Data.Tree
import Urbit.Pos
import Urbit.Moon.Arity
import Urbit.Uruk.Dash.Exp

import Bound                 (abstract1, fromScope, toScope)
import Bound.Var             (Var(..))
import Data.List             (iterate, (!!))
import Data.Void             (Void, absurd)
import Numeric.Natural       (Natural)
import Prelude               (read)
import Text.Show.Pretty      (pPrint, ppShow)
import Urbit.Atom            (Atom)
import Urbit.Uruk.JetSpec    (jetSpec)

import qualified Data.Char           as C
import qualified Language.Haskell.TH as TH
import qualified Urbit.Atom          as Atom
import qualified Urbit.Moon.Bracket  as B


-- Utils -----------------------------------------------------------------------

type Nat = Natural

pattern NS = N S
pattern NK = N K
pattern NJ = N J
pattern ND = N D
pattern NB = N (SingJet BEE)
pattern NC = N (SingJet SEA)

pattern SnTag = N (DataJet (NAT 28275))
pattern BnTag = N (DataJet (NAT 28258))
pattern CnTag = N (DataJet (NAT 28259))

jn :: Pos -> Val
jn 1 = N J
jn n = jn (n-1) :& N J


-- Properties of Data Jets -----------------------------------------------------

djArity :: DataJet -> Pos
djArity (NAT _) = 2
djArity (Sn n)  = 2 + n
djArity (In n)  = n
djArity (Bn n)  = 2 + n
djArity (Cn n)  = 2 + n

djTag :: DataJet -> Val
djTag (NAT _) = N K
djTag (Sn _)  = N $ DataJet $ NAT $ Atom.utf8Atom "sn"
djTag (In _)  = N $ DataJet $ NAT $ Atom.utf8Atom "in"
djTag (Bn _)  = N $ DataJet $ NAT $ Atom.utf8Atom "bn"
djTag (Cn _)  = N $ DataJet $ NAT $ Atom.utf8Atom "cn"

skSucc :: Val
skSucc = NS :& (NS :& (NK :& NS) :& NK)

djBody :: DataJet -> Val
djBody = \case
  NAT 0 -> NS :& NK
  NAT n -> skSucc :& djBody (NAT $ pred n)
  Sn  n -> iterate (NB :& (NB :& NS) :& NB :&) NS !! (fromIntegral n - 1)
  In  _ -> NS :& NK :& NK
  Bn  n -> iterate (NB :& NB :&)               NB !! (fromIntegral n - 1)
  Cn  n -> iterate (NB :& (NB :& NC) :& NB :&) NC !! (fromIntegral n - 1)

natMatch :: Val -> Maybe Natural
natMatch (NS :& NK) = pure 0
natMatch (NS :& (NS :& (NK :& NS) :& NK) :& n) = succ <$> natMatch n
natMatch _ = Nothing

snMatch :: Val -> Maybe Pos
snMatch NS                            = Just 1
snMatch (NB :& (NB :& NS) :& NB :& r) = succ <$> snMatch r
snMatch _                             = Nothing

bnMatch :: Val -> Maybe Pos
bnMatch NB              = Just 1
bnMatch (NB :& NB :& n) = succ <$> bnMatch n
bnMatch _               = Nothing

cnMatch :: Val -> Maybe Pos
cnMatch NC                            = Just 1
cnMatch (NB :& (NB :& NC) :& NB :& n) = succ <$> cnMatch n
cnMatch _                             = Nothing

djMatch :: (Pos, Val, Val) -> Maybe DataJet
djMatch (2, N K, natMatch -> Just n) = Just (NAT n)
djMatch (r, SnTag, snMatch -> Just p) | r == p + 2 = Just (Sn p)
djMatch (r, BnTag, bnMatch -> Just p) | r == p + 2 = Just (Bn p)
djMatch (r, CnTag, cnMatch -> Just p) | r == p + 2 = Just (Cn p)
djMatch _                            = Nothing

djTuple :: DataJet -> (Pos, Val, Val)
djTuple sj = (djArity sj, djTag sj, djBody sj)

djUnMatch :: DataJet -> Val
djUnMatch sj = jn ari :& tag :& bod
 where
  (ari, tag, bod) = djTuple sj
