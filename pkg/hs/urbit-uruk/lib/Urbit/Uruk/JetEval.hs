module Urbit.Uruk.JetEval where

import ClassyPrelude

import Data.Bits             (shiftL, (.|.))
import Data.Function         ((&))
import Data.List             (iterate, (!!))
import Numeric.Natural       (Natural)
import Urbit.Uruk.DashParser (DataJet(..), ExpTree(..), Pos, SingJet(..))

import qualified Urbit.Atom            as Atom
import qualified Urbit.Uruk.DashParser as Dash
import qualified Urbit.Uruk.Jets       as Jets

import Data.Tree


-- Typse -----------------------------------------------------------------------

data Match
  = MD !Dash.DataJet
  | MS !Dash.SingJet
  | MU !Pos !Val !Val
 deriving (Eq, Ord, Generic)
 deriving anyclass NFData

data Ur
  = S
  | K
  | J !Pos
  | D
  | M Match !Natural ![Val]
 deriving (Eq, Ord, Generic)
 deriving anyclass NFData

pattern NS = N S
pattern NK = N K
pattern NJ n = N (J n)
pattern ND = N D
pattern NM x y z = N (M x y z)

type Exp = Dash.ExpTree Ur
type Val = Exp


--------------------------------------------------------------------------------

reduce :: Exp -> Maybe Exp
reduce = \case
    NK :& x :& y            → Just $ x
    (reduce → Just xv) :& y → Just $ xv :& y
    x :& (reduce → Just yv) → Just $ x :& yv
    NS :& x :& y :& z       → Just $ x :& z :& (y :& z)
    ND :& x                 → Just $ dump x
    NJ n :& NJ 1            → Just $ NJ (succ n)
    NJ n :& t :& b          → Just $ NM (match n t b) (fromIntegral n) []
    NM m 0 xs               → Just $ runJet m xs
    NM m n xs :& x          → Just $ NM m (pred n) (xs <> [x])
    _                       → Nothing

match :: Pos -> Val -> Val -> Match
match n t b = Jets.jetMatch (n, dashVal t, dashVal b) & \case
  Nothing         -> MU n t b
  Just (Left  dj) -> MD dj
  Just (Right sj) -> MS sj

dashUr :: Ur -> Dash.Val
dashUr = \case
  S                 -> Jets.NS
  K                 -> Jets.NK
  J n               -> Jets.NJ
  D                 -> Jets.ND
  M (MD dj   ) _ xs -> go (N (Dash.DataJet dj)) xs
  M (MS sj   ) _ xs -> go (N (Dash.SingJet sj)) xs
  M (MU n t b) _ xs -> go (Jets.jn n :& dashVal t :& dashVal b) xs
 where
  go :: Dash.Val -> [Val] -> Dash.Val
  go x []       = x
  go x (y : ys) = go (x :& dashVal y) ys

dashVal :: Val -> Dash.Val
dashVal = \case
  N n    -> dashUr n
  x :& y -> dashVal x :& dashVal y

mkNat :: Natural -> Val
mkNat n = N $ M (MD (NAT n)) 2 []

dump :: Val -> Val
dump = mkNat . snd . go . dashVal
 where
  go :: Dash.Val -> (Int, Natural)
  go = \case
    N Dash.S            -> (3, 0)
    N Dash.K            -> (3, 2)
    N Dash.J            -> (3, 4)
    N Dash.D            -> (3, 6)
    N (Dash.DataJet dj) -> go (Jets.djUnMatch dj)
    N (Dash.SingJet sj) -> go (Jets.sjUnMatch sj)
    x :& y              -> (rBits, rNum)
     where
      (xBits, xNum) = go x
      (yBits, yNum) = go y
      rBits         = 1 + xBits + yBits
      rNum          = 1 .|. shiftL xNum 1 .|. shiftL yNum (1 + xBits)

runJet :: Match -> [Val] -> Val
runJet = curry \case
  (MU _ _ b  , xs       ) -> foldl' (:&) b xs
  (MD (NAT n), [x, y]   ) -> error "TODO"
  (MD (Bn  n), xs       ) -> error "TODO: Bn"
  (MD (Cn  n), xs       ) -> error "TODO: Cn"
  (MD (Sn  n), xs       ) -> error "TODO: Sn"
  (MS EYE    , [x]      ) -> x
  (MS BEE    , xs       ) -> error "TODO: BEE"
  (MS SEA    , xs       ) -> error "TODO: SEA"
  (MS SEQ    , xs       ) -> error "TODO: SEQ"
  (MS YET    , [f, x, y]) -> f :& x :& y
  (MS FIX    , xs       ) -> error "TODO: FIX"
  (MS IFF    , xs       ) -> error "TODO: IFF"
  (MS PAK    , xs       ) -> error "TODO: PAK"
  (MS ZER    , xs       ) -> error "TODO: ZER"
  (MS EQL    , xs       ) -> error "TODO: EQL"
  (MS ADD    , xs       ) -> error "TODO: ADD"
  (MS INC    , xs       ) -> error "TODO: INC"
  (MS DEC    , xs       ) -> error "TODO: DEC"
  (MS FEC    , xs       ) -> error "TODO: FEC"
  (MS MUL    , xs       ) -> error "TODO: MUL"
  (MS BEX    , xs       ) -> error "TODO: BEX"
  (MS LSH    , xs       ) -> error "TODO: LSH"
  (MS SUB    , xs       ) -> error "TODO: SUB"
  (MS DED    , xs       ) -> error "TODO: DED"
  (MS UNI    , xs       ) -> error "TODO: UNI"
  (MS YES    , xs       ) -> error "TODO: YES"
  (MS NAH    , xs       ) -> error "TODO: NAH"
  (MS LEF    , xs       ) -> error "TODO: LEF"
  (MS RIT    , xs       ) -> error "TODO: RIT"
  (MS CAS    , xs       ) -> error "TODO: CAS"
  (MS CON    , xs       ) -> error "TODO: CON"
  (MS CAR    , xs       ) -> error "TODO: CAR"
  (MS CDR    , xs       ) -> error "TODO: CDR"
  _                       -> error "runJet: Bad jet arity"
