module Practice.HoonSyntax where

import ClassyPrelude

import Control.Monad.Reader
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import Practice.HoonCommon (Atom, Axis, Term, Wing, Limb(..))

-- | Base type
data Bass
  = Non
  | Cel
  | Flg
  | Nul
  | Vod
  | Aur Term

data Hoon
  = Wung Wing
  | Adam Atom Term
  --
  | Bass Bass
  | Bcbc  -- ^ type of type
  | Bcbr (Map Term Spec)  -- ^ lead core type (prev $!)
  | Bccb Hoon  -- ^ type of expression
  | Bccl Spec [Spec]  -- ^ tuple
  | Bccn [(Atom, Spec)]  -- ^ tagged union XX what if tags have different aura
  | Bcdt Spec (Map Term Spec)  -- ^ gold core type, allegedly (may change this)
  | Brhp Spec Spec  -- ^ function
  | Bckt Spec Spec  -- ^ god save me
  | Bcts Skin Spec  -- ^ facialized type
  | Bcpt Spec Spec  -- ^ atomic-cellular disjunction
  --
  | Brcn (Map Term Hoon)
  | Brts Skin Hoon
  --
  | Clcb Hoon Hoon
  | Clkt Hoon Hoon Hoon Hoon
  | Clhp Hoon Hoon
  | Clls Hoon Hoon Hoon
  | Clsg [Hoon]
  | Cltr [Hoon]
  --
  -- | Cncb  [%cncb p=wing q=(list (pair wing hoon))]            ::  %_
  | Cndt Hoon Hoon
  | Cnhp Hoon Hoon
  | Cncl Hoon [Hoon]
  -- | Cntr  [%cntr p=wing q=hoon r=(list (pair wing hoon))]     ::  %*
  | Cnkt Hoon Hoon Hoon Hoon
  | Cnls Hoon Hoon Hoon
  -- | Cnsg  [%cnsg p=wing q=hoon r=(list hoon)]                 ::  %~
  | Cnts Wing [(Wing, Hoon)]
  --
  | Dtkt Hoon Hoon
  | Dtls Hoon
  | Dttr Hoon Hoon
  | Dtts Hoon Hoon
  | Dtwt Hoon
  --
  | Ktls Hoon Hoon
  | Kthp Spec Hoon
  | Ktfs Hoon Spec
  -- | Ktsg fold constant?
  | Ktwt Hoon  -- ^ lead
  | Ktts Skin Hoon  -- ^ apply faces
  | Ktcl Spec  -- ^ mold
  | Ktzp Spec Hoon
  --
  | Sgfs Term Hoon
  --
  --
  | Tsfs Skin Hoon Hoon
  | Tsmc Skin Hoon Hoon  -- XX why is this not =\
  | Tsdt Wing Hoon Hoon
  | Tswt Wing Hoon Hoon Hoon
  | Tsgl Hoon Hoon
  | Tsgr Hoon Hoon
  | Tshp Hoon Hoon
  | Tskt Skin Wing Hoon Hoon
  | Tsls Hoon Hoon
  | Tssg [Hoon]
  -- | Tstr
  -- | Tscm
  --
  | Wtbr [Hoon]
  | Wthp Wing [(Skin, Hoon)]
  | Wtcl Hoon Hoon Hoon
  | Wtdt Hoon Hoon Hoon
  | Wtkt Wing Hoon Hoon
  | Wtgl Hoon Hoon
  | Wtgr Hoon Hoon
  -- | Wtls am not approve
  | Wtpm [Hoon]
  | Wtpt Wing Hoon Hoon
  | Wtts Skin Hoon
  | Wtzp Hoon
  --
  | Zpzp

-- | Patterns
type Skin = Hoon

-- | Types
type Spec = Hoon

-- | That pattern which consists only of a variable
pattern Rash :: Term -> Skin
pattern Rash t = Wung [Ally t]


-- Parser ----------------------------------------------------------------------

data Mode = Wide | Tall
  deriving (Eq, Ord, Show)

type Parser = ReaderT Mode (Parsec Void Text)

hoon :: Parser Hoon
hoon = undefined

skin :: Parser Skin
skin = undefined

spec :: Parser Spec
spec = undefined

