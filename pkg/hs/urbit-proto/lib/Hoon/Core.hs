{-# LANGUAGE StrictData #-}

module Hoon.Core where

import ClassyPrelude hiding (find)
import Prelude (MonadFail(..))

import Numeric.Natural

import qualified Data.Map as LM

import Nock

type Atom = Natural
type Axis = Atom

type Term = String

-- | The "type of type"
type Type = Base

-- | Subject type
type SubjectT = Type

-- | Result type. The result of an expression used to be called its "product."
type ResultT = Type

-- | Subject value, held in closures
type Env = Base

-- | Result of evaluating a Hoon expression against a subject in the type
-- checker.
data Base
  = Atom Atom
  | Cons Base Base
  | Lamb Hold
  | Core (LM.Map Term Base) Base
  | Name Term Base
  | Aura Term
  | Cell Type Hold
  | Func Type Hold
  | Corp (Map Term Type) (Maybe SubjectT)
  | Face Term Base
  | Type
  | Wait Wait ~ResultT
  | Cast Base ResultT
  | Mash Base ResultT

-- | Actually a closure, not a hold in the previous hoon sense.
-- I've just reused the name. The semantics are: the result of
-- *running* cod against subject *value* env, possibly with an 
-- arg consed onto env, NOT type checking cod against subject type.
data Hold = Hold
  { env :: Env
  , cod :: Code
  }

-- | A neutral term; a bunch of eliminators wrapping `.`; generalized,
-- read-only wing.
data Wait
  = Root
  | Limb Wait Limb
  | Call Wait Base
  -- TODO gosh does => belong here too?
  -- TODO %=?
  -- TODO pattern matching :/

data Limb
  = Axis Axis
  | Ally Atom Term

instance Show Limb where
  show = \case
    Axis a -> "+" <> show a
    Ally (fromIntegral -> n) t -> replicate n '^' <> t

data Step = L | R
  deriving (Eq, Ord, Show)

hop :: Step -> Axis -> Axis
hop L = peg 2
hop R = peg 3

peg :: Axis -> Axis -> Axis
peg a = \case
  0 -> 0 -- I guess? the hoon diverges
  1 -> a
  2 -> a * 2
  3 -> a * 2 + 1
  b -> b `mod` 2 + peg a (b `div` 2) * 2

-- | Combo of cap and mas
cut :: Axis -> Maybe (Step, Axis)
cut = \case
  0 -> Nothing
  1 -> Nothing
  2 -> Just (L, 1)
  3 -> Just (R, 1)
  a -> let Just (s, b) = cut (a `div` 2)
       in Just (s, a `mod` 2 + b * 2)

run :: Axis -> [Step]
run = map fst . pop

-- | Really very sorry
pop :: Axis -> [(Step, Axis)]
pop a = case cut a of
  Nothing -> []
  Just (s, a') -> (s, a) : pop a'

type Wing = [Limb]

-- | Desugared hoon, which will be compiled to nock or run against a subject.
data Code
  = Numb Atom
  | Clhp Code Code
  | Brts Code {- TODO pattern? -} Code
  | Brcn (Map Term Code)
  | Ktts Term Code
  | Nunt Term
  | Bccl Code Code
  | Bchp Code Code
  -- TODO gold core type? can I just not provide syntax for this? $? for lead?
  | Bcbr (Map Text Code)
  | Bcts Term Code
  | Bcbc
  | Tsgr Code Code
  | Cnhp Code Code
  | Cnts Wing (Map Wing Code)
  -- TODO pattern matching
  | Kthp Code Code
  | Ktzp Code Code

data Fail
  = FailText String
  | FailNest Type Type
  | FailFind Wing Type
  | FailLook Wing Base

instance Show Fail where
  show _ = "A problem has occurred while type-checking your program. Please "
        <> "restart your computer and try again."

-- Type checking monad
type Check = Either Fail

failOver :: Fail -> Check a -> Check a
failOver f = either (Left . const f) Right

instance MonadFail Check where
  fail = Left . FailText

data Find
  = Leg Axis
  | Arm Axis Term (Set Term)

hopFind :: Step -> (Find, a) -> (Find, a)
hopFind s = \case
  (Leg a,          x) -> (Leg (hop s a),          x)
  (Arm a arm arms, x) -> (Arm (hop s a) arm arms, x)

-- | Project type by wing, getting type and search trace
find :: Wing -> Type -> Check ([Find], Type)
find [] t = pure ([], t)
find (l:ls) t = failOver (FailFind (l:ls) t) do
  (f, t) <- fond l t
  (fs, t) <- find ls t
  pure (f:fs, t)
  where
    fond = \case
      Axis a -> axis (run a)
      -- TODO implement ^
      Ally _ n -> ally n

    axis :: [Step] -> Type -> Check (Find, Type)
    axis _ Atom{} = fail "find-term" 
    axis _ Cons{} = fail "find-term" 
    axis _ Lamb{} = fail "find-term"
    axis _ Core{} = fail "find-term"
    axis _ Name{} = fail "find-term" -- XX allow Named types?
    -- FIXME I think I need to actually be able to do this; figure it out
    axis _ Wait{} = fail "find-wait"
    -- The ResultTs here ought to be Type, but should we check that?
    axis l (Cast b _) = axis l b
    axis l (Mash b _) = axis l b
    axis [] t = pure (Leg 1, t)
    axis (_:_) Aura{} = fail "find-aura"
    axis (L:ss) (Cell l _) = hopFind L <$> axis ss l
    -- TODO figure out how to do right-projection
    -- find(+, [@, @]) = @, but
    -- find(+, [a/?(foo bar) ?-(a; %foo @, %bar ^)]) = ???
    -- it's a stuck term that might eventually become equal to some other stuck
    -- term, but how to represent it? It seems I can't just represent it as the
    -- dangling subexpression, as I might in a normal dependent implementation,
    -- because it's dangling WITH RESPECT TO A SPECIFIC SUBJECT, and worse, it's
    -- not a specific subject type; for example in T =
    -- [[a/?(foo bar) ?-(a; %foo @, %bar ^)] a/?(foo bar) ?-(a; %foo @, %bar ^)]
    -- find(->, T) /= find(+>, T). Unclear what to do here. Maybe find no longer
    -- cuts it as an abstraction? :(
    -- maybe we need find-bonk analogous to mull-bonk
    axis (R:ss) (Cell _ r) = hopFind R <$> fail "find-cell-R"
    axis (_:_) Func{} = fail "find-func"
    -- note: context still on right
    axis (R:ss) (Corp _ (Just con)) = hopFind R <$> axis ss con
    axis (_:_) Corp{} = fail "find-corp"
    axis ss (Face _ b) = axis ss b
    axis (_:_) Type = fail "find-type"

    ally :: Term -> Type -> Check (Find, Type)
    ally _ Atom{} = fail "find-term"
    ally _ Cons{} = fail "find-term" 
    ally _ Lamb{} = fail "find-term"
    ally _ Core{} = fail "find-term"
    ally _ Name{} = fail "find-term" -- XX allow Named types?
    -- FIXME I think I need to actually be able to do this; figure it out
    ally _ Wait{} = fail "find-wait"
    -- The ResultTs here ought to be Type, but should we check that?
    ally n (Cast b _) = ally n b
    ally n (Mash b _) = ally n b
    ally n Aura{} = fail ("find." <> n)
    -- TODO right projections again sadface
    ally n (Cell l r) = hopFind L <$> ally n l {-<|> fail "find-cell-right"-}
    ally n Func{} = fail ("find." <> n)
    ally n (Corp bat con) = case lookup n bat of
      Just t -> pure (Arm 1 n undefined, t)
      Nothing -> case con of
        Just t -> hopFind R <$> ally n t
        Nothing -> fail "find-corp"
    ally n (Face m t)
      | n == m    = pure (Leg 1, t)
      | otherwise = fail "find-face"
    ally _ Type = fail "find-type"


{-
data Base
  = Atom Atom
  | Cons Base Base
  | Lamb Hold
  | Core (Map Term ~Base)
  | Name Term Base
  | Aura Term
  | Cell Type Hold
  | Func Type Hold
  | Corp (Maybe SubjectT) (Map Term Type)
  | Face Term Base
  | Type
  | Wait Wait ResultT
  | Cast Base ResultT
  | Mash Base ResultT
-}

-- | Project base by wing
look :: Wing -> Base -> Base
look [] b = b
look (l:ls) b = leek l $ look ls b
  where
    leek :: Limb -> Base -> Base
    leek = \case
      Axis a -> axis (pop a)
      -- TODO implement ^
      Ally _ n -> ally n

    axis :: [(Step, Atom)] -> Base -> Base
    axis [] b = b
    axis _ Atom{} = error "look-atom"
    axis ((L,_):ss) (Cons l _) = axis ss l
    axis ((R,_):ss) (Cons _ r) = axis ss r
    axis _ Lamb{} = error "look-lamb"
    axis ((L,_):_) Core{} = error "look-batt"
    axis ((R,_):ss) (Core _ con) = axis ss con
    axis ss (Name _ b) = axis ss b
    axis _ Aura{} = error "look-type"
    axis _ Cell{} = error "look-type"
    axis _ Func{} = error "look-type"
    axis _ Corp{} = error "look-type"
    axis _ Face{} = error "look-type"
    axis _ Type{} = error "look-type"
    -- The only interesting case
    axis ((_,a):_) (Wait w ~t) = Wait (Limb w (Axis a)) ty
      where ~ty = either (error . show) snd $ find [Axis a] t
    axis ss (Cast b _) = axis ss b
    axis ss (Mash b _) = axis ss b
    
    ally :: Term -> Base -> Base
    ally = undefined
     

-- | Given a base, produce code that, when run against any subject, evaluates
-- to that same base
lift :: Base -> Code
lift = undefined

-- | Evaluate code against subject to produce base
drop :: Base -> Code -> Base
drop = undefined

-- | Verify that the first type is a subtype of the second
nest :: Type -> Type -> Check ()
nest = undefined

-- | Verify that the first type can be coerced to the second. Not transitive.
-- Coercions always compile to a no-op in nock.
fits :: Type -> Type -> Check ()
fits = undefined

-- | Given subject type, verify that code has result type
work :: Type -> Code -> Type -> Check ()
work = undefined

-- | Given subject type, determine result type of code
play :: Type -> Code -> Check Type
play = undefined

-- | Compile given code to nock, given subject type
mint :: Type -> Code -> Nock
mint = undefined