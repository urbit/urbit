module Practice.DependentHoon3 where

import ClassyPrelude hiding (even, find)

import Control.Monad.Reader
import Data.Set (isSubsetOf)

import Practice.HoonCommon (Atom, Axis, Grit(..), Term, Wing, Limb(..), Nat)

-- | Desugared hoon. Depending on the stage of compilation, different wing types
-- are permitted. As a rough overview, the compiler pipeline is:
--
--   Hoon  ---open--->  Code Wing  ---------play--->  Code Stub, Type
--                      Code Wing, Type  ---work--->  Code Stub
--                                                      \
--                                                       ---mint--->  Nock
--
-- The Wing type allows all wings, while the Stub type allows only raw axes and
-- arm pulls. In this way, name resolution and type checking is separated from
-- code generation (and, very importantly, from compile-time interpretation,
-- where we would otherwise suffer horribly because the fully winged language is
-- not Liskov compliant).
--
data Code a
  = Wing a
  --
  | Atom Atom Grit Term
  | Cons (Code a) (Code a)
  | Lamb (Pelt a) (Code a)
  | Name Term (Code a)
  --
  | Plus (Code a)
  | Slam (Code a) (Code a)
  --
  | Aura Term
  | Fork (Set Atom) Term
  | Cell (Code a) (Code a)
  | Gate (Code a) (Code a)
  | Mask Term (Code a)
  | Noun
  | Void
  | Type
  --
  | With (Code a) (Code a)
  | Push (Code a) (Code a)
  | Nest { cod :: Code a, typ :: Code a }
  deriving (Functor, Foldable, Traversable)

-- | A desugared skin; i.e., a pattern.
data Pelt a
  = Punt                    -- ^ _     wildcard
  | Peer Term               -- ^ a     variable
  | Part (Code a)           -- ^ %foo  constant
  | Pair (Pelt a) (Pelt a)  -- ^ []    cons
  -- | Pace Term (Pelt a)      -- ^ a=    apply/strip face XX as pattern
  -- TODO figure out syntax for as-patterns. can we reuse pace?
  | Pest (Pelt a) (Code a)  -- ^ /   patern nest
  -- | Past (Pelt a) (Code a)  -- ^ ``  pattern cast
  deriving (Functor, Foldable, Traversable)

-- | Air-chilled wing. In the course of type checking, a Code with ordinary
-- wings in it, such as a.b, is translated into on where those wings have been
-- resolved to axes, e.g. +6. The exception is that if the leftmost limb in a
-- wing is determined to refer to an arm of a core, we leave it in place, but
-- resolve the core location into an axis. Thus the two kinds of Stubs
-- correspond to Nock 0 and 9. The change from Wing to Stub is one of the
-- reasons that Code is parametric in wing type.
data Stub
  = Leg Axis
  -- | Arm Axis Term (Set Term)
  deriving (Eq, Ord)

instance Show Stub where
  show = \case
    Leg a -> "+" <> show a

-- | The system of "levels," analogous to de Bruijn levels, allows for a stable
-- way of referring to part of the subject, even as the subject continues to
-- grow to the right. The checker tracks the "current level" of the subject, L.
-- Then the pair (lvl, ax) is interpreted as the raw Axis peg(2^(L - lvl), ax).
-- You can think of (lvl, ax) as representing an axis grounded at the leftmost
-- node of depth L, rather than grounded at the root as in ordinary axes: You
-- walk up lvl steps from that leftmost node, then go down according to ax in
-- the usual way. This system allows us to avoid retraversing the subject type
-- every time the subject grows to rewrite stuck axes, which would ultimately be
-- quadratic. (What is a stuck axis for? It's a reference into a part of the the
-- subject for which the seminoun stores no information. For example, if a gate
-- takes a type argument, and in the body of that gate we want to construct a
-- list of that type, it'd be a (list +6), where "+6" is an opaque reference
-- into the seminoun that, in nest checking, compares equal only to itself
-- (which is all that we mean when we say "opaque reference"). The point of the
-- level system is to avoid having to grow that +6 when we tislus something new.
type Level = Nat

-- | Frozen wing.
data Rump
  = Leg' (Level, Axis)
  -- | Arm' (Level, Axis) Term (Set Term)

instance Show Rump where
  show = \case
    Leg' (l, a) -> "+" <> show l <> "_" <> show a

-- | Check whether two leveled axes correspond to the same axis. Because I'm too
-- stupid right now, the current encoding of leveled axes has duplicate reprs.
even :: (Level, Axis) -> (Level, Axis) -> Bool
even (l, a) (m, b)
  | l < m     = peg (2 ^ (m - l)) a == b
  | l > m     = a == peg (2 ^ (l - m)) b
  | otherwise = a == b

instance Eq Rump where
  Leg' la == Leg' lb = even la lb

-- | Code with wings fully resolved to axes.
type Cold = Code Stub

-- | Increase the number of formal wings permitted when passing under a tisgar,
-- allowing types to refer opaquely to parts of the seminoun of the old subject.
-- These outer wings are NOT permitted in user code, NOR do they have
-- computational meaning; rather, they just sit around stuck to serve as things
-- we track the equality of in the nest checker. For example, suppose your gate
-- takes in a type, then you construct a (list +6), then you tisgar it. The type
-- of the subject is now (list "^+6"), which uses an outer reference.
-- Incidentally, there is now no way for the user to write this type down inside
-- the tisgar; this is fine because there also isn't a way to construct the
-- runtime type information inside either. If you want these things, you should
-- have tisgared more info.
data Hop b a
  = New b  -- ^ reference to current subject
  | Old a  -- ^ reference to outer subject
  deriving (Functor, Foldable, Traversable)

instance (Show a, Show b) => Show (Hop a b) where
  show = \case
    New x -> show x
    Old y -> show y <> "^"

deriving instance (Eq a, Eq b) => Eq (Hop a b)

-- | Alias useful for pedagogical purposes.
type Type = Base

-- | In Base, a common pattern is a closure, suspended over some copy of the
-- subject, and awaiting an extra value before execution can continue. The copy
-- of the subject (think of the context of the result of executing a |%) is
-- paired with the code to execute once the payload is inflated. But often we
-- already have a constant Base we wish to put in this spot, e.g. if the type is
-- not dependent. Thus, a second case provides for this here, for performance.
data Bind a
  = Bind (Base a) {- ^ closed-over subject -} (Code (Hop Stub a))
  | Base (Base a) {- ^ constant -}
  deriving (Functor, Foldable, Traversable)

-- | Fully evaluated expression, possibly stuck on opaque values / symbols in
-- a. These can be thought of as constants, in the sense that if converted back
-- up to code via `loft`, the result will be a code which is "position
-- independent", and does not have any wings into the current subject. FIXME
-- last sentence.
data Base a
  = Stop' Rump
  | Fore' a
  --
  | Atom' Atom Grit Term
  | Cons' (Base a) (Base a)
  | Lamb' (Base a) {- ^ closed-over subject -} (Code (Hop Stub a))
  | Name' Term (Base a)
--  | Push' (Base a) (Base a)
  --
  | Plus' (Base a)
  | Slam' (Base a) (Base a)
  | Look' (Base a) Stub
  --
  | Aura' Term
  | Fork' (Set Atom) Term
  | Cell' (Type a) (Base a) {- ^ closed-over subject -} (Code (Hop Stub a))
  | Gate' (Type a) (Base a) {- ^ closed-over subject -} (Code (Hop Stub a))
  | Mask' Term (Base a)
  | Noun'
  | Void'
  | Type'
  deriving (Functor, Foldable, Traversable)

-- | Read a value back into code, with reference to the current level.
loft :: Level -> Base a -> Code (Hop Stub a)
loft lvl = \case
  -- XX we should have some printout here if lvl > l, which is a serious
  -- invariant violation that should be more legible
  Stop' (Leg' (l, a)) -> Wing (New $ Leg $ peg (2 ^ (lvl - l)) a)
  Fore' x -> Wing (Old x)
  --
  Atom' a g au -> Atom a g au
  Cons' a b -> Cons (loft lvl a) (loft lvl b)
  -- NOTE Kovacs has the rule `VLam x t -> Lam x (quote (l + 1) (t $$ VVar l))`
  -- corresponding to our
  --   Lamb' s a ->
  --     Lamb Punt $ loft (l + 1) $ eval (Cons' s $ Stop' (Leg' (l + 1, 3))) a
  -- but I think that our With allows us to skip the inner eval
  -- XX decide whether this is true and prudent.
  -- Also notice the practice of lofting a level higher under a binder, e.g. in
  -- Kovacs `VPi x a b -> Pi x (quote l a) (quote (l + 1) (b $$ VVar l))`.
  -- If my understanding is correct, this single difference is the way in which
  -- subject oriented programming is easier to understand than de Bruijn.
  --
  Lamb' s a -> With (loft lvl s) $ Lamb Punt a
  Name' n a -> Name n (loft lvl a)
  --
  Plus' a -> Plus (loft lvl a)
  Slam' a b -> Slam (loft lvl a) (loft lvl b)
  Look' a s -> With (loft lvl a) $ Wing (New s)
  --
  Aura' au -> Aura au
  Fork' as au -> Fork as au
  Cell' t s c -> With (loft lvl s) $ Cell (loft lvl t) c
  Gate' t s c -> With (loft lvl s) $ Gate (loft lvl t) c
  Mask' n b -> Mask n (loft lvl b)
  Noun' -> Noun
  Void' -> Void
  Type' -> Type

-- | Axially project a value; i.e. implement Nock 0 or 9.
look :: Stub -> Base a -> Base a
look s b = home s $ walk a b
 where
  a = case s of
    Leg a -> a

  walk a b = case (cut a, b) of
    (Nothing,     c)               -> c
    (_,           Look' c (Leg i)) -> Look' c $ Leg (peg i a)
    (_,           Name' _ c)       -> walk a c
    (Just (L, b), Cons' c _)       -> walk b c
    (Just (R, b), Cons' _ c)       -> walk b c
    (Just _,      _)               -> Look' b s

  home s b = case s of
    Leg _ -> b

-- | Given a seminoun representing the subject, evaluate code into a seminoun
-- product.
eval :: Base a -> Code (Hop Stub a) -> Base a
eval sub = \case
  Wing (Old x) -> Fore' x
  Wing (New s) -> look s sub
  --
  Atom a g au -> Atom' a g au
  Cons c d -> Cons' (eval sub c) (eval sub d)
  Lamb _ a -> Lamb' sub a
  Name n a -> Name' n (eval sub a)
  --
  Plus c -> case eval sub c of
    Atom' a g au -> Atom' (a + 1) g au
    b -> Plus' b
  Slam c d -> case eval sub c of
    -- TODO replace with gold core thing
    Lamb' s c -> eval (Cons' s $ eval sub d) c
    b -> Slam' b (eval sub d)
  --
  Aura au -> Aura' au
  Fork as au -> Fork' as au
  Cell c d -> Cell' (eval sub c) sub d
  Gate c d -> Gate' (eval sub c) sub d
  Mask f c -> Mask' f (eval sub c)
  Noun -> Noun'
  Void -> Void'
  Type -> Type'
  --
  With c d -> eval (eval sub c) d
  Push c d -> eval (Cons' sub $ eval sub c) d
  Nest{cod} -> eval sub cod

--
-- Type checking resources
--

-- | Type checking monad
type Check = ReaderT [Act] (Either ([Act], Fail))

data Con a = Con
  { lvl :: Level
  , sut :: Type a
  , ken :: Base a
  }

-- | Grow the subject without knowledge, using an unevaluated type
hide :: Con a -> Code (Hop Stub a) -> Con a
hide Con{lvl, sut, ken} x = Con
  { lvl = lvl + 1
  , sut = Cell' sut ken x
  , ken = Cons' ken $ Stop' (Leg' (lvl + 1, 3))
  }

-- | Grow the subject without knowledge
hide' :: Con a -> Type a -> Con a
hide' Con{lvl, sut, ken} t = Con
  { lvl = lvl + 1
  , sut = Cell' sut ken $ loft (lvl + 1) t
  , ken = Cons' ken $ Stop' (Leg' (lvl + 1, 3))
  }

-- | Grow the subject with knowledge
shew :: Con a -> Base a -> Type a -> Con a
shew Con{lvl, sut, ken} b t = Con
  { lvl = lvl + 1
  , sut = Cell' sut ken $ loft (lvl + 1) t
  , ken = Cons' ken b
  }

-- | Construct a nondependent cell type. Thinking of providing this "freely"
-- as another constructor under Cell for efficiency. Make sure you know your
-- cell type is nondependent before attempting this!
--
-- The only difference between dependent and nondependent cells here is that we
-- loft below at lvl, rather than lvl + 1. XX think hard about this and test!
cell' :: Con a -> Base a -> Base a -> Base a
cell' Con{lvl, ken} l r = Cell' l ken $ loft lvl r


-- | Mode for fit-checking in `fits`: nest, cast, or exact equality.
data Fit
  = FitCast  -- ^ perform a coercibility check; i.e. ignore auras
  | FitNest  -- ^ perform a subtyping check
  | FitSame  -- ^ perform a type (or value) equivalence check
  deriving (Eq, Ord)

instance Show Fit where
  show = \case
    FitCast -> "cast"
    FitNest -> "nest"
    FitSame -> "same"

type Var a = (Eq a, Show a)

-- | Error reporting context, analogous to stack trace item.
data Act
  = forall a. Show a => ActFits Fit (Type a) (Type a)
  | forall a. Show a => ActFind (Type a) Wing
  | forall a. Show a => ActToil Fit (Pelt Wing) (Type a)
  | ActRomp (Pelt Wing)
  | forall a. Show a => ActWork Fit (Code Wing) (Type a)
  | ActPlay (Code Wing)
  | ActNote Text

data Fail
  = forall a. Show a => FindFail Term (Type a)
  | forall a. Show a => FitsFail Fit (Type a) (Type a)
  -- | forall a. Show a => SkinRash (Code a)  -- ^ not a valid pattern
  | RompWild (Pelt Wing)  -- ^ not enough info to extract type from pelt
  | forall a. Show a => NeedGate (Type a)  -- ^ type is not a gate
  | BailNote Text  -- ^ failure with note
  | BailFail  -- ^ unspecified failure

-- | Push an error reporting stack frame.
act :: Act -> Check b -> Check b
act a = local (a:)

bail :: Fail -> Check a
bail f = ask >>= \as -> lift $ Left (as, f)

bailFail :: Check a
bailFail = bail BailFail

--
-- Axial operations
--

data Step = L | R
  deriving (Eq, Ord, Show)

hop :: Step -> Axis -> Axis
hop L = peg 2
hop R = peg 3

peg :: Axis -> Axis -> Axis
peg a = \case
  0 -> 0  -- I guess? the hoon diverges
  1 -> a
  2 -> a * 2
  3 -> a * 2 + 1
  b -> b `mod` 2 + peg a (b `div` 2) * 2

-- | Combo of cap and mas. FIXME name change.
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

pole :: Axis -> Stub -> Stub
pole a = \case
  Leg b -> Leg (peg a b)

--
-- Core operations of the compiler
--

-- | Resolve the names in a Wing, producing the type of that part of the subject
-- and an axial representation of the wing. Am tempted to rename this to "clip".
find :: forall a. Var a => Con a -> Wing -> Check (Stub, Type a)
find sub@Con{lvl, sut, ken} win = act (ActFind sut win) do
  (st, Con{sut}) <- fond sub win
  pure (st, sut)
 where
  fond :: Con a -> Wing -> Check (Stub, Con a)
  fond con@Con{lvl, sut, ken} = \case
    [] -> pure (Leg 0, con)
    l:ls -> fond sub ls >>= \case
      -- (_, Arm{}) -> bail undefined  -- arm must occur leftmost
      (Leg a, con) -> do
        (st, con) <- limb con l
        pure (pole a st, con)

  limb :: Con a -> Limb -> Check (Stub, Con a)
  limb con = \case
    Axis a -> (Leg a,) <$> axis con a
    Ally f -> ally con f

  -- XX what should meaningfully happen with lvl here? or should we strip it out
  axis :: Con a -> Axis -> Check (Con a)
  axis con@Con{sut, ken} a = case (cut a, sut) of
    (Nothing,     _)           -> pure con
    (_,           Mask' _ t)   -> axis con{sut=t} a
    (Just (L, a), Cell' t _ _) -> axis con{sut=t, ken=(look (Leg 2) ken)} a
    -- XX under what circumstances will it be the case that we have an equation
    -- for the value of the head, but this knowledge is not inlined into the tail?
    (Just (R, a), Cell' _ s c) -> let hd = look (Leg 2) ken
                                      tl = look (Leg 3) ken
                                      ty = eval (Cons' s hd) c
                                  in  axis con{sut=ty, ken=tl} a
    -- XX an old note reads: "arguably for Liskov, should be Noun :("; rethink
    (_,           _)           -> bailFail

  ally :: Var a => Con a -> Term -> Check (Stub, Con a)
  ally con f = maybe (bail $ FindFail f sut) pure $ lope con
   where
    lope :: Con a -> Maybe (Stub, Con a)
    lope con@Con{sut, ken} = case sut of
      Mask' g t
        | f == g    -> pure (Leg 1, con)
        | otherwise -> Nothing

      Cell' t s c -> asum
        -- NB: We look to the right first, because =+ now pushes to the right.
        [ do let hd = look (Leg 2) ken
                 tl = look (Leg 3) ken
                 ty = eval (Cons' s hd) c
             (st, con) <- lope con{sut=ty, ken=tl}
             pure (pole 3 st, con)
        , first (pole 2) <$> lope con{sut=t, ken=(look (Leg 2) ken)}
        ]

      -- Gold/Lead

      _ -> Nothing
{-
-- | Inflate pattern to seminoun.
fill :: Pelt a -> Semi a
fill = \case
  Punt -> Wing Stop
  Peer _ -> Wing Stop
  Part c -> fmap Roll c
  Pair p q -> Cons (fill p) (fill q)
  Pace f p -> Name f (fill p)  -- although we could also strip the face
  Pest p t -> fill p

-- | Asymetrically merge pattern onto seminoun
meld :: Con a -> Semi a -> Pelt a -> Check (Semi a)
meld con ken pet = act (ActMeld con pet) $ go ken pet
 where
  go ken = \case
    Punt -> ken
    Peer _ -> ken
    Part c -> (fits FitSame con (Roll <$> c) ken $> ken) <|> case c of
      -- XX think more about this criterion. what if a = b = %foo and
      -- we are melding to %bar? This should also be meld-vain.
      -- I think the seminoun needs to be evaluated.
      -- Yes, both the seminoun and the expression in the pattern need to be
      -- evaluated, although frankly I don't know if we'll ever expose the full
      -- power of equality patterns to the user.
      a@Atom{} -> undefined
-}

{-
-- | Produce a seminoun corresponding to a pattern and a leveled axis modeling
-- the location of the subject being refined.
fill :: Con a => (Level, Axis) -> Pelt Stub -> Base a
fill (lvl, a) = \case
  Punt -> Stop' (Leg' (lvl, a))
-}

-- | Given a pattern, verify that it is compatibile with the given type.
-- This also effects a chilling step although it is not clear yet whether this
-- is needed or should take on some different form (e.g. stripping Pace and
-- Pest).
toil :: Var a => Con a -> Fit -> Pelt Wing -> Type a -> Check (Pelt Stub)
toil con@Con{ken} fit pet typ = act (ActToil fit pet typ) case pet of
  Punt -> pure Punt
  Peer f -> pure (Peer f)
  Part c -> Part <$> work con fit c typ -- Note nonreversal
  Pair p q -> undefined
  -- XX Honestly I'm not sure what to do here.
  -- Pace f p -> toil con fit p typ
  Pest p c -> do
    x <- work con FitNest c Type'
    let t = eval ken (fmap New x)
    fits fit t typ
    s <- toil con FitNest p t
    pure (Pest s x)

-- | Given a pattern, determine its type. Fail if not enough info (e.g. from
-- Pest/Past).
romp :: Var a => Con a -> Pelt Wing -> Check (Pelt Stub, Type a)
romp con@Con{ken} pet = act (ActRomp pet) case pet of
  Punt -> bail (RompWild pet)
  Peer _ -> bail (RompWild pet)
  Part c -> do (x, t) <- play con c; pure (Part x, t)
  Pair p q -> do (x, t) <- romp con p
                 (y, u) <- romp con q
                 pure (Pair x y, cell' con t u)
  -- Pace f p -> do (x, t) <- romp con p; pure (Pace f x, Mask' f t)
  Pest p c -> do
    x <- work con FitNest c Type'
    let t = eval ken (fmap New x)
    s <- toil con FitNest p t
    pure (Pest s x, t)

-- | Perform subtyping, coercibility, or equality check.
-- XX figure out proper encoding of recursion via cores or gates
fits :: forall a. Var a => Fit -> Type a -> Type a -> Check ()
fits fit t u = act (ActFits fit t u) case (t, u) of
  (Mask' _ v, w) -> fits fit v w
  (v, Mask' _ w) -> fits fit v w

  -- What is the right thing to do here? May be moot because we're almost
  -- certainly unifying with Mask, and also because these have no effect on the
  -- "chilled" post-work/play language. The old comment reads:
  --
  -- > I believe that for old-school faces, the fallback rules will have to
  -- > strip and succeed, and these rules will have to be listed immediately
  -- > after Look. Another nail in that coffin.
  --
  -- Another nail in the coffin of keeping Mask and Name separate.
  --
  (Name' _ v, w) -> fits fit v w
  (v, Name' _ w) -> fits fit v w

  (Noun', Noun') -> pure ()
  (Noun', _) -> fitsFail
  (_, Noun') -> case fit of
    FitSame -> fitsFail
    FitNest -> pure ()
    FitCast -> pure ()

  (Void', Void') -> pure ()
  (Void', _) -> case fit of
    FitSame -> fitsFail
    FitNest -> pure ()
    FitCast -> pure ()
  (_, Void') -> fitsFail

  (Stop' r, Stop' s)
    | r == s    -> pure ()
    | otherwise -> fitsFail
  (Stop'{}, _) -> fitsFail
  (_, Stop'{}) -> fitsFail

  (Fore' r, Fore' s)
    | r == s    -> pure ()
    | otherwise -> fitsFail
  (Fore'{}, _) -> fitsFail
  (_, Fore'{}) -> fitsFail

  -- XX confirm this, but I do think we have to decide definitional equality
  -- of terms as part of nest. E.g. Suppose someone has opaquely defined
  -- vect/$-(@ $ $) in their context. Because it's opaque, we cannot rely on
  -- inlining the definition in the context of nest checking. Thus to decide
  -- (vect 1 @) <?= (vect 1 *), we must determine that vect and vect are the
  -- same variable, 1 and 1 are the same term, and @ <= *.
  --
  -- But the above has a serious problem. Is vect covariant or contravariant?
  -- The rule implied above implicitly treats all such "opaque type functions"
  -- as covariant which is WRONG. Absent some variance marking solution,
  -- these should presumably be regarded as invariant.
  --
  -- To make invariance work, the proposed solution is to add a third Fit mode
  -- FitSame which does equality rather than subtyping, and switch to it under
  -- eliminators such as Slam (XX and other eliminators and introductors?).
  --
  (Atom' a _ _, Atom' b _ _) | a == b -> pure ()
  (Atom'{}, _) -> fitsFail
  (_, Atom'{}) -> fitsFail

  (Cons' v w, Cons' v' w') -> do fits fit v v'; fits fit w w'
  (Cons'{}, _) -> fitsFail
  (_, Cons'{}) -> fitsFail

  -- Evaluate the function bodies against a fresh opaque symbol. To get a fresh
  -- symbol, we have a bunch of options:
  --   - Track level as an argument to fits, as Kovacs does, incrementing under
  --     binders. We can then use (lvl + 1, 3) as the new Rump. Downside: not
  --     clear how to get this value when comparing two RTTIs at runtime.
  --   - Possibly, store a level in each saved, closed over, subject, taking the
  --     larger of the two. Think hard about whether this actually works.
  --   - Use Bound library style bullshit. Change the `a` type argument to allow
  --     an extra variable. This is a bit weird because the Codes are actually
  --     Hop Stub a, and bases have an extra Rump in them (so they are kinda
  --     Hop Rump a, except we couldn't get away with literally doing that), so
  --     we have to insert the variable just under the top level. But it works.
  --     On the other hand in a strict language paying the fmap cost upfront is
  --     not great? But we know for a fact that we'll be processing the entire
  --     body on success anyway, so? But also subtyping means if we're clever
  --     we can avoid fmapping entirely, if a can be encoded such that it's a
  --     subtype of Hop Stub a.
  --
  (Lamb'{}, Lamb'{}) -> fits fit (eval (Cons' s new) c) (eval (Cons' z new) d)
   where
    new = Fore' (New ())
    Lamb' s c = fmap Old t
    Lamb' z d = fmap Old u

  (Lamb'{}, _) -> fits fit (eval (Cons' s new) c) (Slam' x new)
   where
    new = Fore' (New ())
    Lamb' s c = fmap Old t
    x = fmap Old u

  (_, Lamb'{}) -> fits fit (Slam' x new) (eval (Cons' z new) d)
   where
    new = Fore' (New ())
    x = fmap Old t
    Lamb' z d = fmap Old u

  -- Elimination forms. Note that since Base, we will only encounter these
  -- "stuck" on some variable, possibly nested.
  (Plus' v, Plus' w) -> fits fit v w
  (Plus'{}, _) -> fitsFail
  (_, Plus'{}) -> fitsFail

  -- Since it hasn't been evaluated away, we are dealing with an opaque type
  -- function application. This means we have no choice but to regard the
  -- function as invariant in its argument.
  (Slam' v w, Slam' v' w') -> do fits fit v v'; fits FitSame w w'
  (Slam'{}, _) -> fitsFail
  (_, Slam'{}) -> fitsFail

  -- The assumption is that these are fully evaluated. This rules out Looks
  -- stacked on top of Looks, as well of Looks on top of cells. Accordingly the
  -- rules are pretty tight. I don't think there's any equiv of the beta-eta
  -- conversion we saw above with functions here.
  (Look' b st, Look' c ub)
    | st == ub  -> fits fit b c
    | otherwise -> fitsFail
  (Look'{}, _) -> fitsFail
  (_, Look'{}) -> fitsFail

  (Aura' au, Aura' ag) -> case fit of
    FitCast -> pure ()
    FitNest -> if ag `isPrefixOf` au then pure () else fitsFail
    FitSame -> if ag ==           au then pure () else fitsFail

  (Fork' cs au, Aura' ag) | fit /= FitSame -> fits @a fit (Aura' au) (Aura' ag)

  (Fork' cs au, Fork' ds ag) -> do
    fits @a fit (Aura' au) (Aura' ag)
    case fit of
      FitSame -> when (cs /= ds) fitsFail
      FitNest -> unless (cs `isSubsetOf` ds) fitsFail
      FitCast -> unless (cs `isSubsetOf` ds) fitsFail

  (Cell' v _ _, Cell' w _ _) -> do
    fits fit v w
    let
      fresh = New ()
      Cell' _ s c = fmap Old t
      Cell' _ z d = fmap Old u
    -- use the smaller type to do case analysis, if applicable
    for_ (cases fresh u) \cas ->
      fits fit
        (eval (Cons' s cas) c)
        (eval (Cons' z cas) d)

  (Gate' v _ _, Gate' w _ _) -> do
    -- Recall that the argument to a gate is contravariant
    fits fit w v
    let
      fresh = New ()
      Gate' _ s c = fmap Old t
      Gate' _ z d = fmap Old u
    -- use the smaller type to do case analysis, if applicable
    for_ (cases fresh w) \cas ->
      fits fit
        (eval (Cons' s cas) c)
        (eval (Cons' z cas) d)

  (Type', Type') -> pure ()

  (Aura'{}, _) -> fitsFail
  (_, Aura'{}) -> fitsFail
  (Fork'{}, _) -> fitsFail
  (_, Fork'{}) -> fitsFail
  (Cell'{}, _) -> fitsFail
  (_, Cell'{}) -> fitsFail
  (Gate'{}, _) -> fitsFail
  (_, Gate'{}) -> fitsFail


 where
  fitsFail = bail (FitsFail fit t u)

  -- For "finite" types, instead of generating a fresh variable, produce all
  -- the "cases" of that type. This could be generalized in a bunch of ways:
  --   - atoms: generate [0, +(<fresh>)]
  --   - cells of finite types
  -- The compiler accordingly does "case analysis" for cell and gate types,
  -- implementing the proper rules for $% nesting as a special case.
  cases :: b {- ^ fresh variable supply -} -> Type a -> [Base b]
  cases var = \case
    Aura' _ -> [Fore' var]  -- or: [Atom' 0 Sand au, Plus' (Fore' var)]?
    -- This is the case we need to make $% work.
    Fork' cs au -> fmap (\a -> Atom' a Rock au) $ toList cs
    -- NOTE it is actually possible to do something cool here, but it would
    -- require constructing an infinite, splittable fresh variable supply.
    Cell' t s c -> [Fore' var]
    -- It is absolutely infeasable to enumerate all gates
    Gate'{} -> [Fore' var]
    -- Also an important case
    Mask' _ t -> cases var t
    Noun' -> [Fore' var]
    Void' -> [Fore' var]
    Type' -> [Fore' var]
    -- The below will actually occur: e.g. stuck variables, applications
    _ -> [Fore' var]

-- | Given subject type and knowledge, verify that code has result type.
-- Since the expected result type is known in this mode, we can lighten the
-- user's annotation burden, e.g. on |= argument. Read about "bidirectional type
-- checking" to learn more.
work :: forall a. Var a => Con a -> Fit -> Code Wing -> Type a -> Check (Code Stub)
work con@Con{lvl, sut, ken} fit cod typ = act (ActWork fit cod typ)
  let playFits = do (x, t') <- play con cod
                    fits fit t' typ
                    pure x
      evil :: Code Stub -> Base a
      evil = eval ken . fmap New
  in case cod of
    Wing{} -> playFits

    -- for introduction forms except atoms, we push the type constraint inward
    -- this allows the user to type-annotate the whole of a big data structure
    -- indcluding cores and gates, without having to also annotate the insides
    -- unless they want to.
    Atom{} -> playFits

    -- XX not using cas-rule `want` functions here because I don't think it's
    -- appropriate to propagate in enCased type ascriptions here, but maybe
    -- I should rethink.
    --     Also, notice how we propagate the fit mode inward. This allows for
    -- "deep casts" e.g. `$-(@ @)`|=(a/@u +(a)). Pretty, but unsure if
    -- desirable yet.
    Cons c d -> case typ of
      Mask' f t -> work con fit cod t
      Cell' u sub e -> do
        x <- work con fit c u
        let can@Con{ken=kan} = shew con (evil x) u
        y <- work can fit d (eval kan e)
        pure (Cons x y)
      _ -> playFits

    Lamb p c -> case typ of
      Mask' f t -> work con fit cod t
      Gate' u sub e -> do
        r <- toil con fit p u
        -- FIXME we must gain or lose faces according to the pelt, not merely
        -- rely on the supplied type!
        let can@Con{ken=kan} = hide' con u
        y <- work can fit c (eval kan e)
        pure (Lamb r y)
      _ -> playFits

    Name f c -> work con fit c typ

    -- elimination forms just use nest
    Plus{} -> playFits
    Slam{} -> playFits
    --Equl{} -> playFits

    -- likewise with types
    Aura{} -> playFits
    Fork{} -> playFits
    Cell{} -> playFits
    Gate{} -> playFits
    --Gold{} -> playFits
    --Lead{} -> playFits
    Mask{} -> playFits
    Noun{} -> playFits
    Void{} -> playFits
    Type{} -> playFits

    With c d -> undefined

    Push c d -> do
      (x, t) <- play con c
      work (shew con (evil x) t) fit d typ

    Nest{} -> playFits

-- | Require the given type to be a function type.
-- XX Deppy had a cas rule here; why?
needGate :: Var a
         => Con a -> Type a -> Check (Type a, Base a, Code (Hop Stub a))
needGate con = \case
  Gate' t s c -> pure (t, s, c)
  Mask' _ t -> needGate con t
  t -> bail $ NeedGate t

-- | Given subject type and knowledge, determine product type of code
play :: forall a. Var a
     => Con a -> Code Wing -> Check (Code Stub, Type a)
play con@Con{lvl, sut, ken} cod = act (ActPlay cod) case cod of
  Wing w -> do
    (st, t) <- find con w
    pure (Wing st, t)

  Atom a Rock t -> pure (Atom a Rock t, Fork' (singleton a) t)

  Atom a Sand t -> pure (Atom a Sand t, Aura' t)

  Cons c d -> do
    (x, t) <- play con c
    (y, u) <- play con d
    -- XX the below invocation appears identical to "constructing a nondependent
    -- cell". Think hard about this.
    pure (Cons x y, Cell' t ken (loft (lvl + 1) u))

  Lamb p c -> do
    -- TODO replace with gold core
    (r, t) <- romp con p
    (x, u) <- play (hide' con t) c
    pure (Lamb r x, Gate' t ken (loft (lvl + 1) u))

  Name n c -> do
    (x, t) <- play con c
    pure (Name n x, Mask' n t)

  Plus c -> do
    -- Following 140, we do not propagate aura.
    x <- work con FitNest c (Aura' "")
    pure (x, Aura' "")

  Slam c d -> do
    (x, ct) <- play con c
    (at, s, rc) <- needGate con ct
    y <- work con FitNest d at
    pure (Slam x y, eval (Cons' s $ evil x) rc)

  Aura au -> pure (Aura au, Type')

  Fork as au -> pure (Fork as au, Type')

  Cell c d -> do
    x <- work con FitNest c Type'
    y <- work (hide con (fmap New x)) FitNest d Type'
    pure (Cell x y, Type')

  Gate c d -> do
    x <- work con FitNest c Type'
    y <- work (hide con (fmap New x)) FitNest d Type'
    pure (Gate x y, Type')

  Mask f c -> do
    x <- work con FitNest c Type'
    pure (Mask f x, Type')

  Noun -> pure (Noun, Type')

  Void -> pure (Void, Type')

  Type -> pure (Type, Type')

  With c d -> undefined

  Push c d -> do
    (x, t) <- play con c
    (y, u) <- play (shew con (evil x) t) d
    pure (Push x y, u)

  Nest{cod, typ} -> do
    x <- work con FitNest typ Type'
    let t = evil x
    y <- work con FitNest cod t
    -- XX think about this
    pure (y, t)

 where
  evil :: Code Stub -> Base a
  evil = eval ken . fmap New

-- | Untypecheck code to recover wings, in the shittiest possible way.
doze :: Stub -> Wing
doze = \case
  Leg a -> [Axis a]
