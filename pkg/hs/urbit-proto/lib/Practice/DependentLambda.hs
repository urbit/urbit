module Practice.DependentLambda where

import ClassyPrelude hiding (find)

-- TODO %foo needs to give rise to a singleton Fork type, not an Aura type
-- This means in principle, I think, that e.g. %0 and 0 must have different
-- abstract syntax. See %rock vs %sand in ++play

import Bound
import Control.Arrow ((<<<), (>>>))
import Control.Monad.Reader
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Foldable (foldrM)
import Data.Maybe (fromJust)
import Data.Set (isSubsetOf)
import Numeric.Natural

import Practice.HoonCommon (Atom, Axis, Grit(..), Term, Wing, Limb(..))

-- | Fully evaluated term. Since a common source of bugs is forgetting to
-- evaluate, it makes great sense for this to be a newtype.
newtype Base a = Base { loft :: Code a -- ^ "Unevalutate" a value back into code.
                      }
  deriving (Functor, Foldable, Traversable)

-- | Alias useful for pedagogical purposes.
type Type = Base

-- | Constraint on variable types
type Vary a = (Eq a, Show a)

-- | Fully desugared core syntax. Traditionally we would call this "term," (or
-- "tt") but in hoon this word is used for something else.
data Code a
  -- for this non-subject-oriented prototype, variables are separate from wings
  = Look a
  -- introduction forms
  | Atom Atom Grit Term
  | Cons (Code a) (Code a)
  | Lamb Term (Scope () Code a)  -- TODO pattern
  | Core (Map Term (Scope () Code a)) (Code a)  -- XX second should be sut
  | Name Term (Code a)  -- a=1
  -- elimination forms
  | Plus (Code a)
  | Slam (Code a) (Code a)
  | Wing Wing (Code a)
  | Equl (Code a) (Code a)
  -- | Edit TODO
  -- type constructors
  | Aura Term
  | Fork (Set Atom) Term
  -- | Join TODO $@
  | Cell (Code a) (Scope () Code a)
  | Gate (Code a) (Scope () Code a)
  | Gold (Map Term (Scope () Code a)) (Code a)
  | Lead (Map Term (Code a))
  | Mask Term (Code a)  -- ^ Liskov-compliant face type  a|@
  -- | Noun
  -- | Void
  | Type
  -- other expressions
  | Bind Term (Code a) (Scope () Code a)  -- =+, push value onto subject
  -- | Push (Map Term (Code a))  -- |%, push battery onto subject
  | Case (Code a) [Scope Int Code a] [Scope Int Code a]
  | Nest { cod :: Code a, typ :: Code a }  --  ^-  a/@  @\a
  | Cast { cod :: Code a, typ :: Code a }  --  ^!  `@`a
  -- todo Noun? or can we encode using recursors + atom + cell
  deriving (Functor, Foldable, Traversable)

-- These patterns let us deconstruct a type into component types, instead of
-- having to reapply Base on each subterm.
-- pattern BCell t c <- Cell (Base -> t) c where BCell t c = Cell (Base t) c

deriveEq1   ''Code
deriveOrd1  ''Code
deriveRead1 ''Code
deriveShow1 ''Code

deriving instance Eq a   => Eq   (Code a)
deriving instance Ord a  => Ord  (Code a)
deriving instance Read a => Read (Code a)
deriving instance Show a => Show (Code a)

deriving instance Show a => Show (Base a)

instance Applicative Code where
  pure = Look
  (<*>) = ap

instance Monad Code where
  return = Look

  Look a >>= f = f a
  Atom a g t >>= _ = Atom a g t
  Cons c d >>= f = Cons (c >>= f) (d >>= f)
  Lamb t c >>= f = Lamb t (c >>>= f)
  Core bat pay >>= f = Core (fmap (>>>= f) bat) (pay >>= f)
  Name n c >>= f = Name n (c >>= f)
  Plus c >>= f = Plus (c >>= f)
  Equl c d >>= f = Equl (c >>= f) (d >>= f)
  Slam c d >>= f = Slam (c >>= f) (d >>= f)
  Wing w c >>= f = Wing w (c >>= f)
  -- Edit TODO
  Aura t >>= _ = Aura t
  Fork opt t >>= _ = Fork opt t
  Cell c d >>= f = Cell (c >>= f) (d >>>= f)
  Gate c d >>= f = Gate (c >>= f) (d >>>= f)
  Gold bat pay >>= f = Gold (fmap (>>>= f) bat) (pay >>= f)
  Lead bat >>= f = Lead (fmap (>>= f) bat)
  Mask t c >>= f = Mask t (c >>= f)
  Type >>= _ = Type
  Bind t c d >>= f = Bind t (c >>= f) (d >>>= f)
  Case c ds es >>= f = Case (c >>= f) (fmap (>>>= f) ds) (fmap (>>>= f) es)
  Nest c d >>= f = Nest (c >>= f) (d >>= f)
  Cast c d >>= f = Cast (c >>= f) (d >>= f)

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

--
-- Type checking resources
--

-- | Type checking monad
type Check = ReaderT [Act] (Either ([Act], Fail))

-- | Error reporting context, analogous to stack trace item.
data Act
  = forall a. ActFits (a -> Text) Fit (Type a) (Type a)
  | forall a. ActFind (a -> Text) (Type a) Wing
  | forall a. ActToil (a -> Text) (Code a) (Type a)
  | forall a. ActRomp (a -> Text) (Code a)
  | forall a. ActWork (a -> Text) Fit (Code a) (Type a)
  | forall a. ActPlay (a -> Text) (Code a)
  | ActNote Text

data Fail
  = forall a. FitsFail (a -> Text) Fit (Type a) (Type a)
  | forall a. SkinRash (a -> Text) (Code a)  -- ^ not a valid pattern
  | forall a. NeedGate (a -> Text) (Type a)  -- ^ type is not a gate
  | BailNote Text  -- ^ failure with note
  | BailFail  -- ^ unspecified failure

bail :: Fail -> Check a
bail f = ask >>= \as -> lift $ Left (as, f)

bailFail :: Check a
bailFail = bail BailFail

-- | An impoverished subject type.
type Env a = a -> Type a

-- | Tracks which parts of the environment were defined by lets and thus have
-- inlinable expressions representing them.
type Semi a = a -> Maybe (Base a)  -- XX should this be base or code?
-- base is good for haskell because lazy, but it also means by-value, which will
-- be extra loopy and ultimately unacceptable. I guess fix later

-- | Full typing context for dependent type checking.
data Con a = Con
  { env :: Env a
  , sem :: Semi a
  , nom :: a -> Term
  }

act :: Act -> Check b -> Check b
act a = local (a:)

-- Note: Handling environmental translucency.
--
-- If you read presentations of type theory, you'll notice notation of the form
--
--     \Gamma |- e : t
--
-- In non-dependent type theories, \Gamma (the capital greek letter) is a list
-- of variable typings x : t. This list is strongly analogous to the "subject
-- type" in hoon.
--
-- In dependent theories, however, \Gamma is a list of entries of the form x : t
-- OR x = e : t, where e is an expression that is allowed to mention variables
-- earlier in the list. I call the first kind "opaque entries" and the second
-- kind "translucent entries." (I have no idea if there is a standard term for
-- these.) Now, as you pass under a new binder, you push something to the end of
-- your \Gamma context; what kind of thing depends on what kind of binder.
-- Basically, lets give rise to translucent entries, while everything else gives
-- rise to opaques. The explanation for this is simple: lets are the only thing
-- with an "rhs" which we can use in place of `e`.
--
-- As an example, consider (I'll use hoonish syntax but this is meant to be an
-- example of an orthodox dependent language, not a subject-oriented hoon):
--
--     |=  a/@
--     =/  b/^  [a +(a)]
--     YOU-ARE-HERE
--
-- At the cursor, the context \Gamma is a : @, b = [a +(a)] : ^. Now suppose we
-- continued the program:
--
--     |=  a/@
--     =/  b/^  [a +(a)]
--     =/  x/(vect @t +(a))  (reap @t +:b 'hi there compiler')
--
-- (Here vect is a list whose length is part of the type.) Does this program
-- type check? Well, reap will produce a vector of type `(vect @t +:b)`, so the
-- question is whether this nests in the expected type of `x`. We wind up check-
-- ing whether `+(a)` is equivalent to `+:b`. Because `b` is translucent, we can
-- replace it with its rhs, then evalute to get `+(a)`. Meanwhile, `+(a)` is
-- stuck on `a` because `a` is opaque. So both expressions evaluate to the same
-- normal form, so we regard them as equal.
--
-- Let's drill down a bit more on two points. First, nest checking relies on a
-- notion of "expressions being equivalent." The truly desired notion of
-- equivalence is "extensional equivalence": the program fragments have the same
-- outputs on the same inputs. This is a sufficient condition for type safety,
-- but it's too strong; in fact, it's undecidable. So we shittily approximate it
-- by saying "two programs are equal if they evaluate to the same normal form."
-- (Note that this too is undecidable unless we can guarantee that evaluation
-- terminates, hence the traditional emphasis on totality checking. Whatever.)
-- This is shitty because it rejects some programs which the "true," impossible
-- type checker admits. But static typing already involves rejecting "ok" code.
-- Moreover, it's by no means the only possible shitty approximation, just the
-- traditional one. One could also compare program texts without doing any
-- evaluation. That rejects more programs, but involves a simpler, faster
-- compiler. This is what Dependent Haskell proposes to do, for compatibility
-- reasons. Bottom line: how hard you work to equate things is a "tradeoff dial"
-- in implementing dependent types.
--
-- Second, a brief remark on what it means for an expression to be stuck. This
-- has a specific, intuitive techincal meaning: you've put an opaque variable
-- inside the head of an elimination form.
--
-- (One final remark: translucent rhss also play a very important role in
-- elaboration. But that is out of scope for today.)
--
-- All of the foregoing leads us to ask, what is its equivalent in the subject-
-- oriented pattern? While the subject is an elegant way to combine the concepts
-- of \Gamma and type in the classical world, it is not immediately clear how
-- to extend this to the new dependent world. Here are some options.
--
-- - Just don't do translucent rhss, as in Dependent Haskell. (This probably
--   blocks elaboration, and it's unclear in practice how hard it is to write
--   dependent programs without this affordance.)
--
-- - Create a new "singleton type" construct. Thus instead of `x = e : t`, one
--   has `x : Solo e t`. But now, in full subject-oriented grandeur, "singleton
--   types" are available to users everywhere types can be found. What are the
--   implications of this? It it requires further study.
--
-- - Don't track rhss in the subject type but instead track them in a separate
--   argument to every type-checking function. If you tisgar something opaque,
--   you get a fully opaque subject. I find this the least persuasive option,
--   but it's the one I choose here anyway because I know it will definitely
--   work, and I want to proceed step-by-step.
--
-- - Here's a particularly hoony-feeling idea: Track rhss only for the arms of
--   gold cores. The idea is that the Gold constructor ([%gold *] case) of the
--   type language stores arm bodies (as in existing hoon) rather than arm
--   return types, thereby becoming translucent, but lead cores are opaque.
--   Since gold cores are used for "libraries" and "library functions" are
--   particularly things we want to inline (?), perhaps this strikes a good
--   balance. Tisluses will be opaque, but you can always barcen something if
--   you need it to be translucent. One downside of this idea is that it forces
--   us (I think??) to make gold cores substantially less nest-compatible with
--   each other. Basically only gold cores with identical (or eval-identical)
--   arms can nest under each other in this world (again, I think), rather than
--   the more permissive situation in 140 where we compare their arm return
--   types. But do we care?

-- | Promote a trm to be usable in a larger context. It would be nice if this
-- could be more efficient.
grow :: Functor f => f a -> f (Var b a)
grow = fmap F

-- | Make a scope which doesn't mention the new variable. Constant time.
blow :: Applicative f => f a -> Scope b f a
blow = Scope . pure . F

-- | Grow the environment to record a new variable with opaque rhs.
hide :: Con a -> Term -> Type a -> Con (Var () a)
hide Con{env, sem, nom} nam typ = let t = grow typ
                                      e = grow <$> env
                                      s = fmap grow <$> sem in Con
  { env = \case
      B () -> t
      F x  -> e x
  , sem = \case
      B () -> Nothing
      F x  -> s x
  , nom = \case
      B () -> nam
      F x  -> nom x
  }

rash :: Code c -> Term
rash = \case
  Mask t _ -> t
  Name t _ -> t
  _ -> "???"

-- | Grow the environment to record a new variable with translucent rhs.
shew :: Vary a => Con a -> Term -> Code a ->  Type a -> Con (Var () a)
shew Con{env, sem, nom} nam cod typ = let c = grow $ eval sem cod
                                          t = grow typ
                                          e = grow <$> env
                                          s = fmap grow <$> sem in Con
  { env = \case
      B () -> t
      F x  -> e x
  , sem = \case
      B () -> Just c
      F x  -> s x
  , nom = \case
      B () -> nam
      F x  -> nom x
  }

-- | Evaluate code to produce base.
eval :: Vary a => Semi a -> Code a -> Base a
eval sem = \case
  Look x -> maybe (Base $ Look x) id (sem x)

  a@Atom{} -> Base a

  Cons c d -> Base $ Cons (go c) (go d)

  l@Lamb{} -> Base l  -- XX do I want to eval under binders, nf style?

  Core bat pay -> Base $ Core bat (go pay)

  Name n c -> Base $ Name n $ go c  -- NB: we do not strip names during eval!

  Plus c -> Base case go c of
    Atom a g au -> Atom (a + 1) g au
    -- Our mantra is: invalid programs get stuck.
    x -> Plus x

  Slam c d -> Base case go c of
    Lamb _ b -> go $ instantiate (const $ go d) b
    f -> Slam c $ go d

  Wing w c -> Base $ foldr limn (go c) w
   where
    limn l c = maybe (replant l c) id $ limb l c

    replant l = \case
      Wing w c -> Wing (l:w) c
      c -> Wing [l] c

    limb (Ally n) (Name m c) | m == n    = Just c
                             | otherwise = Nothing
    limb (Ally n) (Cons c d)             = limb (Ally n) d <|> limb (Ally n) c
    limb (Ally n) (Core bat pay)         = go <$> instantiate (const pay)
                                             <$> lookup n bat
                                           <|> limb (Ally n) pay
    limb (Ally n) _                      = Nothing
    limb (Axis a) c                      = case (cut a, c) of
      (Nothing,     c)          -> Just c
      (_,           Name _ c)   -> limb (Axis a) c
      (Just (L, b), Cons c _)   -> limb (Axis b) c
      (Just (R, b), Cons _ c)   -> limb (Axis b) c
      (Just (L, b), Core _ _)   -> Nothing
      (Just (R, b), Core _ pay) -> limb (Axis b) pay
      (Just _,      _)          -> Nothing

  Equl c d -> Base $ equl (go c) (go d)

  c@Aura{} -> Base c

  c@Fork{} -> Base c

  -- Consider {(pair @ @) (pair @ @)}. If the left is equivalent to {p/@ q/@},
  -- then so should the right. But that means we need to eval under the binder.
  Cell c d -> Base $ Cell (go c)
                          (toScope $ loft $ eval (hidden sem) $ fromScope d)

  -- Presumably likewise.
  Gate c d -> Base $ Gate (go c)
                          (toScope $ loft $ eval (hidden sem) $ fromScope d)

  -- XX again eval under binder q
  Gold bat pay -> Base $ Gold bat pay

  Lead bat -> Base $ Lead $ fmap go bat

  Mask n c -> Base $ Mask n $ go c

  Type -> Base Type

  Bind _ c d -> eval sem $ instantiate (const $ go c) d

  Case{} -> undefined

  Nest{cod} -> eval sem cod

  Cast{cod} -> eval sem cod

 where
  go = loft . eval sem

  hidden sem = let s = fmap grow <$> sem in \case
    B () -> Nothing
    F x  -> s x

  -- This may be a problem. How do we simulate nocklike equality of functions
  -- and nouns at compile time? Answer: if your program depends on nocklike
  -- behavior, the evaluator gets stuck on the relevant term. That is,
  --
  --    .=  1  |=  x  x
  --
  -- "evaluates to itself" at compile time. This is a legitimate thing to do
  -- because "evaluation" here is a stand in for the equivalence relation on
  -- terms that the compiler uses to decide nesting, that is, x ~ y iff eval x
  -- == eval y. We are free to choose any equivalence relation finer than
  -- extensional/behavioral equivalence. Indeed, Dependent Haskell plans to use
  -- *textual equality*. In this way, the runtime evaluator is allowed to be
  -- "more fully featured" than the compile time one, without loss of integrity.
  equl c d =
    let out c d = case (c, d) of
          (Atom a _ _, Atom b _ _) -> Just (a == b)
          (Cons c d,   Cons e f) -> (&&) <$> out c e <*> out d f
          (Atom{},     Cons{})     -> Just False
          (Cons{},     Atom{})     -> Just False
          -- XX that reminds me, people can also apply equality to reified types
          -- pls reason about that thx
          _ | c == d           -> Just True  -- too cheeky? unwise?
            | otherwise        -> Nothing
    in case out c d of
      Just True  -> Atom 0 Rock "f"
      Just False -> Atom 1 Rock "f"
      Nothing    -> Equl c d

-- | A common pattern is to fill in a binder and continue evaluating.
-- XX in the subject oriented paradigm, we have to instead extend the context
-- with show and then evaluate
flow :: Vary a => Con a -> Code a -> Scope () Code a -> Base a
flow con cod sop =
  eval (sem con) (instantiate (const $ loft $ eval (sem con) cod) sop)

-- | Take an action for each arm in both maps, failing if the maps don't line up
farm :: Ord k => Map k a -> Map k b -> (a -> b -> Check ()) -> Check ()
farm b b' act = do
  let arms = keys b
  when (arms /= keys b') bailFail
  for_ arms \arm -> do
    let a  = fromJust $ lookup arm b
    let a' = fromJust $ lookup arm b'
    act a a'

-- | Mode for fit-checking in `fits`: either nest or cast.
data Fit
  = FitCast  -- ^ perform a coercibility check; i.e. ignore faces (masks), auras
  | FitNest  -- ^ perform a subtyping check
  | FitSame  -- ^ perform a type equivalence check
  deriving (Eq, Ord)

instance Show Fit where
  show = \case
    FitCast -> "cast"
    FitNest -> "nest"
    FitSame -> "same"

-- | Perform subtyping, coercibility, or equality check.
-- XX figure out proper encoding of recursion via cores or gates
fits :: Vary a => Con a -> Fit -> Type a -> Type a -> Check ()
fits con fit t u = act (ActFits (nom con) fit t u) $ go con fit t u
 where
  go :: Vary a => Con a -> Fit -> Type a -> Type a -> Check ()
  go con fit t u = case (loft t, loft u) of
    -- These have to come first to avoid being excluded by the (_, Foo{}) cases.
    (Mask n c, Mask m d) | n == m -> go con fit (Base c) (Base d)
    (Mask _ c, d)
      | FitSame <- fit -> fitsFail
      | otherwise      -> go con fit (Base c) (Base d)
    (c, Mask _ d)
      | FitSame <- fit  -> fitsFail
      | otherwise       -> go con fit (Base c) (Base d)

    (Look x, Look y) | x == y -> pure ()
    {-
    -- XX Actually, I think it's an implementation error for either of the next
    -- two to fire. It would indicate that we haven't evaluated sufficiently.
    (Look x, d) | Just c <- sem con x -> go con fit c (Base d)
    (c, Look x) | Just d <- sem con x -> go con fit (Base c) d -}
    (Look _, _) -> fitsFail
    (_, Look _) -> fitsFail

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
    (Atom a _ _, Atom b _ _) | a == b -> pure ()
    (Atom{}, _) -> fitsFail
    (_, Atom{}) -> fitsFail

    (Cons c d, Cons c' d') -> do go con fit (Base c) (Base c')
                                 go con fit (Base d) (Base d')
    (Cons{}, _) -> fitsFail
    (_, Cons{}) -> fitsFail

    (Lamb _ c, Lamb _ d) | c == d -> pure ()
    (Lamb{}, _) -> fitsFail
    (_, Lamb{}) -> fitsFail

    -- The apparently free choice of p or p' in show goes away in the subject-
    -- oriented context, but probably comes back even worse in the tisgar case.
    -- Among other things, it appears we will need to tisgar (perform arbitrary
    -- computations on) semis. Actually, in the case below, we will have to
    -- "push" the head of the semi into the body of the expression, sort of the
    -- opposite.
    (Core b p, Core b' p') -> do
      go con fit (Base p) (Base p')
      farm b b' \a a' -> go con fit (flow con p a) (flow con p' a')
    (Core{}, _) -> fitsFail
    (_, Core{}) -> fitsFail

    -- I believe that for old-school faces, the fallback rules will have to
    -- strip and succeed, and these rules will have to be listed immediately
    -- after Look. Another nail in that coffin.
    (Name n c, Name m d) | n == m -> go con fit (Base c) (Base d)
    (Name{}, _) -> fitsFail
    (_, Name{}) -> fitsFail

    -- Elimination forms. Note that since Base, we will only encounter these
    -- "stuck" on some variable.
    (Plus c, Plus d) -> go con fit (Base c) (Base d)
    (Plus{}, _) -> fitsFail
    (_, Plus{}) -> fitsFail

    -- Since it hasn't been evaluated away, we are dealing with an opaque type
    -- function application. This means we have no choice but to regard the
    -- function as invariant in its argument.
    (Slam c d, Slam c' d') -> do go con fit (Base c) (Base c')
                                 go con FitSame (Base d) (Base d')
    (Slam{}, _) -> fitsFail
    (_, Slam{}) -> fitsFail

    -- XX we should recognize axes equivalent to faces :/
    -- n.b.: things like the "empty wing" should have been evaluated out by now
    (Wing w c, Wing v d) | w == v -> go con fit (Base c) (Base d)
    (Wing{}, _) -> fitsFail
    (_, Wing{}) -> fitsFail

    (Equl c d, Equl c' d') -> do go con fit (Base c) (Base c')
                                 go con fit (Base d) (Base d')
    (Equl{}, _) -> fitsFail
    (_, Equl{}) -> fitsFail

    (Aura au, Aura ag) -> case fit of
      FitCast -> pure ()
      FitNest -> if ag `isPrefixOf` au then pure () else fitsFail
      FitSame -> if ag ==           au then pure () else fitsFail

    (Fork cs au, Aura ag) | fit /= FitSame ->
      go con fit (Base $ Aura au) (Base $ Aura ag)

    (Fork cs au, Fork ds ag) -> do
      go con fit (Base $ Aura au) (Base $ Aura ag)
      case fit of
        FitSame -> when (cs /= ds) fitsFail
        _ -> unless (cs `isSubsetOf` ds) fitsFail

    (Cell c d, Cell c' d') -> do
      go con fit (Base c) (Base c')
      -- when we pass into the right of {a|t u} {b|t' u'}, we have to chose
      -- one of a,b to be the variable name. this choice is arbitrary and
      -- affects only error messages
      go (hide con (rash c) $ Base c)
         fit
         (Base $ fromScope d)
         (Base $ fromScope d')

    (Gate c d, Gate c' d') -> do
      go con fit (Base c') (Base c)
      go (hide con (rash c') $ Base c')
         fit
         (Base $ fromScope d)
         (Base $ fromScope d')

    -- TODO (Gate{}, Cell Noun Noun) -> pure ()

    (Gold bat pay, Gold bat' pay') -> do
      go con fit (Base pay) (Base pay')
      farm bat bat' \a a' -> go
        -- the choice of rash is arbitrary here, again
        (hide con (rash pay) $ Base pay)
        fit
        (Base $ fromScope a)
        (Base $ fromScope a')

    (Gold bat pay, Lead bat') ->
      farm bat bat' \a a' -> go
        (hide con (rash pay) $ Base pay)
        fit
        (Base $ fromScope a)
        (Base $ grow $ a')

    (Lead bat, Lead bat') ->
      farm bat bat' \a a' ->
        go con fit (Base a) (Base a')

    -- TODO (Gold _ pay, Cell Noun pay') -> go con fit (Base pay) (Base pay')
    -- TODO (Lead{}, Cell Noun Noun) -> pure ()

    -- for Mask, see top of function

    (Type, Type) -> pure ()

    (Aura{}, _) -> fitsFail
    (_, Aura{}) -> fitsFail
    (Fork{}, _) -> fitsFail
    (_, Fork{}) -> fitsFail
    (Cell{}, _) -> fitsFail
    (_, Cell{}) -> fitsFail
    (Gate{}, _) -> fitsFail
    (_, Gate{}) -> fitsFail
    (Gold{}, _) -> fitsFail
    (_, Gold{}) -> fitsFail
    (Lead{}, _) -> fitsFail
    (_, Lead{}) -> fitsFail
    (Type,   _) -> fitsFail
    (_,   Type) -> fitsFail
   where
    fitsFail = bail (FitsFail (nom con) fit t u)

    -- Bind, Nest, Cast should be impossible because of evaluation
    -- TODO cas rule

{-
  -- other expressions
  | Bind (Code a) (Scope () Code a)
  | Case (Code a) [Scope Int Code a] [Scope Int Code a]
  | Nest { cod :: Code a, typ :: Code a }  --  ^-  a/@  @\a
  | Cast { cod :: Code a, typ :: Code a }  --  ^!  `@`a
-}


-- we need `cod` so we can substitute -:cod in the dependency on the rhs of a
-- cell type. I think it goes away with dependent types, since all finds are
-- against `.`.
find :: Vary a => Con a -> Code a -> Type a -> Wing -> Check ([Axis], Type a)
find con cod typ win = act (ActFind (nom con) typ win)
                     $ foldrM (go con cod) ([], typ) win
 where
  go :: Vary a
     => Con a
     -> Code a
     -> Limb
     -> ([Axis], Type a)
     -> Check ([Axis], Type a)
  go con cod (Axis a) (as, t) = (a:as,) <$> step con cod a t
  go con cod (Ally n) (as, t) = walk con cod n t <&> \(a, t) -> (a:as, t)

  step :: Vary a => Con a -> Code a -> Axis -> Type a -> Check (Type a)
  step con cod a t = case (cut a, loft t) of
    (Nothing,     _)        -> pure t
    (_,           Mask _ c) -> step con cod a (Base c)
    (Just (L, a), Cell c _) -> step con (heed 2 cod) a (Base c)
    (Just (R, a), Cell _ d) -> step con (heed 3 cod) a $ flow con (heed 2 cod) d
    (Just (L, _), Gold _ _) -> bailFail  -- TODO Noun?
    (Just (R, a), Gold _ c) -> step con (heed 3 cod) a (Base c)
    (Just (L, _), Lead _)   -> bailFail  -- TODO Noun?
    (Just (R, _), Lead _)   -> bailFail  -- TODO Noun?
    (_,           _)        -> bailFail  -- arguably for Liskov, should be Noun :(

  heed :: Axis -> Code a -> Code a
  heed ax = \case
    Wing (Axis a : w) c -> Wing (Axis (peg ax a) : w) c
    Wing w c -> Wing (Axis ax : w) c
    c -> Wing [Axis ax] c

  fend :: Term -> Map Term a -> Axis
  fend _ _ = 0  -- XX FIXME

  walk :: Vary a => Con a -> Code a -> Term -> Type a -> Check (Axis, Type a)
  walk con cod n = maybe bailFail pure . lope con cod
   where
    lope con cod = loft >>> \case
      Mask m c
        | n == m    -> pure (1, Base c)
        | otherwise -> Nothing

      Cell c d ->
        -- NB: We look to the right first, because =+ now pushes to the right.
        first (peg 3) <$> lope con (heed 3 cod) (flow con (heed 2 cod) d) <|>
        first (peg 2) <$> lope con (heed 2 cod) (Base c)

      Gold bat pay ->
        do
          t <- lookup n bat
          let a = fend n bat
          pure (peg 2 a, flow con (heed 3 cod) t)
        <|> first (peg 3) <$> lope con (heed 3 cod) (Base pay)

      Lead bat -> do
        t <- lookup n bat
        let a = fend n bat
        pure (peg 2 a, Base t)

      _ -> Nothing

-- | Check that a pattern has the given type, producing the types of its
-- variables.
toil :: Vary a
     => Con a
     -> (Int -> Term)
     -> Code (Var Int a)
     -> Type a
     -> Check (IntMap (Type a))
toil con nom e t = undefined

-- | Determine the type of a pattern, if possible, also producing the types of
-- its variables
romp :: Vary a
     => Con a
     -> (Int -> Term)
     -> Code (Var Int a)
     -> Check (Type a, IntMap (Type a))
romp con nom e = undefined

-- | Given subject type, verify that code has result type. Since the expected
-- result type is known in this mode, we can lighten the user's annotation
-- burden, e.g. on |= argument. Read about "bidirectional type checking" to
-- learn more.
work :: Vary a => Con a -> Fit -> Code a -> Type a -> Check ()
work con fit e tau@(Base t) = act (ActWork (nom con) fit e tau)
  let playFits = do t' <- play con e
                    fits con fit t' tau
  in case e of
    Look{} -> playFits

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
    Cons c d -> case t of Cell u v -> do work con fit c (Base u)
                                         work con fit d (flow con c v)
                          _ -> playFits

    -- This lets us, e.g., turn with |=(a +(a)) without annotating a.
    Lamb x c -> case t of Gate u v -> work (hide con x (Base u)) fit
                                        (fromScope c) (Base $ fromScope v)
                          _ -> playFits

    -- XX this is actually all wrong. The binder in the arms must refer to the
    -- whole core. Absent subject orientation, this means |% must have a
    -- syntactic element for the selfname.
    Core bat pay -> case t of Gold tat tay -> do let tie = Base tay
                                                 work con fit pay tie
                                                 -- XX should we play pay
                                                 -- instead of using tie?
                                                 farm bat tat \a t -> work
                                                   -- another arbitrary rash
                                                   (shew con (rash pay) pay tie)
                                                   fit
                                                   (fromScope a)
                                                   (Base $ fromScope t)
                              Lead tat -> do tay <- play con pay
                                             farm bat tat \a t -> work
                                               (shew con (rash pay) pay tay)
                                               fit
                                               (fromScope a)
                                               (grow $ Base t)
                              _ -> playFits

    Name n c -> case t of Mask m t | n == m -> work con fit c tau
                          _ -> playFits

    -- elimination forms just use nest
    Plus{} -> playFits
    Slam{} -> playFits
    Wing{} -> playFits
    Equl{} -> playFits

    -- likewise with types
    Aura{} -> playFits
    Fork{} -> playFits
    Cell{} -> playFits
    Gate{} -> playFits
    Gold{} -> playFits
    Lead{} -> playFits
    Mask{} -> playFits
    Type{} -> playFits

    Bind x c d -> do tc <- play con c
                     work (shew con x c tc) fit (fromScope d) (grow tau)

    Case s cs ds -> do st <- play con s
                       undefined
                       {-
                       -- ugh again wrong, because of the Int scope
                       for cs \c -> work con Nest c st
                       -- WRONG, we must do refinement, i.e. substitution
                       for ds \d -> work con fit d t
                       -}

    Nest{} -> playFits
    Cast{} -> playFits


-- | Require the given type to be a function type.
-- XX Deppy had a cas rule here; why?
needGate :: Vary a => Con a -> Type a -> Check (Type a, Scope () Code a)
needGate con = \case
  Base (Gate c d) -> pure (Base c, d)
  Base (Mask _ t) -> needGate con (Base t)
  t -> bail $ NeedGate (nom con) t


-- | Given subject type, determine result type of code.
play :: Vary a => Con a -> Code a -> Check (Type a)
play con cod = act (ActPlay (nom con) cod) case cod of
  Look v -> pure $ env con v

  Atom a Rock t -> pure $ Base $ Fork (singleton a) t
  Atom _ Sand t -> pure $ Base $ Aura t

  Cons c d -> do
    Base tc <- play con c
    Base td <- play con d
    pure $ Base $ Cell tc (blow td)

  Lamb _ c -> do
    -- TODO we need to get the type out of the pattern, if any
    tb <- undefined
    -- we also need to need to regard the body against subject a $-gold core
    -- which is not possible in the reduced paradagim of this prototype.
    undefined

  Core bat pay -> do
    tay <- play con pay
    -- XX avoid going on an infinite loop
    mfix \t -> undefined

  Name n c -> do
    Base t <- play con c
    pure $ Base $ Mask n t

  Plus c -> do
    -- Following 140, we do not propagate aura.
    work con FitNest c (Base $ Aura "")
    pure (Base $ Aura "")

  Slam c d -> do
    (td, tr) <- needGate con =<< play con c
    work con FitNest d td
    pure $ flow con c tr

  Wing w c -> do
    t <- play con c
    snd <$> find con c t w

  Equl c d -> do
    -- XX why did I write this?
    -- work con FitNest c undefined -- Noun
    -- work con FitNest d undefined -- Noun
    _ <- play con c
    _ <- play con d
    pure $ Base $ Fork (setFromList [0,1]) "f"

  Aura{} -> pure $ Base Type

  Fork{} -> pure $ Base Type

  Cell c d -> do
    -- idea: unify Name and Mask, have a=Type cast under Type, but not nest,
    -- use FitCast here. What would go horribly wrong?
    work con FitNest c (Base Type)
    let c' = eval (sem con) c
    work (hide con (rash c) c') FitNest (fromScope d) (Base Type)
    pure $ Base Type

  Gate c d -> do
    work con FitNest c (Base Type)
    let c' = eval (sem con) c
    work (hide con (rash c) c') FitNest (fromScope d) (Base Type)
    pure $ Base Type

  Gold bat pay -> do
    work con FitNest pay (Base Type)
    let t = eval (sem con) pay
    for bat \b -> work (hide con (rash pay) t) FitNest (fromScope b) (Base Type)
    pure $ Base Type

  -- likewise under the a=Type idea, we'd FitCast here
  Lead bat -> do
    for bat \b -> work con FitNest b (Base Type)
    pure $ Base Type

  Mask _ c -> do
    work con FitNest c (Base Type)
    pure $ Base Type

  Type -> pure $ Base Type

  Bind x c d -> do
    tc <- play con c
    -- XX again consider alternatives, but I think this is actually the
    -- canonical thing to do; test in idris?
    Base u <- play (shew con x c tc) (fromScope d)
    pure $ flow con c (toScope u)

  -- The easy answer is to say it has type Case c cs ts, where ts is the type
  -- of each d. The better answer is to attempt some good faith computed fork,
  -- and fail and require a type annotation if it fails.
  Case c cs ds -> undefined

  Nest{cod, typ} -> do
    let t = eval (sem con) typ
    work con FitNest cod t
    pure t

  Cast{cod, typ} -> do
    let t = eval (sem con) typ
    work con FitCast cod t
    pure t
