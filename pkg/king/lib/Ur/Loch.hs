{-
    - TODO Implement signed integers as a "module".

    - TODO Jet the `subtract` module.

    - TODO Write `subtract :: {Int @ @ Int.t}`
      - Jet subtract applied to the Int module.

    - TODO Serialize the `subtract` closure.

    - TODO Type-Check everything besides Eval

    - TODO Type-Check Eval (probably needs a type argument too)
      - Eval :: (t:Type) -> AST -> t

    - TODO Serialize a closure with type information?

    - Compile Nock to Loch.

    - Think through optimization:

      - Can I optimise the `cons` function (`\\[1 0]`)?

        - Not really.

        - It returns a closure.

        - Functions that operate on primitive data can be replaced.

        - Functions that take n arguments, and then return a primitive
          data type can be replaced.

          - By "can be replaced". I mean that I can simply store the
            arguments instead of manipulating the AST. I can always
            recover the old behavior by simply reducing the AST against
            the stored arguments.

        - Can I store functions as `[unsatisfied/@ cod (list val)]`?

          - For example,

            - `\\[1 0]`         becomes `[2 \\[1 0] ~[]]`
            - `(\\[1 0] #0)`    becomes `[1 \\[1 0] ~[#0]]`
            - `(\\[1 0] #0 #0)` becomes `[0 \\[1 0] ~[#0 #0]]`
            - Which evaluates to [#0 #0]

        - Still trying to wrap my mind around which problem I'm actually
          thinking about.

          - Usually, you optimize by *replacing* code with a different
            implementation that has the same behavior.

          - Urbit has a unique constraint, which is that the current state
            of the system at any given point in time must be *completely*
            predictable, and must be portable to another implementation.

          - This seems like it should make optimization more difficult,
            but I don't totally understand how or in which specific
            places.

          - What are those places?

          - All functions must maintain enough information to be
            serialized, and the result of that serialization must be
            exactly the same as if we simply performed reductions on
            the AST.

          - The state of an urbit is always a closure.

          - The state of an urbit is always a closure.

          - So, everything must happen by reduction, except for what?

            - Anything that's guarenteed to be fully evaluated before
              the end of the event.

            - For example,

              - `add/{[@ @] @}` can be optimized.

                - Once invoked, it will never be partially evaluated.

            - `turn/{[{a b} (list a)] (list b)}` can be optimized.

              - It might return a list of closures, but that's okay. None
                of the guts of the `turn` AST need every be exposed.

                ```
                turn :: (a -> b, [a]) -> [b]
                turn (f, xs) = go [] xs
                  where
                    go acc []     = reverse acc
                    go acc (x:xs) = go (f x:acc) xs
                ```

            - So basically, closures are very expensive.

            - Use primitive data types.

            - Jet aggressively.
-}

module Ur.Loch
  ( Lv(..)
  , Lc(..)
  , lc, dc, ec
  , ex_id
  , ex_cons
  , ex_zeros
  , ex_one_two_three
  , ex_jetted_add
  , ex_eva

  , ex_uni
  , hs_uni

  , ex_lef, ex_rit, ex_cas
  , hs_lef, hs_rit, hs_cas

  , ex_con, ex_car, ex_cdr
  , hs_con, hs_car, hs_cdr

  , ex_zer, ex_inc, ex_dec

  , ex_add, ex_unnat, ex_nat
  , ex_add_three_four

  , ex_lcons, ex_nil, ex_flop, ex_turn
  ) where

import Ur.Common


-- Utilities ------------------------------------------------------------------

newtype Ex a = Ex a

instance Eq (Ex a) where
  x == y = True

instance Ord (Ex a) where
  compare x y = EQ

instance Show (Ex a) where
  show = const "Ex"


-- Values ----------------------------------------------------------------------

data Lv
    = Un
    | Le Lv
    | Ri Lv
    | Cn Lv Lv
    | At At
    | Fn Lc [Lv]
    | Jt At Lc [Lv] (Ex (Lv -> Lv))
  deriving (Eq, Ord)

instance Show Lv where
    show = \case
        Un     -> "~"
        Le x   -> "L" <> show x
        Ri x   -> "R" <> show x
        Cn x y -> showList (show <$> conList [x] y)
        At a   -> "#" <> show a
        Fn b []-> show (Lam b)
        Fn b e -> mconcat [ "{", show (Lam b)
                          , " : ", intercalate " " (show <$> e)
                          , "}" ]

        Jt n b e _ ->
            "@" <> show n <> "(" <> show (Fn b e) <> ")"
      where
        showList :: [String] -> String
        showList args = "[" <> intercalate " " args <> "]"

        conList acc (Cn x y) = conList (x:acc) y
        conList acc y        = reverse (y:acc)


-- Expressions -----------------------------------------------------------------

{-
    Syntax:
        @       -- Nat
        {a b}   -- a -> b
        {a b c} -- {a {b c}}
        ~       -- ()
        [a b]   -- (a, b)
        [a b c] -- (a, (b, c))
        <a b>   -- Either a b
        <a b c> -- Either a (Either b c)

    Only (Lam, Var, App, Fix, Eva, and Jet) are strictly required.

    - Unit/sum/pair are trivial to implement using closures.
    - We can build the naturals from the above.
    - We can use data jets to make the above approach fast.
    - However, I want the argument to Eval to be "concrete" data.
    - Also, I want the argument (event) and result (effects) of
      the Urbit function to be simple data, not closures.
    - Also, I want the argument (event) and result (effects) of
-}
data Lc
    -- Lambda Calculus
    = Lam Lc
    | Var At
    | App Lc Lc
    | Fix Lc

    -- Eval and Jet Registration
    | Eva Lc
    | Jet At Lc

    -- Unit
    | Unt

    -- Products
    | Con Lc Lc
    | Car Lc
    | Cdr Lc

    -- Co-Products
    | Lef Lc
    | Rit Lc
    | Cas Lc Lc Lc

    -- Natural Numbers
    | Nat At
    | Inc Lc
    | Dec Lc
  deriving (Eq, Ord)

instance Show Lc where
    show = \case
        Lam b     -> "\\" <> show b
        Var v     -> show v
        App a b   -> "(" <> intercalate " " (show <$> appList [b] a) <> ")"

        Nat n     -> "#" <> show n
        Inc x     -> "+" <> show x
        Dec x     -> "-" <> show x

        Unt       -> "~"

        Con h t   -> "[" <> intercalate " " (show <$> conList [h] t) <> "]"
        Car x     -> "h" <> show x
        Cdr x     -> "t" <> show x

        Lef x     -> "L" <> show x
        Rit x     -> "R" <> show x
        Cas x y z -> "<" <> show x <> " " <> show y <> ">" <> show z
        Eva x     -> "!" <> show x

        Jet n x   -> "@" <> show n <> "(" <> show x <> ")"
        Fix x     -> "%" <> show x

      where
        appList acc (App x y) = appList (y:acc) x
        appList acc x         = x:acc

        conList acc (Con h t) = conList (h:acc) t
        conList acc a         = reverse (a:acc)


-- Expression Encoding ---------------------------------------------------------

{-
    Placeholder encoding.

    Should use sums instead of nouns, since this is not typeable.
-}

{-
    Turn an expression into code that generates an AST for that expression.
-}
ec :: Lc -> Lc
ec = \case
    Lam b     -> Con (Nat 00) (ec b)
    Var a     -> Con (Nat 01) (Nat a)
    App x y   -> Con (Nat 02) (Con (ec x) (ec y))
    Nat n     -> Con (Nat 03) (Nat n)
    Inc x     -> Con (Nat 04) (ec x)
    Dec x     -> Con (Nat 05) (ec x)
    Con x y   -> Con (Nat 06) (Con (ec x) (ec y))
    Car x     -> Con (Nat 07) (ec x)
    Cdr x     -> Con (Nat 08) (ec x)
    Lef x     -> Con (Nat 09) (ec x)
    Rit x     -> Con (Nat 10) (ec x)
    Cas l r x -> Con (Nat 11) (Con (ec l) (Con (ec r) (ec x)))
    Unt       -> Con (Nat 12) Unt
    Eva x     -> Con (Nat 13) (ec x)
    Jet nm x  -> Con (Nat 14) (Con (Nat nm) (ec x))
    Fix x     -> Con (Nat 15) (ec x)


{-
    Parse an AST value into an expression.
-}
dc :: Lv -> Lc
dc = \case
    Cn (At 00) b                -> Lam (dc b)

    Cn (At 01) (At v)           -> Var v
    Cn (At 01) _                -> error "bad-var"

    Cn (At 02) (Cn x y)         -> App (dc x) (dc y)
    Cn (At 02) _                -> error "bad-app"

    Cn (At 03) (At n)           -> Nat n
    Cn (At 03) _                -> error "bad-nat"

    Cn (At 04) x               -> Inc (dc x)
    Cn (At 05) x               -> Dec (dc x)

    Cn (At 06) (Cn x y)        -> Con (dc x) (dc y)
    Cn (At 06) _               -> error "bad-con"

    Cn (At 07) x               -> Car (dc x)
    Cn (At 08) x               -> Cdr (dc x)
    Cn (At 09) x               -> Lef (dc x)
    Cn (At 10) x               -> Rit (dc x)
    Cn (At 11) (Cn l (Cn r x)) -> Cas (dc l) (dc r) (dc x)
    Cn (At 11) _               -> error "bad-cas"
    Cn (At 12) Un              -> Unt
    Cn (At 12) _               -> error "bad-unt"
    Cn (At 13) x               -> Eva (dc x)
    Cn (At 14) (Cn (At n) x)   -> Jet n (dc x)
    Cn (At 14) _               -> error "bad-jet"
    Cn (At 15) x               -> Fix (dc x)
    Cn (At _)  _               -> error "bad-cod"
    _                          -> error "bad-exp"


-- Interpreter -----------------------------------------------------------------

lc :: Lc -> Lv
lc = go []
  where
    go :: [Lv] -> Lc -> Lv
    go env = \case
        Nat a     -> At a
        Inc x     -> inc (go env x)
        Dec x     -> dec (go env x)
        Con x y   -> Cn (go env x) (go env y)
        Car x     -> car (go env x)
        Cdr x     -> cdr (go env x)
        Lef x     -> Le (go env x)
        Rit x     -> Ri (go env x)
        Cas l r x -> cas (go env l) (go env r) (go env x)
        Unt       -> Un
        Eva x     -> eva (go env x)
        Jet x y   -> jet x (go env y)
        Lam b     -> Fn b env
        Var v     -> env !! fromIntegral v
        App x y   -> app (go env x) (go env y)
        Fix b     -> app (Fn b env) (go env (Fix b))

    jetMatch :: At -> (Lc, [Lv]) -> Maybe (Lv -> Lv)
    jetMatch nm (b, e) = lookup (nm, Fn b e) jets

    jet nm (Fn b e) | Just f <- jetMatch nm (b,e) =
        Jt nm b e (Ex f)
    jet nm (Fn b e) =
        Jt nm b e $ Ex $ \x -> go (x:e) b
    jet nm _ =
        error "Can only jet functions for now"

    app (Fn b e)           x = go (x:e) b
    app (Jt nm b e (Ex f)) x = f x
    app fn                 x = error "bad-app"

    inc (At n) = At (succ n)
    inc _      = error "bad-inc"

    dec (At 0) = Le Un
    dec (At n) = Ri (At (pred n))
    dec _      = error "bad-dec"

    car (Cn l _) = l
    car _        = error "bad-car"

    cdr (Cn _ r) = r
    cdr _        = error "bad-cdr"

    cas :: Lv -> Lv -> Lv -> Lv
    cas l _ (Le x) = app l x
    cas _ r (Ri x) = app r x
    cas _ _ _      = error "bad-cas"

    eva :: Lv -> Lv
    eva ast = go [] (dc ast)


-- Jets ------------------------------------------------------------------------

addExp :: Lc
addExp = Lam (Lam (Fix bod `App` Var 1 `App` Var 0))
  where
    bod = Lam $ Lam $ Cas lef rit (Dec (Var 1))
    lef = Lam (Var 1) -- return second argument
    rit = Lam (Var 3 `App` Var 0 `App` Inc (Var 1))

addNam :: At
addNam = 765

addImpl :: Lv -> Lv
addImpl x = Jt addNam addExp [x] $ Ex $ \y -> go x y
  where
    go :: Lv -> Lv -> Lv
    go (At x) (At y) = At (x+y)
    go _      _      = error "bad-add"

jets :: Map (At, Lv) (Lv -> Lv)
jets = mapFromList
    [ ((addNam, lc addExp), addImpl)
    ]


-- Examples --------------------------------------------------------------------

ex_id :: Lc
ex_id = Lam (Var 0)

ex_zero :: Lc
ex_zero = Lam (Var 0) `App` Nat 0

ex_cons :: Lc
ex_cons = Lam $ Lam $ Con (Var 1) (Var 0)

ex_zeros = ex_cons `App` ex_zero `App` ex_zero

ex_trip :: Lc
ex_trip = Lam $ Lam $ Lam $ Con (Var 2) $ Con (Var 1) (Var 0)

ex_one_two_three :: Lc
ex_one_two_three = ex_trip `App` Nat 1 `App` Nat 2 `App` Nat 3

ex_jetted_add :: Lc
ex_jetted_add = Jet addNam addExp

ex_eva :: Lc
ex_eva = app (eva bod)
  where
    eva :: Lc -> Lc
    eva = Eva . ec

    app :: Lc -> Lc
    app b = App (App b $ Nat 0) (Nat 1)

    bod :: Lc
    bod = Lam
        $ Lam
        $ Con (Var 1)
        $ Inc (Var 0)


-- Unit, Sum, Product, and Nat in Lambda Calculus ------------------------------

type Uni     = ∀a. a -> a
type Sum a b = ∀c. (a -> c) -> (b -> c) -> c
type Con a b = ∀c. (Sum (a -> c) (b -> c)) -> c

hs_uni :: Uni
hs_uni x = x

hs_lef :: a -> Sum a b
hs_lef x l r = l x

hs_rit :: b -> Sum a b
hs_rit x l r = r x

hs_cas :: (a -> c) -> (b -> c) -> Sum a b -> c
hs_cas l r s = s l r

hs_con :: a -> b -> Con a b
hs_con x y get = get (\l -> l x) (\r -> r y)

hs_car :: Con a b -> a
hs_car x = x (hs_lef id)

hs_cdr :: Con a b -> b
hs_cdr x = x (hs_rit id)

ex_uni :: Lc
ex_uni = ex_id

ex_lef, ex_rit, ex_cas :: Lc
ex_lef = Lam $ Lam $ Lam $ App (Var 1) (Var 2)
ex_rit = Lam $ Lam $ Lam $ App (Var 0) (Var 2)
ex_cas = Lam $ Lam $ Lam $ App (App (Var 0) (Var 2)) (Var 1)

ex_con, ex_car, ex_cdr :: Lc
ex_con = Lam $ Lam $ Lam (App (App (Var 0) getCar) getCdr)
  where
    getCar = Lam $ App (Var 0) (Var 3)
    getCdr = Lam $ App (Var 0) (Var 2)

ex_car = Lam $ App (Var 0) $ App ex_lef ex_id

ex_cdr = Lam $ App (Var 0) $ App ex_rit ex_id

ex_zer, ex_inc, ex_dec :: Lc
ex_zer = App ex_lef ex_id
ex_inc = ex_rit
ex_dec = ex_id

ex_unnat :: Lc
ex_unnat = Fix $ Lam $ ex_cas `App` (Lam $ Nat 0)
                              `App` (Lam $ Inc (Var 2 `App` Var 0))
                              `App` (Var 0)

ex_nat :: Lc
ex_nat = Fix $ Lam $ Cas (Lam $ ex_zer)
                         (Lam $ ex_inc `App` (Var 2 `App` Var 0))
                         (Dec $ Var 0)

ex_add :: Lc
ex_add = Fix bod -- Lam (Lam (Fix bod `App` Var 1 `App` Var 0))
  where
    bod = Lam $ Lam $ ex_cas `App` lef
                             `App` rit
                             `App` (ex_dec `App` Var 1)
    lef = Lam $ Var 1
    rit = Lam $ Var 3 `App` Var 0 `App` (ex_inc `App` Var 1)

ex_add_three_four :: Lc
ex_add_three_four =
    App ex_unnat $ ex_add `App` (ex_nat `App` Nat 3)
                          `App` (ex_nat `App` Nat 4)


-- Lists -----------------------------------------------------------------------

ex_lcons :: Lc -> Lc -> Lc
ex_lcons x y = Rit (Con x y)

ex_nil :: Lc
ex_nil = Lef Unt

{-
    flop = go []
      where
        go acc = \case
            []   -> acc
            x:xs -> go (x:acc) xs
-}
ex_flop :: Lc
ex_flop = Jet 98 (go `App` ex_nil)
  where
    go = Fix $ Lam $ Lam $ Cas (Lam $ Var 2)
                               (Lam $ Var 3 `App` ex_lcons (Car $ Var 0) (Var 2)
                                            `App` (Cdr $ Var 0))
                               (Var 0)

ex_turn :: Lc
ex_turn = App bod ex_flop
  where
    bod = Lam $ Jet 99 $ Lam (go `App` ex_nil `App` Car (Var 0))
    go = Fix $ Lam $ Lam $ Cas (Lam (Var 5 `App` Var 2))
                               (Lam (Var 3 `App` (ex_lcons
                                                  (Car (Var 4)
                                                     `App`
                                                   Car (Var 0))
                                                  (Var 3))
                                           `App` (Cdr (Var 0))))
                               (Var 0)
