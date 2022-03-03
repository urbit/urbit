module Practice.Hoon2DependentLambda where

import ClassyPrelude hiding (init, last)
import Prelude (init, last)


import Bound

import Practice.DependentLambda
import Practice.HoonCommon
import Practice.HoonSyntax

type Desugar = Either Text

open :: Hoon -> Desugar (Code Term)
open = undefined{-\case
  Wung w@(last -> Ally x)  -> pure $ Wing (init w) (Look x)
  Wung _ -> Left "unsupported wing"
  Adam g a au -> pure $ Atom a g au
  --
  Bass Non -> Left "unsupported *"
  Bass Cel -> Left "unsupported ^"
  Bass Flg -> pure $ Fork (setFromList [0,1]) "f"
  Bass Nul -> pure $ Fork (setFromList [0]) "n"
  Bass Vod -> Left "unsupported !"
  Bass (Fok as au) -> pure $ Fork (setFromList as) au
  Bass (Aur au) -> pure $ Aura au
  Bass Typ -> pure Type
  Bccb h -> Left "unsupported _"
  Bccl s ss -> boil s ss unmask open Cell
  Bccn [] -> Left "unsupported empty $% (XX what should it be?)"
  Bccn cs -> do
    let fork = Fork (setFromList [a | (a, _, _) <- cs]) "" -- FIXME aura
    let pats = [gale $ Atom a Rock au | (a, au, _) <- cs]
    let beds = [s | (_, _, s) <- cs]
    bods <- traverse (fmap gale . open) beds
    pure $ Cell fork (Scope $ Case (Look $ B ()) pats bods)
   where
    gale :: Code a -> Scope Int Code (Var () (Code a))
    gale = Scope . pure . F . pure . F
  Bcdt s m -> cook s m unmask open (flip Gold)
  Bchp s t -> bake s t unmask open Gate
  Bckt{} -> Left "unsupported $^"
  Bcts (Rash x) h -> Mask x <$> open h
  Bcts{} -> Left "unsupported pattern in $="
  Bcpt{} -> Left "unsupported $@"
  Bcwt m -> Lead <$> traverse open m
  --
  Brcn{} -> Left "unsupported naked |%; use with => for now"
  Brts (Rash x) h -> Lamb x <$> abstract1 x <$> open h
  Brts{} -> Left "unsupported pattern in |="
  --
  Clcb h j -> Cons <$> open j <*> open h
  Clkt h j k l -> Cons <$> open h
                       <*> (Cons <$> open j
                                 <*> (Cons <$> open k
                                           <*> open l))
  Clhp h j -> Cons <$> open h <*> open j
  Clls h j k -> Cons <$> open h <*> (Cons <$> open j <*> open k)
  Clsg hs -> foldr Cons (Atom 0 Rock "n") <$> traverse open hs
  Cltr [] -> Left "empty :*"
  Cltr [h] -> open h
  Cltr (h:hs) -> Cons <$> open h <*> open (Cltr hs)
  --
  Cndt h j -> Slam <$> open j <*> open h
  Cnhp h j -> Slam <$> open h <*> open j
  Cncl h hs -> Slam <$> open h <*> open (Cltr hs)
  Cnkt h j k l -> Slam <$> open h
                       <*> (Slam <$> open j
                                 <*> (Slam <$> open k
                                           <*> open l))
  Cnls h j k -> Slam <$> open h <*> (Slam <$> open j <*> open k)
  Cnts{} -> Left "unsupported %="
  --
  Dtkt{} -> Left "unsupported .^"
  Dtls h -> Plus <$> open h
  Dttr{} -> Left "unsupported .*"
  Dtts h j -> Equl <$> open h <*> open j
  Dtwt{} -> Left "unsupported .?"
  --
  Ktls{} -> Left "unsupported ^+"
  Kthp s h -> do typ <- open s; cod <- open h; pure Nest {..}
  Ktfs h s -> do typ <- open s; cod <- open h; pure Nest {..}
  Ktzp s h -> do typ <- open s; cod <- open h; pure Cast {..}
  Ktwt h -> Left "unsupported ^? lead cast"  -- XX fixme soon
  Ktts (Rash x) h -> Name x <$> open h
  Ktts{} -> Left "unsupported pattern in ^= face"
  Ktcl{} -> Left "unsupported ^: mold"
  --
  Sgfs{} -> Left "unsupported ~/"
  --
  Tsfs (Rash x) h j -> Bind x <$> open h <*> (abstract1 x <$> open j)
  Tsfs{} -> Left "unsupported pattern in =/"
  Tsmc (Rash x) j h -> Bind x <$> open h <*> (abstract1 x <$> open j)
  Tsmc{} -> Left "unsupported pattern in =;"
  Tsdt{} -> Left "unsupported =."
  Tswt{} -> Left "unsupported =?"
  -- Please excuse this temporary hack
  Tsgl (Brcn m) h -> cook h m unname open (flip Core)
  Tsgl{} -> Left "unsupported =<"
  Tsgr h (Brcn m) -> cook h m unname open (flip Core)
  Tsgr{} -> Left "unsupported =>"
  Tshp{} -> Left "unsupported =-"
  Tskt{} -> Left "unsupported =^"
  Tsls{} -> Left "unsupported =+"
  Tssg{} -> Left "unsupported =~"
  --
  Wtbr{} -> Left "unsupported ?|"
  Wthp{} -> Left "unsupported ?-"
  Wtcl{} -> Left "unsupported ?:"
  Wtdt{} -> Left "unsupported ?."
  Wtkt{} -> Left "unsupported ?^"
  Wtgl{} -> Left "unsupported ?>"
  Wtgr{} -> Left "unsupported ?<"
  Wtpm{} -> Left "unsupported ?@"
  Wtpt{} -> Left "unsupported ?@"
  Wtts{} -> Left "unsupported ?="
  Wtzp{} -> Left "unsupported ?!"
  --
  Zpzp -> Left "unsupported !!"

 where
  unmask :: Hoon -> Maybe Term
  unmask = \case
    Bcts (Rash x) _ -> Just x
    _ -> Nothing

  unname :: Hoon -> Maybe Term
  unname = \case
    Ktts (Rash x) _ -> Just x
    _ -> Nothing

  bake :: (Applicative f, Eq a)
       => Hoon
       -> Hoon
       -> (Hoon -> Maybe a)
       -> (Hoon -> f (Code a))
       -> (Code a -> Scope () Code a -> Code a)
       -> f (Code a)
  bake h j fac go make = case fac h of
    Nothing -> make <$> go h <*> (blow <$> go j)
    Just f  -> make <$> go h <*> (abstract1 f <$> go j)

  cook :: (Applicative f, Traversable c, Eq a)
       => Hoon
       -> c Hoon
       -> (Hoon -> Maybe a)
       -> (Hoon -> f (Code a))
       -> (Code a -> c (Scope () Code a) -> Code a)
       -> f (Code a)
  cook h hs fac go make = case fac h of
    Nothing -> make <$> go h <*> traverse (fmap blow . go) hs
    Just f  -> make <$> go h <*> traverse (fmap (abstract1 f) . go) hs

  boil :: (Applicative f, Eq a)
       => Hoon
       -> [Hoon]
       -> (Hoon -> Maybe a)
       -> (Hoon -> f (Code a))
       -> (Code a -> Scope () Code a -> Code a)
       -> f (Code a)
  boil h hs fac go make = case hs of
    []   -> go h
    j:js -> case fac h of
      Nothing -> make <$> go h <*> (blow <$> boil j js fac go make)
      Just f  -> make <$> go h <*> (abstract1 f <$> boil j js fac go make)-}

shut :: (a -> Term) -> Code a -> Hoon
shut e = undefined{-\case
  Look a -> Wung [Ally $ e a]
  --
  Atom a g au -> Adam g a au
  Cons c d -> case shut e d of
    Clhp h j -> Clls (shut e c) h j
    Clls h j k -> Clkt (shut e c) h j k
    Clkt h j k l -> Cltr [shut e c, h, j, k, l]
    Cltr hs -> Cltr (shut e c : hs)
    h -> Clhp (shut e c) h
  Lamb t sc -> Brts (Wung [Ally t]) $ shut f $ fromScope sc
    where f = \case B () -> t; F x -> e x
  Core b p -> Tsgr (shut e p) (Brcn $ fmap (shut (unname e p) . fromScope) b)
  Name t c -> Ktts (Wung [Ally t]) (shut e c)
  --
  Plus c -> Dtls (shut e c)
  Slam c d -> case shut e d of
    Clhp h j -> Cnls (shut e c) h j
    Clls h j k -> Cnkt (shut e c) h j k
    Clkt h j k l -> Cncl (shut e c) [h, j, k, l]
    Cltr hs -> Cncl (shut e c) hs
    h -> Cnhp (shut e c) h
  Wing w (Look x) -> Wung (w ++ [Ally $ e x])
  Wing w c -> Tsgl (Wung w) (shut e c)
  Equl c d -> Dtts (shut e c) (shut e d)
  --
  Aura au -> Bass (Aur au)
  Fork as "f" | as == setFromList [0, 1] -> Bass Flg
  Fork as au -> Bass (Fok (toList as) au)
  Cell c d -> case shut (unmask e c) $ fromScope d of
    Bccl s ss -> Bccl (shut e c) (s:ss)
    s -> Bccl (shut e c) [s]
  Gate c d -> Bchp (shut e c) (shut (unmask e c) $ fromScope d)
  Gold cs c -> Bcdt (shut e c) (shut (unmask e c) . fromScope <$> cs)
  Lead cs -> Bcwt (shut e <$> cs)
  Mask t c -> Bcts (Wung [Ally t]) (shut e c)
  Type -> Bass Typ
  --
  Bind t c d -> Tsfs (Wung [Ally t]) (shut e c) (shut f $ fromScope d)
   where f = \case B () -> t; F x -> e x
  Case c ss ds -> error "open: case"
  Nest{cod, typ} -> Kthp (shut e typ) (shut e cod)  -- XX should print as / wide
  Cast{cod, typ} -> Ktzp (shut e typ) (shut e cod)

 where
  unname e = \case
    Name t _ -> \case B () -> t;     F x -> e x
    _        -> \case B () -> "???"; F x -> e x

  unmask e = \case
    Mask t _ -> \case B () -> t;     F x -> e x
    _        -> \case B () -> "???"; F x -> e x-}
