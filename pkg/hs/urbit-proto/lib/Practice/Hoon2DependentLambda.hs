module Practice.Hoon2DependentLambda where

import ClassyPrelude hiding (init, last)
import Prelude (init, last)


import Bound

import Practice.DependentLambda
import Practice.HoonCommon
import Practice.HoonSyntax

type Desugar = Either Text

open :: Hoon -> Desugar (Code Term)
open = \case
  Wung w@(last -> Ally x)  -> pure $ Wing (init w) (Look x)
  Wung _ -> Left "unsupported wing"
  Adam _ a au -> pure $ Atom a au
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
    let pats = [gale $ Atom a au | (a, au, _) <- cs]
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
  Brts (Rash x) h -> Lamb <$> abstract1 x <$> open h
  Brts{} -> Left "unsupported pattern in |="
  --
  Clcb h j -> Cons <$> open j <*> open h
  Clkt h j k l -> Cons <$> open h
                       <*> (Cons <$> open j
                                 <*> (Cons <$> open k
                                           <*> open l))
  Clhp h j -> Cons <$> open h <*> open j
  Clls h j k -> Cons <$> open h <*> (Cons <$> open j <*> open k)
  Clsg hs -> foldr Cons (Atom 0 "n") <$> traverse open hs
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
  Tsfs (Rash x) h j -> Bind <$> open h <*> (abstract1 x <$> open j)
  Tsfs{} -> Left "unsupported pattern in =/"
  Tsmc (Rash x) j h -> Bind <$> open h <*> (abstract1 x <$> open j)
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
      Just f  -> make <$> go h <*> (abstract1 f <$> boil j js fac go make)
