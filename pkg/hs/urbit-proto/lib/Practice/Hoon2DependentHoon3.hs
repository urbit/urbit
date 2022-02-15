module Practice.Hoon2DependentHoon3 where

import ClassyPrelude

import Practice.DependentHoon3
import Practice.HoonCommon
import Practice.HoonSyntax

type Desugar = Either Text

open :: Hoon -> Desugar (Code Wing)
open = \case
  Wung w -> pure $ Wing w
  Wild -> Left "open-skin: unexpected '_' in non-skin position"
  Adam g a au -> pure $ Atom a g au
  --
  Bass Non -> pure Noun
  Bass Cel -> pure $ Cell Noun Noun
  Bass Flg -> pure $ Fork (setFromList [0,1]) "f"
  Bass Nul -> pure $ Fork (setFromList [0]) "n"
  Bass Vod -> pure Void
  Bass (Fok as au) -> pure $ Fork (setFromList as) au
  Bass (Aur au) -> pure $ Aura au
  Bass Typ -> pure Type
  Bccb h -> Left "unsupported _"
  Bccl s [] -> open s
  Bccl s (s':ss) -> Cell <$> open s <*> open (Bccl s' ss)
  Bccn [] -> pure Void
  Bccn cs -> do
    let fork = Bass $ Fok [a | (a, _, _) <- cs] "" -- FIXME aura
    let pats = [(Adam Rock a au, s) | (a, au, s) <- cs]
    open $ Bccl fork [Wthp [Axis 3] pats]
  Bcdt s m -> Left "unsupported $." --cook s m unmask open (flip Gold)
  Bchp s t -> Gate <$> open s <*> open t
  Bckt{} -> Left "unsupported $^"
  Bcts (Rash x) h -> Mask x <$> open h
  Bcts{} -> Left "unsupported pattern in $="
  Bcpt{} -> Left "unsupported $@"
  Bcwt m -> Left "unsupported $?" --Lead <$> traverse open m
  --
  Brcn{} -> Left "unsupported |%"
  Brts s h -> Lamb <$> flay s <*> open h
  --
  Clcb h j -> Cons <$> open j <*> open h
  Clkt h j k l -> Cons <$> open h
                       <*> (Cons <$> open j
                                 <*> (Cons <$> open k
                                           <*> open l))
  Clhp h j -> Cons <$> open h <*> open j
  Clls h j k -> Cons <$> open h <*> (Cons <$> open j <*> open k)
  Clsg [] -> pure $ Atom 0 Rock "n"
  Clsg (h:hs) -> Cons <$> open h <*> open (Clsg hs)
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
  Dtts h j -> Left "unsupported .=" --Equl <$> open h <*> open j
  Dtwt{} -> Left "unsupported .?"
  --
  Ktls{} -> Left "unsupported ^+"
  Kthp s h -> do typ <- open s; cod <- open h; pure Nest {..}
  Ktfs h s -> do typ <- open s; cod <- open h; pure Nest {..}
  Ktzp s h -> Left "unsupported cast" -- do typ <- open s; cod <- open h; pure Cast {..}
  Ktwt h -> Left "unsupported ^? lead cast"  -- XX fixme soon
  Ktts (Rash x) h -> Name x <$> open h
  Ktts{} -> Left "unsupported pattern in ^= face"
  Ktcl{} -> Left "unsupported ^: mold"
  --
  Sgfs{} -> Left "unsupported ~/"
  --
  Tsfs (Wung [Ally f]) h j -> Push <$> (Name f <$> open h) <*> open j
  Tsfs{} -> Left "unsupported =/"
  -- Tsmc (Rash x) j h -> Bind x <$> open h <*> (abstract1 x <$> open j)
  Tsmc{} -> Left "unsupported =;"
  Tsdt{} -> Left "unsupported =."
  Tswt{} -> Left "unsupported =?"
  -- Please excuse this temporary hack
  --Tsgl (Brcn m) h -> cook h m unname open (flip Core)
  Tsgl h j -> flip With <$> open h <*> open j
  --Tsgr h (Brcn m) -> cook h m unname open (flip Core)
  Tsgr h j -> With <$> open h <*> open j
  Tshp h j -> Push <$> open j <*> open h
  Tskt{} -> Left "unsupported =^"
  Tsls h j -> Push <$> open h <*> open j
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
  flay :: Hoon -> Desugar (Pelt Wing)
  flay = \case
    Wild -> pure Punt
    Wung [Ally f] -> pure $ Peer f
    Adam g a au -> pure $ Part $ Atom a g au
    --
    Clcb h j -> Pair <$> flay j <*> flay h
    Clkt h j k l -> Pair <$> flay h
                         <*> (Pair <$> flay j
                                   <*> (Pair <$> flay k
                                             <*> flay l))
    Clhp h j -> Pair <$> flay h <*> flay j
    Clls h j k -> Pair <$> flay h <*> (Pair <$> flay j <*> flay k)
    Clsg [] -> pure $ Part $ Atom 0 Rock "n"
    Clsg (h:hs) -> Pair <$> flay h <*> flay (Clsg hs)
    Cltr [] -> Left "empty :*"
    Cltr [h] -> flay h
    Cltr (h:hs) -> Pair <$> flay h <*> flay (Cltr hs)
    --
    _ -> Left "flay-meat: expression in pattern context"

shut :: Code Wing -> Hoon
shut = \case
  Wing w -> Wung w
  --
  Atom a g au -> Adam g a au
  Cons c d -> case shut d of
    Clhp h j -> Clls (shut c) h j
    Clls h j k -> Clkt (shut c) h j k
    Clkt h j k l -> Cltr [shut c, h, j, k, l]
    Cltr hs -> Cltr (shut c : hs)
    h -> Clhp (shut c) h
  Lamb p c -> Brts (flap p) (shut c)
  --Core b p -> Tsgr (shut p) (Brcn $ fmap (shut (unname e p) . fromScope) b)
  Name t c -> Ktts (Wung [Ally t]) (shut c)
  --
  Plus c -> Dtls (shut c)
  Slam c d -> case shut d of
    Clhp h j -> Cnls (shut c) h j
    Clls h j k -> Cnkt (shut c) h j k
    Clkt h j k l -> Cncl (shut c) [h, j, k, l]
    Cltr hs -> Cncl (shut c) hs
    h -> Cnhp (shut c) h
  --Equl c d -> Dtts (shut c) (shut d)
  --
  Aura au -> Bass (Aur au)
  Fork as "f" | as == setFromList [0, 1] -> Bass Flg
  Fork as au -> Bass (Fok (toList as) au)
  Cell Noun Noun -> Bass Cel
  Cell c d -> case shut d of
    Bccl s ss -> Bccl (shut c) (s:ss)
    s -> Bccl (shut c) [s]
  Gate c d -> Bchp (shut c) (shut d)
  Mask t c -> Bcts (Wung [Ally t]) (shut c)
  Noun -> Bass Non
  Void -> Bass Vod
  Type -> Bass Typ
  --
  With c d -> Tsgr (shut c) (shut d)
  -- TODO push with face becomes tisfas AFTER pelt/face repr
  Push c d -> Tsls (shut c) (shut d)
  --Case c ss ds -> error "open: case"
  Nest{cod, typ} -> Kthp (shut typ) (shut cod)  -- XX should print as / wide
  --Cast{cod, typ} -> Ktzp (shut typ) (shut cod)


flap :: Pelt Wing -> Hoon
flap = \case
  Punt -> Wild
  Peer f -> Wung [Ally f]
  Part c -> case c of
    Atom a g au -> Adam g a au
    _ -> error "flap-part: no syntax yet for complex equality patterns" -- XX
  Pair p q -> case flap q of
    Clhp h j -> Clls (flap p) h j
    Clls h j k -> Clkt (flap p) h j k
    Clkt h j k l -> Cltr [flap p, h, j, k, l]
    Cltr hs -> Cltr (flap p : hs)
    h -> Clhp (flap p) h
  Pest p c -> Kthp (flap p) (shut c)

-- | Hack to make Bases pretty printable somehow
lock :: Show a => Base a -> Hoon
lock = \case
  Stop' r -> Wung [Ally $ tshow r]
  Fore' x -> Wung [Ally $ tshow $ Old @Text x]
  --
  Atom' a g au -> Adam g a au
  Cons' x y -> case lock y of
    Clhp h j -> Clls (lock x) h j
    Clls h j k -> Clkt (lock x) h j k
    Clkt h j k l -> Cltr [lock x, h, j, k, l]
    Cltr hs -> Cltr (lock x : hs)
    h -> Cnhp (lock x) h
  Lamb' x c -> Brts (lock x) (shut $ fmap hack c)
  Name' f x -> Ktts (Wung [Ally f]) (lock x)
  --
  Plus' x -> Dtls (lock x)
  Slam' x y ->  case lock y of
    Clhp h j -> Cnls (lock x) h j
    Clls h j k -> Cnkt (lock x) h j k
    Clkt h j k l -> Cncl (lock x) [h, j, k, l]
    Cltr hs -> Cncl (lock x) hs
    h -> Cnhp (lock x) h
  Look' x st -> Tsgl (Wung $ doze st) (lock x)
  --
  Aura' au -> Bass (Aur au)
  Fork' as au -> Bass (Fok (toList as) au)
  Cell' t s c -> Bccl (lock t) [Brts (lock s) (shut $ fmap hack c)]
  Gate' t s c -> Bchp (lock t) $ Brts (lock s) (shut $ fmap hack c)
  Mask' f x -> Bcts (Wung [Ally f]) (lock x)
  Noun' -> Bass Non
  Void' -> Bass Vod
  Type' -> Bass Typ
 where
  hack = singleton . Ally . tshow
