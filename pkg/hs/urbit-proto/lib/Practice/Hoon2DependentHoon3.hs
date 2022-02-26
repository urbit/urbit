module Practice.Hoon2DependentHoon3 where

import ClassyPrelude

import Practice.DependentHoon3
import Practice.HoonCommon
import Practice.HoonSyntax

type Desugar = Either Text

open :: Hoon -> Desugar Soft
open = \case
  Wung w -> pure $ Wng w
  Wild -> Left "open-skin: unexpected '_' in non-skin position"
  Adam g a au -> pure $ Atm a g au
  --
  Bass b -> pure $ Bas b
  Bccb h -> Left "unsupported _"
  Bccl s [] -> open s
  Bccl s (s':ss) -> Cll <$> open s <*> open (Bccl s' ss)
  Bccn [] -> pure $ Bas Vod
  Bccn cs -> do
    let fork = Bass $ Fok [a | (a, _, _) <- cs] "" -- FIXME aura
    let pats = [(Adam Rock a au, s) | (a, au, s) <- cs]
    open $ Bccl fork [Wthp [Axis 3] pats]
  Bcdt s m -> Left "unsupported $." --cook s m unmask open (flip Gold)
  Bchp s t -> Gat <$> open s <*> open t
  Bckt{} -> Left "unsupported $^"
  Bcts s h -> open $ Ktts s h  -- TODO remove this later and fix tests
  Bcpt{} -> Left "unsupported $@"
  Bcwt m -> Left "unsupported $?" --Lead <$> traverse open m
  --
  Brcn{} -> Left "unsupported |%"
  Brts s h -> Lam <$> flay s <*> open h
  --
  Clcb h j -> Cns <$> open j <*> open h
  Clkt h j k l -> Cns <$> open h
                       <*> (Cns <$> open j
                                 <*> (Cns <$> open k
                                           <*> open l))
  Clhp h j -> Cns <$> open h <*> open j
  Clls h j k -> Cns <$> open h <*> (Cns <$> open j <*> open k)
  Clsg [] -> pure $ Atm 0 Rock "n"
  Clsg (h:hs) -> Cns <$> open h <*> open (Clsg hs)
  Cltr [] -> Left "empty :*"
  Cltr [h] -> open h
  Cltr (h:hs) -> Cns <$> open h <*> open (Cltr hs)
  --
  Cndt h j -> Sla <$> open j <*> open h
  Cnhp h j -> Sla <$> open h <*> open j
  Cncl h hs -> Sla <$> open h <*> open (Cltr hs)
  Cnkt h j k l -> Sla <$> open h
                       <*> (Sla <$> open j
                                 <*> (Sla <$> open k
                                           <*> open l))
  Cnls h j k -> Sla <$> open h <*> (Sla <$> open j <*> open k)
  Cnts{} -> Left "unsupported %="
  --
  Dtkt{} -> Left "unsupported .^"
  Dtls h -> Plu <$> open h
  Dttr{} -> Left "unsupported .*"
  Dtts h j -> Equ <$> open h <*> open j
  Dtwt h -> open $ Wtts (Clhp Wild Wild) h  -- TODO gets converted back later
  --
  Ktls{} -> Left "unsupported ^+"
  Kthp s h -> do typ <- open s; sof <- open h; pure Net {typ, sof}
  Ktfs h s -> do typ <- open s; sof <- open h; pure Net {typ, sof}
  Ktzp s h -> do typ <- open s; sof <- open h; pure Cat {typ, sof}
  Ktwt h -> Left "unsupported ^? lead cast"  -- XX fixme soon
  Ktts s h -> Fac <$> flay s <*> open h
  Ktcl{} -> Left "unsupported ^: mold"
  --
  Sgfs{} -> Left "unsupported ~/"
  --
  Tsfs s h j -> open $ Tsls (Ktts s h) j
  Tsmc s h j -> open $ Tsfs s j h
  Tsdt{} -> Left "unsupported =."
  Tswt{} -> Left "unsupported =?"
  --Tsgl (Brcn m) h -> cook h m unname open (flip Core)
  Tsgl h j -> open $ Tsgr j h
  --Tsgr h (Brcn m) -> cook h m unname open (flip Core)
  Tsgr h j -> Wit <$> open h <*> open j
  Tshp h j -> open $ Tsls j h
  Tskt{} -> Left "unsupported =^"
  Tsls h j -> Pus <$> open h <*> open j
  Tssg{} -> Left "unsupported =~"
  --
  Wtbr{} -> Left "unsupported ?|"
  Wthp{} -> Left "unsupported ?-"
  Wtcl{} -> Left "unsupported ?:"
  Wtdt{} -> Left "unsupported ?."
  Wtkt{} -> Left "unsupported ?^"
  Wtgl{} -> Left "unsupported ?>"
  Wtgr{} -> Left "unsupported ?<"
  Wtpm{} -> Left "unsupported ?&"
  Wtpt{} -> Left "unsupported ?@"
  Wtts{} -> Left "unsupported ?="
  Wtzp{} -> Left "unsupported ?!"
  --
  Zpzp -> Left "unsupported !!"

flay :: Hoon -> Desugar Pelt
flay = \case
  Wild -> pure Punt
  Wung [Ally f] -> pure $ Peer f
  Adam g a au -> pure $ Part $ Atm a g au
  --
  Clcb h j -> Pair <$> flay j <*> flay h
  Clkt h j k l -> Pair <$> flay h
                       <*> (Pair <$> flay j
                                 <*> (Pair <$> flay k
                                           <*> flay l))
  Clhp h j -> Pair <$> flay h <*> flay j
  Clls h j k -> Pair <$> flay h <*> (Pair <$> flay j <*> flay k)
  Clsg [] -> pure $ Part $ Atm 0 Rock "n"
  Clsg (h:hs) -> Pair <$> flay h <*> flay (Clsg hs)
  Cltr [] -> Left "empty :*"
  Cltr [h] -> flay h
  Cltr (h:hs) -> Pair <$> flay h <*> flay (Cltr hs)
  --
  Kthp h j -> Pest <$> flay j <*> open h
  Ktfs h j -> Pest <$> flay h <*> open j
  --
  _ -> Left "flay-meat: expression in pattern context"

shut :: Soft -> Hoon
shut = \case
  Wng w -> Wung w
  --
  Atm a g au -> Adam g a au
  Cns c d -> case shut d of
    Clhp h j -> Clls (shut c) h j
    Clls h j k -> Clkt (shut c) h j k
    Clkt h j k l -> Cltr [shut c, h, j, k, l]
    Cltr hs -> Cltr (shut c : hs)
    h -> Clhp (shut c) h
  Lam p c -> Brts (flap p) (shut c)
  Fac p c -> Ktts (flap p) (shut c)
  --Core b p -> Tsgr (shut p) (Brcn $ fmap (shut (unname e p) . fromScope) b)
  --
  Plu c -> Dtls (shut c)
  Sla c d -> case shut d of
    Clhp h j -> Cnls (shut c) h j
    Clls h j k -> Cnkt (shut c) h j k
    Clkt h j k l -> Cncl (shut c) [h, j, k, l]
    Cltr hs -> Cncl (shut c) hs
    h -> Cnhp (shut c) h
  Equ c d -> Dtts (shut c) (shut d)
  --
  Bas b -> Bass b
  Cll c d -> case shut d of
    Bccl s ss -> Bccl (shut c) (s:ss)
    s -> Bccl (shut c) [s]
  Gat c d -> Bchp (shut c) (shut d)
  --
  Wit c d -> Tsgr (shut c) (shut d)
  Pus c d -> Tsls (shut c) (shut d)
  --Case c ss ds -> error "open: case"
  Net{sof, typ} -> Kthp (shut typ) (shut sof)  -- XX should print as / wide
  Cat{sof, typ} -> Ktzp (shut typ) (shut sof)

flap :: Pelt -> Hoon
flap = \case
  Punt -> Wild
  Peer f -> Wung [Ally f]
  Part c -> case c of
    Atm a g au -> Adam g a au
    _ -> error "flap-part: no syntax yet for complex equality patterns" -- XX
  Pair p q -> case flap q of
    Clhp h j -> Clls (flap p) h j
    Clls h j k -> Clkt (flap p) h j k
    Clkt h j k l -> Cltr [flap p, h, j, k, l]
    Cltr hs -> Cltr (flap p : hs)
    h -> Clhp (flap p) h
  Pons p q -> Ktts (flap p) (flap q)
  Pest p c -> Ktfs (flap p) (shut c)

-- | Hack to make Bases pretty printable somehow
lock :: Show a => Base a -> Hoon
lock = \case
  Rump' r -> Wung [Ally $ tshow r]
  Fore' x -> Wung [Ally $ tshow $ Old @Text x]
  --
  Atom' a g au -> Adam g a au
  Cons' x y -> case lock y of
    Clhp h j -> Clls (lock x) h j
    Clls h j k -> Clkt (lock x) h j k
    Clkt h j k l -> Cltr [lock x, h, j, k, l]
    Cltr hs -> Cltr (lock x : hs)
    h -> Clhp (lock x) h
  Lamb' x c -> Tsgr (lock x) $ Brts Wild (shut . rest $ c)
  --
  Plus' x -> Dtls (lock x)
  Slam' x y ->  case lock y of
    Clhp h j -> Cnls (lock x) h j
    Clls h j k -> Cnkt (lock x) h j k
    Clkt h j k l -> Cncl (lock x) [h, j, k, l]
    Cltr hs -> Cncl (lock x) hs
    h -> Cnhp (lock x) h
  Look' x (Leg a) -> Tsgl (Wung [Axis a]) (lock x)
  --
  Aura' au -> Bass (Aur au)
  Fork' as au -> Bass (Fok (toList as) au)
  Cell' t s c -> Tsgr (lock s) $ Bccl (lock t) case shut . rest $ c of
    Bccl h hs -> h:hs
    h -> [h]
  Gate' t s c -> Tsgr (lock s) $ Bchp (lock t) (shut . rest $ fmap hack c)
  --Mask' f x -> Ktts (Wung [Ally f]) (lock x)
  Face' (Mask m) x -> Ktts (Wung [Ally m]) (lock x)
  Face' (Link ls) x -> Ktts Wild (lock x)  -- FIXME ?
  Noun' -> Bass Non
  Void' -> Bass Vod
  Type' -> Bass Typ
 where
  hack :: Show a => a -> Wing
  hack = singleton . Ally . tshow

