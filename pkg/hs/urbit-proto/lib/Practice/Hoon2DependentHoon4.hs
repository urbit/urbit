module Practice.Hoon2DependentHoon4 where

import ClassyPrelude

import Practice.DependentHoon4
import Practice.HoonCommon hiding (Bass(..))
import qualified Practice.HoonCommon as HC
import Practice.HoonSyntax

type Desugar = Either Text

open :: Hoon -> Desugar Soft
open = \case
  Wung w -> pure $ Wng w []
  Wild -> Left "open-skin: unexpected '_' in non-skin position"
  Adam g a au -> pure $ Atm a g au
  --
  Bass HC.Non -> pure $ Non
  Bass HC.Cel -> pure $ Cel Non Non
  Bass HC.Flg -> pure $ Aur "f" (Fork $ setFromList [0, 1])
  Bass HC.Nul -> pure $ Aur "n" (Fork $ setFromList [0])
  -- XX Aura
  Bass (HC.Fok as au) -> pure $ Aur au (Fork $ setFromList as)
  Bass HC.Vod -> pure $ Vod
  Bass (HC.Aur au) -> pure $ Aur au Bowl
  Bass HC.Typ -> pure $ Typ
  Bcbr t u vs -> Cor <$> open t <*> open u <*> traverse open vs
  Bccb h -> Left "unsupported _"
  Bccl s [] -> open s
  Bccl s (s':ss) -> Ral <$> open s <*> open (Bccl s' ss)
  Bccn [] -> pure $ Vod
  Bccn cs -> do
    let fork = Bass $ HC.Fok [a | (a, _, _) <- cs] "" -- FIXME aura
    let pats = [(Adam Rock a au, s) | (a, au, s) <- cs]
    open $ Bccl fork [Kthp (Bass HC.Typ) $ Wthp [Axis 2] pats]
  Bcgr t ss -> Left "unsupported $>" --Fok <$> open t <*> traverse flay ss
  Bcgl{} -> Left "unsupported $<"
  Bchp s t -> do
    s <- open s
    t <- open t
    pure $
      Sal [Axis 7] $
        Cor
          (Cel (Mot s Non) Non)
          (Cel s Non)
          (mapFromList [("", t)])
  Bckt{} -> Left "unsupported $^"
  Bcmc s t -> Mot <$> open s <*> open t
  Bcts h s -> Sin <$> open h <*> open s
  Bcpt{} -> Left "unsupported $@"
  Bcwt w h -> Sal w <$> open h
  --
  Brcn hs -> Cru <$> traverse open hs
  Brts t h -> do
    t <- open t
    h <- open h
    pure $
      Mut t (Net {sof = Atm 0 Sand "", typ = Non}) $
        Cru (mapFromList [("", h)])
  --
  Clcb h j -> Cel <$> open j <*> open h
  Clkt h j k l -> Cel <$> open h
                       <*> (Cel <$> open j
                                 <*> (Cel <$> open k
                                           <*> open l))
  Clhp h j -> Cel <$> open h <*> open j
  Clls h j k -> Cel <$> open h <*> (Cel <$> open j <*> open k)
  Clsg [] -> pure $ Atm 0 Rock "n"
  Clsg (h:hs) -> Cel <$> open h <*> open (Clsg hs)
  Cltr [] -> Left "empty :*"
  Cltr [h] -> open h
  Cltr (h:hs) -> Cel <$> open h <*> open (Cltr hs)
  --
  Cndt h j -> open $ Cnhp j h
  -- XX are there other cases in which we can elim =+?
  {-
  -- Not ok here because it might be an arm wing
  -- needs to move to next pass XX.
  Cnhp (Wung w) j -> do
    t <- open j
    pure $ Wng (Ally "" : w) [([Axis 6], t)]
  -}
  Cnhp h j -> do
    s <- open h
    t <- open j
    pure $
      Pus s $
        Wng [Ally "", Axis 2]
          [ ( [Axis 6]
            , Wit (Wng [Axis 3] []) t
            )
          ]
  Cncl h hs -> open $ Cnhp h (Cltr hs)
  Cnkt h j k l -> open $ Cnhp h (Clls j k l)
  Cnls h j k -> open $ Cnhp h (Clhp j k)
  Cnts w whs -> Wng w <$> traverse (\(w, h) -> (w,) <$> open h) whs
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
  Ktwt w h -> Hid w <$> open h
  Ktts s h -> Fac <$> flay s <*> open h
  Ktcn h -> Left "unsupported ^%"
  Ktcl{} -> Left "unsupported ^: mold"
  --
  Sgfs{} -> Left "unsupported ~/"
  --
  Tsfs s h j -> open $ Tsls (Ktts s h) j
  Tsmc s h j -> open $ Tsfs s j h
  Tscl s h j -> Mut <$> open s <*> open h <*> open j
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
  Wthp w [] -> Left "empty ?-"
  Wthp w [(s, h)] -> Rhe <$> open (Wtts s $ Wung w) <*> open h
  Wthp w ((s, h):cs) ->
    Tes <$> open (Wtts s $ Wung w) <*> open h <*> open (Wthp w cs)
  Wtcl h j k -> Tes <$> open h <*> open j <*> open k
  Wtdt h j k -> open $ Wtcl j h k
  -- we possibly want to move "^" from being a type to being a skin exclusively
  Wtkt w h j -> open $ Wtcl (Wtts (Clhp Wild Wild) (Wung w)) h j
  Wtgl{} -> Left "unsupported ?>"
  Wtgr{} -> Left "unsupported ?<"
  Wtpm{} -> Left "unsupported ?&"
  -- this suggests we probably want a skin notation for "any atom"; "@@"?
  Wtpt w h j -> open $ Wtcl (Wtts (Clhp Wild Wild) (Wung w)) j h
  Wtts s h -> Fis <$> flay s <*> open h
  Wtwt h j -> Rhe <$> open h <*> open j
  Wtzp{} -> Left "unsupported ?!"
  --
  Zpzp -> Left "unsupported !!"
  --
  Hxgl{} -> Left "not available in user syntax: #<"
  Hxgr{} -> Left "not available in user syntax: #>"

flay :: Hoon -> Desugar Pelt
flay = \case
  Wild -> pure Punt
  Wung [Ally f] -> pure $ Peer f
  Adam g a au -> pure $ Part a
  --
  Clcb h j -> Pair <$> flay j <*> flay h
  Clkt h j k l -> Pair <$> flay h
                       <*> (Pair <$> flay j
                                 <*> (Pair <$> flay k
                                           <*> flay l))
  Clhp h j -> Pair <$> flay h <*> flay j
  Clls h j k -> Pair <$> flay h <*> (Pair <$> flay j <*> flay k)
  Clsg [] -> pure $ Part 0
  Clsg (h:hs) -> Pair <$> flay h <*> flay (Clsg hs)
  Cltr [] -> Left "empty :*"
  Cltr [h] -> flay h
  Cltr (h:hs) -> Pair <$> flay h <*> flay (Cltr hs)
  --
  Kthp t h -> Pest <$> flay h <*> open t
  Ktfs h t -> Pest <$> flay h <*> open t
  Ktzp t h -> Past <$> flay h <*> open t
  --
  h -> Left ("flay-meat: expression in pattern context: " <> tshow h)

-- | Resugar a Soft into a Hoon.
shut :: Soft -> Hoon
shut = \case
  Wng w [] -> Wung w
  -- Wng (Ally "" : ws) [([Axis 6], s)] -> sla (Wng ws []) s
  Wng w wss -> Cnts w $ map (\(w, s) -> (w, shut s)) wss
  --
  Atm a g au -> Adam g a au
  Cel c d -> case shut d of
    Clhp h j -> Clls (shut c) h j
    Clls h j k -> Clkt (shut c) h j k
    Clkt h j k l -> Cltr [shut c, h, j, k, l]
    Cltr hs -> Cltr (shut c : hs)
    h -> Clhp (shut c) h
--  Lam c d -> Brts (shut c) (shut d)
  Fac p c -> Ktts (flap p) (shut c)
  Cru cs -> Brcn (fmap shut cs)
  --Core b p -> Tsgr (shut p) (Brcn $ fmap (shut (unname e p) . fromScope) b)
  --
  Plu c -> Dtls (shut c)
  Equ c d -> Dtts (shut c) (shut d)
  Tes c d e -> Wtcl (shut c) (shut d) (shut e)
  Rhe c d -> Wtwt (shut c) (shut d)
  Fis p c -> Wtts (flap p) (shut c)
  --
  Aur au Bowl -> Bass (HC.Aur au)
  Aur "n" (Fork as) | as == setFromList [0] -> Bass HC.Nul
  Aur "f" (Fork as) | as == setFromList [0, 1] -> Bass HC.Flg
  Aur au (Fork as) -> Bass (HC.Fok (toList as) au)
  Ral c d -> case shut d of
    Bccl s ss -> Bccl (shut c) (s:ss)
    s -> Bccl (shut c) [s]
  Gat c d -> Bchp (shut c) (shut d)
  Cor c d es -> Bcbr (shut c) (shut d) (fmap shut es)
  --Fok t ss -> Bcgr (shut t) (map flap ss)
  Sin c t -> Bcts (shut c) (shut t)
  Mot t u -> Bcmc (shut t) (shut u)
  Fus c p -> Bcgr (shut c) (flap p)
  Sal [Axis 7] (Cor (Cel (Mot _ Non) _) (Cel s _) (mapToList -> [("", t)])) ->
    Bchp (shut s) (shut t)
  Sal [Axis 7] (Cor (Cel Non _) (Cel s _) (mapToList -> [("", t)])) ->
    Bchp (shut s) (shut t)
  Sal w t -> Bcwt w (shut t)
  Non -> Bass HC.Non
  Vod -> Bass HC.Vod
  Typ -> Bass HC.Typ
  --
  -- XX ridiculous hack for printing holds
  Wit (Wit _ (Cru cs)) c
    | (ar, _) :  _ <- filter (\(_,d) -> c == d) (mapToList cs) ->
      Wung [Ally ar]
  -- XX another ridiculous hack because lofted Crux' has this tisgar that I
  -- basically never want to see, but which is needed for the correct semantics
  -- (XX rethink? also how can we resugar to |=?)
  -- This will cause glorious Issues if the user source has an actual =>  |% lol
  Wit _ (Cru cs) -> shut (Cru cs)
  Wit c d -> Tsgr (shut c) (shut d)
  Pus c (Wng [Ally "", Axis 2] [([Axis 6], Wit (Wng [Axis 3] []) d)]) -> sla c d
  Pus (Atm 0 Sand _) (Cru (mapToList -> [("", d)])) -> Brts Wild (shut d)
  Pus c d -> Tsls (shut c) (shut d)
  Mut c
    (Net { sof = Atm 0 Sand "", typ = Non })
    (Cru (mapToList -> [("", d)])) ->
   Brts (shut c) (shut d)
  Mut c d e -> Tscl (shut c) (shut d) (shut e)
  --Case c ss ds -> error "open: case"
  Net{sof, typ} -> Kthp (shut typ) (shut sof)  -- XX should print as / wide
  Cat{sof, typ} -> Ktzp (shut typ) (shut sof)
  Hid w s -> Ktwt w (shut s)
 where
  sla c d = case shut d of
    Clhp h j -> Cnls (shut c) h j
    Clls h j k -> Cnkt (shut c) h j k
    Clkt h j k l -> Cncl (shut c) [h, j, k, l]
    Cltr hs -> Cncl (shut c) hs
    h -> Cnhp (shut c) h

-- | Read back a Soft into a hoon without resugaring much.
shut' :: Soft -> Hoon
shut' = \case
  Wng w [] -> Wung w
  Wng w wss -> Cnts w $ map (\(w, s) -> (w, shut' s)) wss
  --
  Atm a g au -> Adam g a au
  Cel c d -> case shut' d of
    Clhp h j -> Clls (shut' c) h j
    Clls h j k -> Clkt (shut' c) h j k
    Clkt h j k l -> Cltr [shut' c, h, j, k, l]
    Cltr hs -> Cltr (shut' c : hs)
    h -> Clhp (shut' c) h
--  Lam c d -> Brts (shut' c) (shut' d)
  Fac p c -> Ktts (flap p) (shut' c)
  Cru cs -> Brcn (fmap shut' cs)
  --Core b p -> Tsgr (shut' p) (Brcn $ fmap (shut' (unname e p) . fromScope) b)
  --
  Plu c -> Dtls (shut' c)
  Equ c d -> Dtts (shut' c) (shut' d)
  Tes c d e -> Wtcl (shut' c) (shut' d) (shut' e)
  Rhe c d -> Wtwt (shut' c) (shut' d)
  Fis p c -> Wtts (flap p) (shut' c)
  --
  Aur au Bowl -> Bass (HC.Aur au)
  Aur "n" (Fork as) | as == setFromList [0] -> Bass HC.Nul
  Aur "f" (Fork as) | as == setFromList [0, 1] -> Bass HC.Flg
  Aur au (Fork as) -> Bass (HC.Fok (toList as) au)
  Ral c d -> case shut' d of
    Bccl s ss -> Bccl (shut' c) (s:ss)
    s -> Bccl (shut' c) [s]
  Gat c d -> Bchp (shut' c) (shut' d)
  Cor c d es -> Bcbr (shut' c) (shut' d) (fmap shut' es)
  --Fok t ss -> Bcgr (shut' t) (map flap ss)
  Sin c t -> Bcts (shut' c) (shut' t)
  Mot t u -> Bcmc (shut' t) (shut' u)
  Fus c p -> Bcgr (shut' c) (flap p)
  Sal w t -> Bcwt w (shut' t)
  Non -> Bass HC.Non
  Vod -> Bass HC.Vod
  Typ -> Bass HC.Typ
  --
  Wit c d -> Tsgr (shut' c) (shut' d)
  Pus c d -> Tsls (shut' c) (shut' d)
  Mut c d e -> Tscl (shut' c) (shut' d) (shut' e)
  --Case c ss ds -> error "open: case"
  Net{sof, typ} -> Kthp (shut' typ) (shut' sof)  -- XX should print as / wide
  Cat{sof, typ} -> Ktzp (shut' typ) (shut' sof)
  Hid w s -> Ktwt w (shut' s)

flap :: Pelt -> Hoon
flap = \case
  Punt -> Wild
  Peer f -> Wung [Ally f]
  Part a -> Adam Rock a (heuAura a)
  Pair p q -> case flap q of
    Clhp h j -> Clls (flap p) h j
    Clls h j k -> Clkt (flap p) h j k
    Clkt h j k l -> Cltr [flap p, h, j, k, l]
    Cltr hs -> Cltr (flap p : hs)
    h -> Clhp (flap p) h
  --Pons p q -> Ktts (flap p) (flap q)
  Pest p c -> Ktfs (flap p) (shut c)
  Past p c -> Ktzp (shut c) (flap p)

-- | Hack to make Bases pretty printable somehow. This is for the implementor
-- and should not be used in user-facing diagnostics.
lock :: Var a => Semi a -> Hoon
lock = \case
  Spot' a -> Wung [Ally $ tshow a]
  Fore' x -> Wung [Ally $ tshow $ Old @Text x]
  Hold' t x c -> Hxgl (shut . rest $ c) (lock x)  -- Wung [Ally t]
  --
  Atom' a -> Adam Sand a (heuAura a)
  Cell' x y -> case lock y of
    Clhp h j -> Clls (lock x) h j
    Clls h j k -> Clkt (lock x) h j k
    Clkt h j k l -> Cltr [lock x, h, j, k, l]
    Cltr hs -> Cltr (lock x : hs)
    h -> Clhp (lock x) h
  -- Lamb' (Jamb c x) -> Tsgr (lock x) $ Brts Wild (shut . rest $ c)
  Crux' _ as x -> Tsgr (lock x) $ Brcn (fmap (shut . rest) as)
  --
  Pull' ar _ x -> case lock x of
    Cnts w eds -> Cnts (Ally ar : w) eds
    h -> Tsgl (Wung [Ally ar]) h
  Plus' x -> Dtls (lock x)
  {-
  Slam' x y ->  case lock y of
    Clhp h j -> Cnls (lock x) h j
    Clls h j k -> Cnkt (lock x) h j k
    Clkt h j k l -> Cncl (lock x) [h, j, k, l]
    Cltr hs -> Cncl (lock x) hs
    h -> Cnhp (lock x) h
  -}
  Equl' x y -> Dtts (lock x) (lock y)
  Test' x y z -> Wtcl (lock x) (lock y) (lock z)
  Fish' f x -> Wtts (flap $ pond f) (lock x)
  Look' x a -> Tsgl (Wung [Axis a]) (lock x)
  Edit' x a y -> case lock x of
    Cnts w eds -> Cnts w (eds ++ [([Axis a], lock y)])
    h -> Tsgr h (Cnts [] [([Axis a], lock y)])
  --
  Aura' au Bowl -> Bass (HC.Aur au)
  Aura' "n" (Fork as) | as == setFromList [0] -> Bass HC.Nul
  Aura' "f" (Fork as) | as == setFromList [0, 1] -> Bass HC.Flg
  Aura' au (Fork as) -> Bass (HC.Fok (toList as) au)
  Rail' t (Jamb c s) -> Tsgr (lock s) $ Bccl (lock t) case shut . rest $ c of
    Bccl h hs -> h:hs
    h -> [h]
  Gate' t (Jamb c s) ->
    Tsgr (lock s) $ Bchp (lock t) (shut . rest $ fmap hack c)
  Core' t (Comb s js) u ->
    Tsgr (lock s) $ Bcbr (lock u) (lock t) (fmap (shut . rest) js)
--  Fork' fs t -> Bcgr (lock t) (map (flap . pond) $ setToList fs)
  Sing' x y -> Bcts (lock x) (lock y)
  Molt' x y -> Bcmc (lock x) (lock y)
  Fuse' x f -> Bcgr (lock x) (flap $ pond f)
  Face' (Mask m) x -> Ktts (Wung [Ally m]) (lock x)
  Face' (Link ls) x -> Ktts Wild (lock x)  -- FIXME ?
  Seal' a x -> Bcwt [Axis a] (lock x)
  Noun' -> Bass HC.Non
  Void' -> Bass HC.Vod
  Type' -> Bass HC.Typ
 where
  hack :: Show a => a -> Wing
  hack = singleton . Ally . tshow

