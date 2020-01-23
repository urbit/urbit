module Ur.Noun.Tank where

import ClassyPrelude
import Ur.Noun.Conversions
import Ur.Noun.TH

--------------------------------------------------------------------------------

type Tang = [Tank]

data Tank
    = Leaf Tape
    | Plum Plum
    | Palm (Tape, Tape, Tape, Tape) [Tank]
    | Rose (Tape, Tape, Tape) [Tank]
  deriving (Eq, Ord, Show)

data WideFmt = WideFmt { delimit :: Cord, enclose :: Maybe (Cord, Cord) }
  deriving (Eq, Ord, Show)

data TallFmt = TallFmt { intro :: Cord, indef :: Maybe (Cord, Cord) }
  deriving (Eq, Ord, Show)

data PlumFmt = PlumFmt (Maybe WideFmt) (Maybe TallFmt)
  deriving (Eq, Ord, Show)

type Plum = AtomCell Cord PlumTree

data PlumTree
    = Para Cord [Cord]
    | Tree PlumFmt [Plum]
    | Sbrk Plum
  deriving (Eq, Ord, Show)

deriveNoun ''WideFmt
deriveNoun ''TallFmt
deriveNoun ''PlumFmt
deriveNoun ''Tank
deriveNoun ''PlumTree

--------------------------------------------------------------------------------

data WashCfg = WashCfg
    { wcIndent :: Word
    , wcWidth  :: Word
    }

--------------------------------------------------------------------------------

wash :: WashCfg -> Tank -> Wall
wash _cfg t = [ram t]

-- win :: WashCfg -> Tank -> Wall
-- win = undefined

flat :: Plum -> Tape
flat = Tape . tshow

ram :: Tank -> Tape
ram = \case
    Leaf tape           -> tape
    Plum plum           -> flat plum
    Palm (p,q,r,s) kids -> ram (Rose (p, q<>r, s) kids)
    Rose (p,q,r) kids   -> q <> loop kids
      where
        loop []     = r
        loop [x]    = ram x <> r
        loop (x:xs) = ram x <> p <> loop xs

{-
  ++  win
    |=  {tab/@ edg/@}
    =.  tac  (act:ug tac)
    %-  fix:ug
    =+  lug=`wall`~
    |^  |-  ^-  wall
        ?-    -.tac
            $leaf  (rig p.tac)
            $plum  (turn ~(tall plume p.tac) |=(=cord (trip cord)))
            $palm
          ?:  fit
            (rig ram)
          ?~  q.tac
            (rig q.p.tac)
          ?~  t.q.tac
            (rig(tab (add 2 tab), lug $(tac i.q.tac)) q.p.tac)
          =>  .(q.tac `(list tank)`q.tac)
          =+  lyn=(mul 2 (lent q.tac))
          =+  ^=  qyr
              |-  ^-  wall
              ?~  q.tac
                lug
              %=  ^$
                tac  i.q.tac
                tab  (add tab (sub lyn 2))
                lug  $(q.tac t.q.tac, lyn (sub lyn 2))
              ==
          (wig(lug qyr) q.p.tac)
        ::
            $rose
          ?:  fit
            (rig ram)
          =.  lug
            |-  ^-  wall
            ?~  q.tac
              ?:(=(~ r.p.tac) lug (rig r.p.tac))
            ^$(tac i.q.tac, lug $(q.tac t.q.tac), tab din)
          ?:  =(~ q.p.tac)
            lug
          (wig q.p.tac)
        ==
    ::
    ++  din  (mod (add 2 tab) (mul 2 (div edg 3)))
    ++  fit  (lte (lent ram) (sub edg tab))
    ++  rig
      |=  hom/tape
      ^-  wall
      ?:  (lte (lent hom) (sub edg tab))
        [(runt [tab ' '] hom) lug]
      =>  .(tab (add tab 2), edg (sub edg 2))
      =+  mut=(trim (sub edg tab) hom)
      :-  (runt [(sub tab 2) ' '] ['\\' '/' (weld p.mut `_hom`['\\' '/' ~])])
      =>  .(hom q.mut)
      |-
      ?~  hom
        :-  %+  runt
              [(sub tab 2) ' ']
            ['\\' '/' (runt [(sub edg tab) ' '] ['\\' '/' ~])]
        lug
      =>  .(mut (trim (sub edg tab) hom))
      [(runt [tab ' '] p.mut) $(hom q.mut)]
    ::
    ++  wig
      |=  hom/tape
      ^-  wall
      ?~  lug
        (rig hom)
      =+  lin=(lent hom)
      =+  wug=:(add 1 tab lin)
      ?.  =+  mir=i.lug
          |-  ?~  mir
                |
              ?|(=(0 wug) ?&(=(' ' i.mir) $(mir t.mir, wug (dec wug))))
        (rig hom)       :: ^ XX regular form?
      [(runt [tab ' '] (weld hom `tape`[' ' (slag wug i.lug)])) t.lug]
    --
  --
-}

{-
++  re
  |_  tac/tank
  ++  ram
    ^-  tape
    ?-    -.tac
        $leaf  p.tac
        $plum  ~(flat plume p.tac)
        $palm  ram(tac [%rose [p.p.tac (weld q.p.tac r.p.tac) s.p.tac] q.tac])
        $rose
      %+  weld
        q.p.tac
      |-  ^-  tape
      ?~  q.tac
        r.p.tac
      =+  voz=$(q.tac t.q.tac)
      (weld ram(tac i.q.tac) ?~(t.q.tac voz (weld p.p.tac voz)))
    ==
  ::
  ++  ug                                                ::  horrible hack
    |%
    ++  ace                                             ::  strip ctrl chars
      |=  a=tape
      ^-  tape
      ?~  a  ~
      ?:  |((lth i.a 32) =(127 `@`i.a))
        $(a t.a)
      [i.a $(a t.a)]
    ::
    ++  act                                             ::  pretend tapes
      |=  tac=tank
      ^-  tank
      ?-  -.tac
        %leaf  [%leaf (hew p.tac)]
        %plum  tac    ::  XX consider
        %palm  :+  %palm
                 [(hew p.p.tac) (hew q.p.tac) (hew r.p.tac) (hew s.p.tac)]
               (turn q.tac act)
        %rose  :+  %rose
                 [(hew p.p.tac) (hew q.p.tac) (hew r.p.tac)]
               (turn q.tac act)
      ==
    ::
    ++  fix                                             ::  restore tapes
      |=  wol=wall
      %+  turn  wol
      |=(a=tape (tufa `(list @c)``(list @)`a))
    ::
    ++  hew                                             ::  pretend tape
      |=(a=tape `tape``(list @)`(tuba (ace a)))
    --
  ::
  ++  win
    |=  {tab/@ edg/@}
    =.  tac  (act:ug tac)
    %-  fix:ug
    =+  lug=`wall`~
    |^  |-  ^-  wall
        ?-    -.tac
            $leaf  (rig p.tac)
            $plum  (turn ~(tall plume p.tac) |=(=cord (trip cord)))
            $palm
          ?:  fit
            (rig ram)
          ?~  q.tac
            (rig q.p.tac)
          ?~  t.q.tac
            (rig(tab (add 2 tab), lug $(tac i.q.tac)) q.p.tac)
          =>  .(q.tac `(list tank)`q.tac)
          =+  lyn=(mul 2 (lent q.tac))
          =+  ^=  qyr
              |-  ^-  wall
              ?~  q.tac
                lug
              %=  ^$
                tac  i.q.tac
                tab  (add tab (sub lyn 2))
                lug  $(q.tac t.q.tac, lyn (sub lyn 2))
              ==
          (wig(lug qyr) q.p.tac)
        ::
            $rose
          ?:  fit
            (rig ram)
          =.  lug
            |-  ^-  wall
            ?~  q.tac
              ?:(=(~ r.p.tac) lug (rig r.p.tac))
            ^$(tac i.q.tac, lug $(q.tac t.q.tac), tab din)
          ?:  =(~ q.p.tac)
            lug
          (wig q.p.tac)
        ==
    ::
    ++  din  (mod (add 2 tab) (mul 2 (div edg 3)))
    ++  fit  (lte (lent ram) (sub edg tab))
    ++  rig
      |=  hom/tape
      ^-  wall
      ?:  (lte (lent hom) (sub edg tab))
        [(runt [tab ' '] hom) lug]
      =>  .(tab (add tab 2), edg (sub edg 2))
      =+  mut=(trim (sub edg tab) hom)
      :-  (runt [(sub tab 2) ' '] ['\\' '/' (weld p.mut `_hom`['\\' '/' ~])])
      =>  .(hom q.mut)
      |-
      ?~  hom
        :-  %+  runt
              [(sub tab 2) ' ']
            ['\\' '/' (runt [(sub edg tab) ' '] ['\\' '/' ~])]
        lug
      =>  .(mut (trim (sub edg tab) hom))
      [(runt [tab ' '] p.mut) $(hom q.mut)]
    ::
    ++  wig
      |=  hom/tape
      ^-  wall
      ?~  lug
        (rig hom)
      =+  lin=(lent hom)
      =+  wug=:(add 1 tab lin)
      ?.  =+  mir=i.lug
          |-  ?~  mir
                |
              ?|(=(0 wug) ?&(=(' ' i.mir) $(mir t.mir, wug (dec wug))))
        (rig hom)       :: ^ XX regular form?
      [(runt [tab ' '] (weld hom `tape`[' ' (slag wug i.lug)])) t.lug]
    --
  --
-}
