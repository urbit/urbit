!:
|%
::  atom ops
::
+|  %atomics
::
++  plot
  =>  |%
      +$  plat
        $@  @                                       ::  measure atom
        $^  $%  [[%c ~] (pair (pair step step) @)]  ::  cut slice
                [[%m ~] (pair (pair step step) @)]  ::  measure slice
                [[%s ~] p=plot]                     ::  subslice
            ==                                      ::
        (pair step @)                               ::  prefix
      --                                            ::
  |^  $^  [l=$ r=$]                                 ::  concatenate
      [a=bloq b=(list plat)]                        ::  serialize
  ::
  ::  +fax: encode a plot
  ::
  ++  fax
    |=  p=$
    ^-  (trel @ bloq step)
    ?^  -.p
      =/  l  $(p l.p)
      =/  r  $(p r.p)
      =/  s  (rig +.l q.r)
      [(add p.l (lsh [q.r s] p.r)) q.r (add r.r s)]
    ::
    ?~  b.p  [0 a.p 0]
    =;  c=(pair @ step)
      =/  d  $(b.p t.b.p)
      [(add p.c (lsh [a.p q.c] p.d)) a.p (add q.c r.d)]
    ::
    ?@  i.b.p
      [i.b.p (met a.p i.b.p)]
    ?-  -.i.b.p
      @       [(end [a.p p.i.b.p] q.i.b.p) p.i.b.p]
      [%c ~]  [(cut a.p [p q]:i.b.p) q.p.i.b.p]
      [%m ~]  =+((cut a.p [p q]:i.b.p) [- (met a.p -)])
      [%s ~]  =/  e  $(p p.i.b.p)
              [p.e (rig +.e a.p)]
    ==
  ::
  ::  +mes: measure a plot
  ::
  ++  mes
    |=  p=$
    ^-  [q=bloq r=step]
    ?^  -.p
      =/  l  $(p l.p)
      =/  r  $(p r.p)
      =/  s  (rig l q.r)
      [q.r (add r.r s)]
    ::
    ?~  b.p  [a.p 0]
    =;  c=q=step
      =/  d  $(b.p t.b.p)
      [a.p (add q.c r.d)]
    ::
    ?@  i.b.p
      (met a.p i.b.p)
    ?-  -.i.b.p
      @       p.i.b.p
      [%c ~]  q.p.i.b.p
      [%m ~]  =+((cut a.p [p q]:i.b.p) (met a.p -))
      [%s ~]  =/  e  $(p p.i.b.p)
              (rig e a.p)
    ==
  --
::
::  +rig: convert between bloqs
::
++  rig
  |=  [=bite b=bloq]
  ^-  step
  ?@  bite  0
  =/  [a=bloq c=step]  bite
  ?:  =(a b)  c
  ?:  (gth a b)
    (lsh [0 (sub a b)] c)
  =/  d  [0 (sub b a)]
  =/  e  (rsh d c)
  ?:(=(0 (end d c)) e +(e))
::
::  +nac: reverse +can
::
++  nac
  |=  [a=bloq b=(list (pair step step)) c=@]
  ^-  (list @)
  ?~  b  ~
  [(cut a [i.b] c) $(b t.b)]
::
::  +dew: dry +hew
::
++  dew
  |=  [a=bite b=* c=@]
  ^-  *
  =<  -
  =/  [d=bloq e=step]  ?^(a a [a 0])
  |-  ^-  (pair * step)
  ?@  b
    [(cut d [e b] c) (add e b)]
  =^  f  e  $(b -.b)
  =^  g  e  $(b +.b)
  [[f g] e]
::
::  +hew: cut many
::
++  hew
  |=  [a=bite c=@]
  =/  d=[=bloq =step]  ?^(a a [a 0])
  |*  b=*
  ^+  [b d]
  ?@  b
    [(cut bloq.d [step.d b] c) bloq.d (add step.d b)]
  =^  f  d  $(b -.b)
  =^  g  d  $(b +.b)
  [[f g] d]
::
++  clz
  |=  [a=bite b=@]
  (sub ?@(a (bex a) (mul (bex bloq.a) step.a)) (met 0 b))
::
++  ctz
  |=  a=@
  ?:  =(0 a)  0
  =|  i=@ud
  |-(?:(=(1 (cut 0 [i 1] a)) i $(i +(i))))
::
++  ham  :: popcount
  |=  a=@
  ?:  =(0 a)  0
  =|  n=@ud
  =/  m  (dec (met 0 a))
  |-  ^-  @ud
  =?  n  =(1 (cut 0 [m 1] a))
    +(n)
  ?:(=(0 m) n $(m (dec m)))
::
::  binary tree ops
::
+|  %arboric
::
++  bao
  |=  n=@ud
  =|  i=@ud
  =|  s=(list)
  |-  ^-  *
  ?:  =(i n)
    =^  d  s  s
    |-(?~(s d $(d [i.s d], s t.s)))
  ::
  =/  d=*  i
  =.  i  +(i)
  =/  j  (ctz i)
  |-  ^-  *
  ?:  =(0 j)
    ^$(s [d s])
  =^  e  s  s
  $(d [e d], j (dec j))
::
++  unroll
  |=  d=*
  =|  s=(list [axe=@ d=*])
  =/  a  1
  |-  ^+  s
  ?@  d
    ?~  s  ~
    $(d d.i.s, a axe.i.s, s t.s)
  :-  [a d]
  $(d -.d, a (peg a 2), s [[(peg a 3) +.d] s])
::
+|  %messages
::
++  mess
  =>  |%
      +$  auth  (each @uxJ @uxH) :: &+sig, |+hmac
      +$  gage  $@(~ page)
      +$  sage  (trel spar:ames auth gage)
      --
  $%  [%page sage]
      [%peek p=spar:ames]
      [%poke p=spar:ames q=sage]
  ==
::
::  packet de/serialization
::
+|  %packets
::
::    > :(add 8 305 1.159)
::    1.472
::
++  pact
  =>  |%
      +$  frag  @udF
      +$  ship  @pH
      +$  rift  @udF
      +$  bloq  @D
      +$  name
        $:  [her=ship rif=rift]
            [boq=bloq wan=$@(~ [typ=?(%auth %data) fag=frag])]
            pat=path
        ==
      +$  auth
        ::
        ::  %0 for fragment 0 or auth packet
        ::    ~      for 1-fragment message and auth packets
        ::    [~ %&] for >4-fragment messages
        ::    [~ %&] for 2-fragment messages
        ::    [~ %|] for 3-4-fragment
        ::
        ::  %1 for fragments 1 - N/2
        ::  ~  for fragments (N/2)+1 - N
        ::
        $@  ~
        $%  [%0 p=auth:mess q=(unit $@(@uxI (pair @uxI @uxI)))]
            [%1 p=(pair @uxI @uxI)]
        ==
      +$  data  [tot=frag aut=auth:pact dat=@]
      +$  lane  $@  @ux
                $%  [%if p=@ifF q=@udE]
                    [%is p=@isH q=@udE]
                ==
      +$  next  (list lane)
      +$  pact  $%  [%page p=name q=data r=next]
                    [%poke p=name q=name r=data]
                    [%peek p=name]
                ==
      --
  ::
  |%
  ++  en
    |=  pak=pact
    ^-  plot
    =/  bod=plot
      ?-  -.pak
        %page  [(en:^name p.pak) (en:^data q.pak) (en:^next r.pak)]
        %peek  (en:^name p.pak)
        %poke  [(en:^name p.pak) (en:^name q.pak) (en:^data r.pak)]
      ==
    =/  hed=plot
      =/  nex=@B
        ?.  ?=(%page -.pak)  0b0
        ?~  r.pak            0b0
        ?^  t.r.pak          0b11
        ?:(?=([%if *] i.r.pak) 0b1 0b10)
      =/  hop  0 :: XX
      (en:head nex -.pak hop (mug p:(fax:plot bod)))
    [hed bod]
  ::
  ++  de
    |=  a=bite
    |=  dat=@
    ^-  [pact boq=bloq sep=step]
    =+  ^=  [hed b]  ((de:head a) dat)
    =+  ^=  [pac c]
      ?-  typ.hed
        %page  =^  nam  b  ((de:^name b) dat)
               =^  dat  b  ((de:^data b) dat)
               =^  nex  b  ((de:^next b nex.hed) ^dat)
               [[typ.hed nam dat nex] b]
      ::
        %peek  =^  nam  b  ((de:^name b) dat)
              [[typ.hed nam] b]
      ::
        %poke  =^  nam  b  ((de:^name b) dat)
               =^  dam  b  ((de:^name b) dat)
               =^  dat  b  ((de:^data b) dat)
               [[typ.hed nam dam dat] b]
      ==
    =/  gum
      (end [0 20] (mug (cut -.c [(rig b -.c) +.c] dat)))
    ?>(=(gum.hed gum) [pac c])
  --
::
++  head
  |%
  ++  en
    |=  [nex=@B typ=?(%page %peek %poke) hop=@ gum=@F]
    ^-  plot
    =/  tip  ?-(typ %page 0b1, %peek 0b10, %poke 0b11)
    =.  hop  (min 7 hop)
    =*  tok  [32 ~tasfyn-partyv]
    :-  bloq=0
    [[2 0] [2 nex] [3 ver=1] [2 tip] [3 hop] [20 gum] tok ~]
  ::
  ++  de
    |=  a=bite
    =/  b=[bloq step]  [0 (rig a 0)]
    |=  dat=@
    ^-  [[nex=@B typ=?(%page %peek %poke) hop=@ gum=@F] bloq step]
    =^  c  b
      ((hew b dat) [res=2 nex=2 ver=3 tip=2 hop=3 gum=20 tok=32])
    ?>  =(0 res.c)
    ?>  =(1 ver.c)
    ?>  =(~tasfyn-partyv tok.c)
    =/  typ  ?+(tip.c !! %0b1 %page, %0b10 %peek, %0b11 %poke)
    [[nex.c typ hop.c gum.c] b]
  --
::
++  next
  |%
  ++  en
    |=  nex=next:pact
    ^-  plot
    :-  bloq=3
    ?~  nex  ~
    ?:  ?=([[%if *] ~] nex)
      [[4 p] [2 q] ~]:i.nex
    |-  ^-  (list plat:plot)
    =;  one=(list plat:plot)
      ?~(t.nex one (weld one $(nex t.nex)))
    ?-  i.nex
      @        =/  l  (met 3 i.nex)
               ?>  (lth l 255)
               [[1 +(l)] [1 2] [l i.nex] ~]
      [%if *]  [[1 7] [1 0] [4 p] [2 q] ~]:i.nex
      [%is *]  =/  l  (met 3 p.i.nex)
               ?>  (lth l 253)
               [[1 (add 3 l)] [1 1] [l p.i.nex] [2 q.i.nex] ~]
    ==
  ::
  ++  de
    |=  [a=bite b=@B]
    =/  c=[bloq step]  [3 (rig a 3)]
    |=  dat=@
    ^-  [next:pact bloq step]
    =<  ?+  b  !!
          %0b0   [~ c]
        ::
          %0b1   =^  if=[@ @]  c  ((hew c dat) 4 2)
                 [[if+if ~] c]
        ::
          %0b10  =^  la  c  (need one)
                 [[la ~] c]
        ::
          %0b11  =|  nex=next:pact
                 |-  ^-  [next:pact bloq step]
                 =/  a  one
                 ?~  a  [(flop nex) c]
                 $(nex [-.u.a nex], c +.u.a)
        ==
    |%
    ++  one
      ^-  (unit [lane:pact bloq step])
      =^  raw  c  ((hew c dat) 1 1)
      ?:  =(0 -.raw)  ~
      ?+  +.raw  !!
        %0  ?>  =(7 -.raw)
            =^  if=[@ @]  c  ((hew c dat) 4 2)
            `[if+if c]
      ::
        %1  ?>  (gte -.raw 3)
            =^  is=[@ @]  c  ((hew c dat) (sub -.raw 3) 2)
            `[is+is c]
      ::
        %2  =^  la  c  ((hew c dat) (dec -.raw))
            `[la c]
      ==
    --
  --
::
::  +name: encoded-path
::
::  range:  { meta[1], her[2^1-4], rif[1-4], boq[1], fag[1-4], len[2], pat[2^0-16 - 1] }
::  max:    {      1,      16,         4,        1,      4,        2,      65.535      }
::  actual: {      1,      16,         4,        1,      4,        2,      277         }
::
::  XX increment path-len by one, exclude zero-length paths
::
::    > :(add 1 16 4 1 4 2 277)
::    305
::
::  for %poke:
::    > (div (sub 305 (mul 2 (sub 305 277))) 2)
::    124
::
++  name
  |%
  ++  en
    |=  name:pact
    ^-  plot
    =/  ran  :: XX wrong ?~(her 0 (dec (met 0 (met 4 (end 7 her)))))
      ?+  (clan:title her)  0b0
        %duke  0b1
        %earl  0b10
        %pawn  0b11
      ==
    =/  ryf  ?~(rif 0 (dec (met 3 rif)))  :: XX is rift always non-zero?
    =+  ^=  [nit tau gaf gyf fag]
      ?~  wan  [0b1 0b0 0b0 0 0]
      =/  gyf  ?~(fag.wan 1 (met 3 (end 5 fag.wan)))
      [0b0 ?:(?=(%auth typ.wan) 0b1 0b0) (dec gyf) gyf fag.wan]
    ::
    =/  tap  =-([p=(met 3 -) q=-] `@t`(rap 3 (join '/' pat)))
    ?>  &(?=(^ pat) (lth p.tap ^~((bex 16)))) :: XX truncate instead?
    :+  bloq=3
      [s+~ 0 [2 ran] [2 ryf] [1 nit] [1 tau] [2 gaf] ~]
    [[(bex +(ran)) her] [+(ryf) rif] [1 boq] [gyf fag] [2 p.tap] tap ~]
  ::
  ++  de
    |=  a=bite
    =/  b=[boq=bloq sep=step]  [0 (rig a 0)]
    |=  pat=@
    ^-  [name:pact _b]
    =^  c  b
      ((hew b pat) [ran=2 ryf=2 nit=1 tau=1 gaf=2])
    ::
    =.  b  [3 (rig b 3)]
    =^  d  b
      %-  (hew b pat)
      :^    who=[her=(bex +(ran.c)) rif=+(ryf.c)]
          boq=1
        fag=?:(=(0b1 nit.c) 0 +(gaf.c))
      tap=2
    ::
    ::  XX ?<  =(0 tap.d)
    =/  pat
      %+  rash  (cut boq.b [sep.b tap.d] pat)
      (more fas (cook crip (star ;~(less fas prn))))
    =/  wan
      ?.  =(0b1 nit.c)
        [?:(=(1 tau.c) %auth %data) fag.d]
      ?>(&(=(0 tau.c) =(0 fag.d)) ~)
    ::
    =.  sep.b  (add sep.b tap.d)
    [[who.d [boq.d wan] pat] b]
  --
::
::  +data: response data
::
::  range:  { meta[1], tot[1-4], aum[32*0-2], aup[32*0-2], len-len[0-1], len[0-255], dat[0-2^252-1] }
::  max:    {      1,      4,        64,          64,              1,        255,        2^252-1    }
::  actual: {      1,      4,        64,          64,              0,        2,          1.024      }
::
::  XX increment len-len by 3, recalculate max limits
::  XX max len-len s/b 32 to align with max bloq size
::  XX move tot after auth to avoid trailing zeros?
::
::    > :(add 1 4 64 64 2 1.024)
::    1.159
::
++  data
  |%
  ++  en
    |=  [tot=frag:pact aut=auth:pact dat=@]
    ^-  plot
    =/  lot  (met 3 (end 5 tot))
    ::
    =/  [[aul=@ubB aum=plat:plot] aur=@ubB aup=plat:plot]
      ?~  aut           [[0b0 0] 0b0 0]
      ?:  ?=(%1 -.aut)  [[0b1 [32 p]] 0b10 [32 q]]:p.aut
      :-  =>  p.aut
          ?:(?=(%& -) [0b10 64 p] [0b11 32 p])
      =/  [aur=@ubB has=(list plat:plot)]
        ?~    q.aut  [0b0 ~]
        ?@  u.q.aut  [0b1 [1 u.q.aut] ~]
        [0b10 [[1 p] [1 q] ~]:u.q.aut]
      [aur s+~ 8 has]
    ::
    =/  len  (met 3 dat)
    =/  nel  (met 3 len)
    =/  men=(pair @B @A)
      ?:((lth nel 3) [nel 0] [0b11 1])
    :+  bloq=3
      [s+~ 0 [2 (dec lot)] [2 aul] [2 aur] [2 p.men] ~]
    [[lot tot] aum aup [q.men nel] [nel len] [len dat] ~]
  ::
  ++  de
    |=  a=bite
    =/  b=[boq=bloq sep=step]  [0 (rig a 0)]
    |=  dat=@
    ^-  [data:pact boq=bloq sep=step]
    =^  c  b
      ((hew b dat) [bot=2 [aul=2 aur=2] men=2])
    =.  b  [3 (rig b 3)]
    =^  d  b
      %-  (hew b dat)
      :^    tot=+(bot.c)
          aum=?+(aul.c 0x0 %0b10 `@`64, %0b11 `@`32)
        aup=?+(aur.c 0x0 %0b1 `@`32, %0b10 [`@`32 `@`32])
      nel=?.(=(3 men.c) 0 1)
    ::
    =/  aut=auth:pact
      =/  mes=(unit auth:mess)
        ?+  aul.c  !!
          %0b0   ?>(=(0b0 aur.c) ~)
          %0b1   ?>(=(0b10 aur.c) ~)
          %0b10  `&+aum.d
          %0b11  `|+aum.d
        ==
      =/  pac=(unit $@(@uxI (pair @uxI @uxI)))
        ?+  aur.c  !!
          %0b0   ?>(=(0 aup.d) ~)
          %0b1   ?>(?=(@ aup.d) `aup.d)
          %0b10  ?>(?=(^ aup.d) `aup.d)
        ==
      ?^  mes  [%0 u.mes pac]
      ?:  =(0b0 aul.c)  ~
      ?>  &(=(0b1 aul.c) ?=([~ @ @] pac))
      [%1 u.pac]
    ::
    =/  nel  ?.(=(3 men.c) men.c nel.d)
    =^  len  sep.b  [(cut 3 [sep.b nel] dat) (add sep.b nel)]
    =^  dat  sep.b  [(cut 3 [sep.b len] dat) (add sep.b len)]
    [[tot.d aut dat] b]
  --
::
++  name-to-beam
  |=  name:pact
  ^-  beam
  :*  [her %$ ud+1]
      %mess  (scot %ud rif)
      %pact  (scot %ud boq)  %etch
      ?~  wan  [%init pat]
      [typ.wan (scot %ud fag.wan) pat]
  ==
::
++  roundtrip
  |*  [dat=* en=$-(* plot) de=$-(@ [* boq=@ sep=@])]
  ^-  (unit _dat)
  =/  pol  (en dat)
  =/  ser  (fax:plot pol)
  =/  ron  (de p.ser)
  ?.  =(dat -.ron)
    ~&  %roundtrip-fail-a
    `-.ron
  ?.  =(q.ser boq.ron)
    ~&  [%roundtrip-fail-b q.ser boq.ron]
    `-.ron
  ?.  =(r.ser sep.ron)
    ~&  [%roundtrip-fail-c r.ser sep.ron]
    `-.ron
  ~
++  generator
  |%
  ++  just  |*(a=* |=(eny=@uvJ [a (shaz eny)]))
  ::
  ++  cook
    |*  [a=$-(* *) b=$-(@uvJ [* @uvJ])]
    |=  eny=@uvJ
    =^  c  eny  (b eny)
    [(a c) eny]
  ::
  ++  flag
    |=  eny=@uvJ
    =+  [b og]=(~(raws og eny) 1)
    [=(b 0) a.og]
  ::
  ++  rand
    |=  top=@u
    |=  eny=@uvJ
    =/  og  ~(. og eny)
    |-  ^-  [@ @uvJ]
    =^  a  og  (rads:og (met 0 top))
    =^  b  og  (raws:og a)
    ?:((gte b top) $ [b a.og])
  ::
  ++  bits
    |=  [zer=? top=@ud]
    |=  eny=@uvJ
    ^-  [@ @uvJ]
    =^  a  eny  ((rand top) eny)
    =+  [b og]=(~(raws og eny) a)
    ?:  &(!zer =(0 b))  $
    [b a.og]
  ::
  ++  char
    |=  [boq=bloq src=@]
    =/  wyd  (met boq src)
    |=  eny=@uvJ
    ^-  [@ @uvJ]
    =^  a  eny  ((rand wyd) eny)
    [(cut boq [a 1] src) eny]
  ::
  ++  many
    |*  [[min=@ud max=@ud] gen=$-(@uvJ [* @uvJ])]
    |=  eny=@uvJ
    =^  a  eny  ((rand max) eny)
    ?:  (lth a min)  $
    =|  [i=@ud lit=(list _-:$:gen)]
    |-  ^+  [lit eny]
    ?:  =(i a)  [lit eny]
    =^  b  eny  (gen eny)
    $(lit [b lit], i +(i))
  ::
  ++  both
    |*  [l=$-(@uvJ [* @uvJ]) r=$-(@uvJ [* @uvJ])]
    |=  eny=@uvJ
    =^  a  eny  (l eny)
    =^  b  eny  (r eny)
    [[a b] eny]
  ::
  ++  pick
    |*  [l=$-(@uvJ [* @uvJ]) r=$-(@uvJ [* @uvJ])]
    |=  eny=@uvJ
    =^  a  eny  (flag eny)
    (?:(a l r) eny)
  ::
  ++  aura
    =|  zer=?
    |=  yaz=@t
    ~+
    ^-  $-(@uvJ [@ @uvJ])
    =/  [max=@ud aur=@ta]
      =/  len  (met 3 yaz)
      ?:  =(0 len)
        [0 %$]
      =.  len  (dec len)
      =/  tyl  (rsh [3 len] yaz)
      ?.  &((gte tyl 'A') (lte tyl 'Z'))
        [0 yaz]
      [(sub tyl 'A') (end [3 len] yaz)]
    ::
    =-  ?@  -  (just -)
        ?:  ?=(%| -<)  ->
        (bits zer ?:(=(0 max) -> (bex max)))
    ::
    =+  yed=(rip 3 aur)
    ?+  yed  &+256
      [%c *]           :-  %|
                       %+  cook  (cury rep 5)
                       (many [0 256] (rand 0x11.0000))  :: XX nonzero?
    ::
      [%d ?(%a %r) *]  &+128
      [%f *]           &+1
      [%n *]           `@`0
      [%i %f *]        &+32
      [%i %s *]        &+128
      [?(%p %q) *]     &+128
      [%r %h *]        &+16
      [%r %s *]        &+32
      [%r %d *]        &+64
      [%r %q *]        &+128
    ::
      [%u %c *]        |+(cook enc:fa (bits & 256))
    ::
      [%t %a %s *]     :-  %|
                       %+  cook  crip
                       %+  both
                         (char 3 ^~((crip (gulf 'a' 'z'))))
                       %+  many  [0 32]
                       %+  char  3
                       ^~((crip (weld (gulf '0' '9') ['-' (gulf 'a' 'z')])))
    ::
      [%t %a *]        :-  %|
                       %+  cook  crip
                       %+  many  [0 64]
                       %+  char  3
                       ^~((crip :(weld "-~_." (gulf '0' '9') (gulf 'a' 'z'))))
    ::
      [%t *]           :-  %|
                       %+  cook  (corl tuft (cury rep 5))
                       (many [0 256] (rand 0x11.0000))  :: XX nonzero?
    ==
  ::
  ++  name
    =>  |%
        ++  ship  (aura 'pH')
        ++  rift  (aura 'udF')
        ++  bloq  (aura 'udD')
        ++  frag  rift
        ++  want  %+  pick  (just ~)
                  (both (pick (just %auth) (just %data)) frag)
        ++  path  (many [1 10] (aura %ta))
        --
    ^-  $-(@uvJ [name:pact @uvJ])
    %+  both  (both ship rift)
    (both (both bloq want) path)
  ::
  ++  data
    =>  |%
        ++  frag  (=+(aura -(zer |)) 'udF')
        ++  hash  (aura 'uxI')
        ++  mess-auth
          (pick (both (just %&) (aura 'uxJ')) (both (just %|) hash))
        ++  pact-auth
          (pick (just ~) (both (just ~) (pick hash (both hash hash))))
        ++  auth
          %+  pick  (just ~)
          %+  pick
            :(both (just %1) hash hash)
          :(both (just %0) mess-auth pact-auth)
        --
    ^-  $-(@uvJ [data:pact @uvJ])
    :(both frag auth (bits & ^~((bex 16))))
  ::
  ++  lane
    =>  |%
        ++  port  (aura 'udE')
        --
    %+  pick
      (bits & 256)
    %+  pick
      :(both (just %if) (aura 'ifF') port)
    :(both (just %is) (aura 'isH') port)
  ::
  ++  pactt
    ^-  $-(@uvJ [pact:pact @uvJ])
    %+  pick
      (both (just %peek) name)
    %+  pick
      :(both (just %page) name data (many [0 2] lane))
    :(both (just %poke) name name data)
  --
::
++  test
  |_  [eny=@uvJ len=@ud]
  ++  name
    =|  i=@ud
    |-  ^-  ?
    ?:  =(i len)  &
    =/  old  eny
    =^  nam  eny  (name:generator eny)
    ?^  ron=(roundtrip nam en:^name $:de:^name)
      ~&([i=i eny=old nam u.ron] |)
    $(i +(i))
  ::
  ++  data
    =|  i=@ud
    |-  ^-  ?
    ?:  =(i len)  &
    =/  old  eny
    =^  dat  eny  (data:generator eny)
    ?^  ron=(roundtrip dat en:^data $:de:^data)
      ~&([i=i eny=old dat u.ron] |)
    $(i +(i))
  ::
  ++  all
    =|  i=@ud
    |-  ^-  ?
    ?:  =(i len)  &
    =/  old  eny
    =^  pac  eny  (pactt:generator eny)
    ?^  ron=(roundtrip pac en:pact $:de:pact)
      ~&([i=i eny=old pac u.ron] |)
    $(i +(i))
  --
::
++  lss
  =,  blake:crypto
  |%
  ++  root-hash
    |=  o=output:blake3
    ^-  @ux
    (output-cv:blake3 (set-flag:blake3 f-root:blake3 o))
  ::
  ++  leaf-hash
    |=  [counter=@ leaf=@]
    ^-  @ux
    (output-cv:blake3 (chunk-output:blake3 counter 1.024^leaf))
  ::
  ::  +build: compute proof data for a message
  ::
  ++  build
    |=  msg=octs
    ^-  [root=@ux proof=(list @ux) pairs=(list [l=@ux r=@ux])]
    =/  chunks  (split-octs:blake3 13 msg)
    =+
      |-  ^-  [o=output:blake3 pairs=(list [l=@ux r=@ux])]
      =/  mid  (div (bex (xeb (dec (lent chunks)))) 2)
      =+  [l=(scag mid chunks) r=(slag mid chunks)]
      ?>  ?=(^ chunks)
      ?~  t.chunks  [(chunk-output:blake3 i.chunks) ~]
      =+  [left=$(chunks l) right=$(chunks r)]
      =/  pair  [(output-cv:blake3 o.left) (output-cv:blake3 o.right)]
      [(parent-output:blake3 pair) [pair (weld pairs.left pairs.right)]]
    =/  root  (root-hash o)
    ?:  =(~ pairs)  [root ~ ~]
    =/  height  (xeb (dec (lent chunks)))
    =/  proof  (turn (scag height pairs) tail)
    =.  proof  (flop (snoc proof l:(snag (dec height) pairs)))
    =.  pairs  (slag height pairs)
    [root proof pairs]
  ::
  ::  +root: compute just the root hash for a message
  ::
  ++  root
    |=  msg=@
    ^-  @ux
    (blake3 32 (met 3 msg)^msg)
  ::
  ::  +recover-root: compute the root hash of a leaf proof
  ::
  ++  recover-root
    |=  proof=(list @ux)
    ?>  ?=([@ @ *] proof)
    =*  l0  i.proof
    =*  l1  i.t.proof
    %-  root-hash
    %+  roll  t.t.proof
    |:  [p=0x0 n=(parent-output:blake3 l0 l1)]
    (parent-output:blake3 (output-cv:blake3 n) p)
  ::
  ::  +verifier: stateful core for sequentially verifying messages
  ::
  ++  verifier
    =<
      |%
      ::
      ++  init
        |=  [leaves=@ proof=(list @ux)]
        ^-  (unit state)
        ?~  proof
          ::  need at least two leaves to have a proof
          ::
          ?.  (lte leaves 1)  ~
          `[leaves 0 [0 1] ~ ~]
        ?.  ?=([@ @ *] proof)  ~
        ::  initialize leaf queue and parent stack with proof hashes;
        ::  after the first two leaves, the next subtree is [2 4]
        ::
        =/  state  [leaves 0 [0 1] ~ t.t.proof]
        `(push-leaves state [i.proof i.t.proof])
      ::
      ++  verify-msg
        |=  [=state [leaf=@ pair=(unit [l=@ux r=@ux])]]
        ^-  (unit _state)
        ?~  ustate=(verify-leaf state leaf)  ~
        ?~  pair  `u.ustate
        ?~  ustate=(verify-pair u.ustate u.pair)  ~
        ::  all good; if the pair held leaf hashes, add them
        ::  to the queue and advance past them; if it held parent
        ::  hashes, add them to the parent stack
        ::
        =.  state  (advance u.ustate)
        ?:  (at-leaf state)
          `(push-leaves state u.pair)
        `(push-parents state u.pair)
      --
    |%
    +$  state
        $:
            leaves=@
            leaf=@                 :: current leaf index
            cur=[l=@ r=@]          :: current pair subtree
            leaf-queue=(list @ux)
            parent-stack=(list @ux)
        ==
    ::
    ++  at-leaf  |=(state =(r.cur +(l.cur)))
    ::
    ++  advance
      |=  =state
      %=  state
        cur  =,  cur.state
            ::  if at a leaf, ascend the next subtree;
            ::  otherwise, descend into the left child
            ::
            ?:  (at-leaf state)
              [+(l) (min leaves.state (add r (bex (ctz r))))]
            [l (add l (bex (dec (xeb (dec (sub r l))))))]
      ==
    ::
    ++  push-leaves
      |=  [=state [l=@ux r=@ux]]
      ^+  state
      ::  NOTE: using a list as a queue isn't ideal, performance-wise,
      ::  but this list never grows larger than log(n) so in practice
      ::  it's fine
      ::
      %-  advance  %-  advance
      state(leaf-queue (weld leaf-queue.state ~[l r]))
    ::
    ++  push-parents
      |=  [=state [l=@ux r=@ux]]
      ^+  state
      state(parent-stack (weld ~[l r] parent-stack.state))
    ::
    ++  verify-leaf
      |=  [=state leaf=@]
      ^-  (unit _state)
      =/  wid  ?.(=(+(leaf.state) leaves.state) 1.024 (met 3 leaf))
      =/  cv  (output-cv:blake3 (chunk-output:blake3 leaf.state [wid leaf]))
      ::  if leaf queue is empty, draw from parent stack; this is
      ::  necessary for any tree with an odd number of leaves, since
      ::  such a tree will contain a pair where the left child is a
      ::  parent and the right child is a leaf
      ::
      ?^  leaf-queue.state
        ?.  =(i.leaf-queue.state cv)  ~
        `state(leaf +(leaf.state), leaf-queue t.leaf-queue.state)
      ?^  parent-stack.state
        ?.  =(i.parent-stack.state cv)  ~
        `state(leaf +(leaf.state), parent-stack t.parent-stack.state)
      ~
    ::
    ++  verify-pair
      |=  [=state pair=[l=@ux r=@ux]]
      ^-  (unit _state)
      =/  cv  (output-cv:blake3 (parent-output:blake3 pair))
      ?~  parent-stack.state          ~
      ?.  =(i.parent-stack.state cv)  ~
      `state(parent-stack t.parent-stack.state)
    --
  --
--
::
::  skeleton vane
::
=>
|%
+$  move  (pair duct (wite note gift))
::
+$  task
  $%  [%hear p=lane:pact q=@]            :: receive a packet
      [%mess p=(unit lane:pact) q=mess]  :: receive a message
      [%make-peek p=spar:ames]           :: initiate %peek request
      [%make-poke p=spar:ames q=path]    :: initiate %poke request
      :: XX combined with $<(%page mess) ?
      :: XX make encrypted request a la %chum / %keen+^
  ==
+$  gift
  $%  [%send p=(list lane:pact) q=@]     :: send a request/response packet
      [%response $>(%page mess)]         :: produce a response message
      :: XX encrypted response a la %near
  ==
::
+$  note
  $%  [%b %wait @da]
  ==
+$  sign
  $%  [%b %wake ~]
  ==
::
+$  axle
  $:  %0
      p=(map ship peer-state)
      :: XX tmp=(map @ux page)  :: temporary hash-addressed bindings
  ==
::  +address: client IP address
::
+$  address
  $%  [%ipv4 @if]
      [%ipv6 @is]
      ::  [%ames @p]
  ==
+$  lane  (each @pC address)
+$  public-key     @uwpublickey
+$  symmetric-key  @uwsymmetrickey
+$  peer-state
  $:  $:  =symmetric-key
          =life
          =rift
          =public-key
          sponsor=ship
      ==
      route=(unit [direct=? =lane])
      pit=(map path request-state)
  ==
+$  request-state
  $:  for=(set duct)
      pay=(unit path)
      ps=(unit packet-state)
  ==
+$  packet-state
  $:  los=state:verifier:lss
      fags=(list @)
  ==
::
++  parse-packet  |=(a=@ -:($:de:pact a))
++  inner-path-to-beam
  |=  [her=ship pat=(pole knot)]
  ^-  (unit [vew=view bem=beam])
  ::  /vane/care/case/desk/[spur]
  ::
  ?.  ?=([van=@ car=@ cas=@ des=@ pur=*] pat)
    ~
  ?~  cas=(de-case cas.pat)
    ~
  `[[van car]:pat [her des.pat u.cas] pur.pat]  :: XX
::
++  get-key-for  |=([=ship =life] *@)
++  get-group-key-for  |=(@ud *(unit @))
+$  binding  [=path root=@uxI]
++  crypt
  |%
  ::
  ++  sign
    |=  [sek=@uxI =binding]
    ^-  @uxJ
    (sign:ed:crypto (jam binding) sek)
  ++  verify-sig
    |=  [pub=@uxI sig=@uxJ =binding]
    ^-  ?
    (veri:ed:crypto sig (jam binding) pub)
  ::
  ++  mac
    |=  [key=@uxI =binding]
    ^-  @uxH
    =/  msg  (jam binding)
    ((keyed:blake3:blake:crypto 32^key) 16 (met 3 msg)^msg)
  ++  verify-mac
    |=  [key=@uxI tag=@uxH =binding]
    ^-  ?
    =(0 (~(dif fe 7) tag (mac key binding)))  :: XX jet for constant-time
  ::
  ++  encrypt
    |=  [key=@uxI iv=@ msg=@]
    ^+  msg
    (~(en ctrc:aes:crypto key 7 (met 3 msg) iv) msg) :: TODO: chacha8
  ++  decrypt  encrypt
  ++  encrypt-path
    |=  [key=@uxI =path]
    ^-  @
    =/  iv  0  :: XX
    (encrypt key iv (jam path))
  ++  decrypt-path
    |=  [key=@uxI cyf=@]
    ^-  path
    =/  iv  0  :: XX
    =/  pat  (decrypt key iv cyf)
    ;;(path (cue pat))
  --
--
::
|=  our=ship
=|  ax=axle
|=  [now=@da eny=@uvJ rof=roof]
|%
::
++  ev
  |_  hen=duct
  ++  ev-hear
    |=  [=lane:pact blob=@]
    ^-  [(list move) axle]
    =/  pac  (parse-packet blob)
    ?-  -.pac
        %page
      ::
      ::  decrypt path
      ::
      =/  pat
        =/  tyl=(pole knot)  pat.p.pac
        ?+  tyl  !!
          [%publ lyf=@ pat=*]  :: unencrypted
            tyl
          [%chum lyf=@ her=@ hyf=@ pat=[cyf=@ ~]]  :: encrypted with eddh key, hmac as iv
            =/  lyf  (slaw %ud lyf.tyl)
            =/  her  (slaw %p her.tyl)
            =/  hyf  (slaw %ud hyf.tyl)
            =/  cyf  (slaw %uv cyf.pat.tyl)
            ?>  &(?=(^ lyf) ?=(^ her) ?=(^ hyf) ?=(^ cyf))
            =/  key  (get-key-for u.her u.hyf)
            tyl(pat (decrypt-path:crypt key u.cyf))
          [%shut kid=@ pat=[cyf=@ ~]]  :: encrypted with group key, sig as iv
            =/  kid  (slaw %ud kid.tyl)
            =/  cyf  (slaw %uv cyf.pat.tyl)
            ?>  &(?=(^ kid) ?=(^ cyf))
            =/  key  (need (get-group-key-for u.kid)) :: XX handle ~
            tyl(pat (decrypt-path:crypt key u.cyf))
        ==
      ::
      ::  check for pending request (peek|poke)
      ::
      ?~  per=(~(get by p.ax) her.p.pac)
        [~ ax]
      ?~  res=(~(get by pit.u.per) pat)
        [~ ax]
      ::
      =/  [typ=?(%auth %data) fag=@ud]
        ?~  wan.p.pac
          [?:((gth tot.q.pac 4) %auth %data) 0]
        [typ fag]:wan.p.pac
      ::
      =/  authenticate
        |=  rut=@uxI
        ?>  ?=([%0 *] aut.q.pac)
        =*  auth  p.aut.q.pac
        =/  =beak  [her.p.pac %$ ud+1]  :: XX where do we get this?
        =/  ful  (en-beam [beak pat.p.pac])
        ?-  -.auth
          %&
            =/  pub  (puck:ed:crypto 0)  :: XX get from jael?
            (verify-sig:crypt pub p.auth ful rut)
          %|
            =/  key
              :: XX is there an easier way to get this?
              =/  tyl=(pole knot)  pat.p.pac
              ?>  ?=([%chum lyf=@ her=@ hyf=@ *] tyl)
              =/  her  (slaw %p her.tyl)
              =/  hyf  (slaw %ud hyf.tyl)
              ?>  &(?=(^ her) ?=(^ hyf))
              (get-key-for u.her u.hyf)
            (verify-mac:crypt key p.auth ful rut)
        ==
      ::
      ?-    typ
          %auth
        ?.  ?|  ?=(~ ps.u.res)
                =(0 fag)
                (gth tot.q.pac 4)
            ==
          [~ ax]
        =/  proof=(list @ux)  (rip 8 dat.q.pac)
        ?>  (authenticate (recover-root:lss proof))
        ?~  state=(init:verifier:lss tot.q.pac proof)
          [~ ax]
        =.  p.ax
          %+  ~(put by p.ax)  her.p.pac
          =-  u.per(pit -)
          %+  ~(put by pit.u.per)  pat.p.pac
          u.res(ps `[u.state ~])
        ::  request next fragment
        ::
        =/  =pact:pact  [%peek p.pac(wan [%data 0])]
        [[[[/ ~] %give %send ~ p:(fax:plot (en:^pact pact))] ~] ax]
      ::
          %data
        ?>  =(13 boq.p.pac)  :: non-standard
        ::  do we have packet state already?
        ::
        ?~  ps.u.res
          ::  no; then this should be the first fragment, and auth should be present
          ::
          ?>  =(0 fag)
          ?>  ?=([%0 *] aut.q.pac)
          ::  is this a standalone message?
          ::
          ?:  =(1 tot.q.pac)
            ?>  (authenticate (root:lss dat.q.pac))
            =/  =spar:ames  [her.p.pac pat.p.pac]
            =/  =auth:mess  p.aut.q.pac
            =/  =page  ;;(page (cue dat.q.pac)) :: XX what if we get ~ instead of a page?
            [[[[/ ~] %give %response [%page [spar auth page]]] ~] ax]
          ::  no; then the proof should be inlined; verify it
          ::  (otherwise, we should have received an %auth packet already)
          ::
          ?>  (lte tot.q.pac 4)
          =/  proof=(list @ux)
            =>  aut.q.pac
            ?>  ?=([%0 *] .)
            ?~(q ~ ?@(u.q [u.q ~] [p q ~]:u.q))
          =.  proof  [(leaf-hash:lss fag dat.q.pac) proof]
          ?>  (authenticate (recover-root:lss proof))
          ?~  state=(init:verifier:lss tot.q.pac proof)
            [~ ax]
          ?~  state=(verify-msg:verifier:lss u.state dat.q.pac ~)
            [~ ax]
          ::  initialize packet state and request next fragment
          ::
          =.  p.ax
            %+  ~(put by p.ax)  her.p.pac
            =-  u.per(pit -)
            %+  ~(put by pit.u.per)  pat.p.pac
            u.res(ps `[u.state ~[dat.q.pac]])
          =/  =pact:pact  [%peek p.pac(wan [%data leaf.u.state])]
          [[[[/ ~] %give %send ~ p:(fax:plot (en:^pact pact))] ~] ax]
        ::  yes, we do have packet state already
        ::
        =*  ps  u.ps.u.res
        ?.  =(leaf.los.ps fag)
          [~ ax]
        ::  extract the pair (if present) and verify
        ::
        =/  pair=(unit [l=@ux r=@ux])
          ?~  aut.q.pac  ~
          `?>(?=([%1 *] .) p):aut.q.pac
        ?~  state=(verify-msg:verifier:lss los.ps dat.q.pac pair)
          [~ ax]
        ::  update packet state
        ::
        =.  los.ps  u.state
        =.  fags.ps  [dat.q.pac fags.ps]
        =.  p.ax
          %+  ~(put by p.ax)  her.p.pac
          =-  u.per(pit -)
          %+  ~(put by pit.u.per)  pat.p.pac
          u.res
        ::  is the message incomplete?
        ::
        ?.  =(+(fag) leaves.los.ps)
          ::  request next fragment
          ::
          =/  =pact:pact  [%peek p.pac(wan [%data leaf.u.state])]
          [[[[/ ~] %give %send ~ p:(fax:plot (en:^pact pact))] ~] ax]
        ::  yield complete message
        ::
        =/  =spar:ames  [her.p.pac pat.p.pac]
        =/  =auth:mess  [%| *@uxH] :: XX should be stored in ps?
        =/  =page  ;;(page (cue (rep 13 (flop fags.ps))))
        [[[[/ ~] %give %response [%page [spar auth page]]] ~] ax]
      ==
    ::
        %peek
      ?.  =(our her.p.pac)
        [~ ax]
      =/  res=(unit (unit cage))  (scry ~ /ames %x (name-to-beam p.pac))
      ?.  ?=([~ ~ ^] res)
        [~ ax]
      [[[[/ ~] %give %send ~ !<(@ q.u.u.res)] ~] ax]
    ::
        %poke
      ::  XX dispatch/hairpin &c
      ::
      ::  - pre-check that we want to process this poke (recognize ack path, ship not blacklisted, &c)
      ::  - initialize our own outbound request for the poke payload
      ::  - start processing the part of the poke payload we already have
      ::    - validation should crash event or ensure that no state is changed
      !!
    ==
  ::
  ++  ev-mess
    |=  [(unit lane:pact) =mess]
    ^-  [(list move) axle]
    ?-  -.mess
        %page
      ?~  per=(~(get by p.ax) ship.p.mess)
        [~ ax]
      ?~  res=(~(get by pit.u.per) path.p.mess)
        [~ ax]
      ::
      ::  XX validate response
      ::  XX give to all ducts in [for.u.res]
      ::
      ::  [%give %response mess]
      ::
      [~ ax(p (~(put by p.ax) ship.p.mess u.per(pit (~(del by pit.u.per) path.p.mess))))]
    ::
        %peek
      ?.  =(our ship.p.mess)
        [~ ax]
      =/  res=(unit (unit cage))
        !!  :: scry for path
      ?.  ?=([~ ~ ^] res)
        [~ ax]
      ::  XX [%give %response %page p.mess [p q.q]:u.u.res]
      [~ ax]
    ::
        %poke
      ::  XX dispatch/hairpin &c
      ::
      ::  - check that we recognize ack-path
      ::  - validate inner payload message
      ::  - route message to inner module (ie, flow)
      ::
      !!
    ==
  ::
  ++  ev-make-mess
    |=  [p=spar:ames q=(unit path)]
    =/  per  (~(gut by p.ax) ship.p *peer-state)  :: XX alien-agenda
    ?^  res=(~(get by pit.per) path.p)
      ?>  =(q pay.u.res)  ::  prevent overriding payload
      =-  [~ ax(p -)]
      %+  ~(put by p.ax)  ship.p
      =-  per(pit -)
      %+  ~(put by pit.per)  path.p
      u.res(for (~(put in for.u.res) hen))
    ::
    ::  XX resolve payload path if present to validate
    ::
    ?:  ?&  ?=(^ q)
            =/  res  *(unit (unit cage))
              :: (rof ~ /ames/foo [[our ...] u.q])
            !?=([~ ~ %message *] res)
        ==
      !! :: XX wat do?
    ::
    =|  new=request-state
    =.  for.new  (~(put in for.new) hen)
    =.  pay.new  q
    =.  p.ax  (~(put by p.ax) ship.p per(pit (~(put by pit.per) path.p new)))
    ::
    ::  XX construct and emit initial request packet
    ::
    =/  =pact:pact
      =/  pat=path
        =/  tyl=(pole knot)  path.p
        ?+  tyl  !!
          [%publ lyf=@ pat=*]  :: unencrypted
            tyl
          [%chum lyf=@ her=@ hyf=@ pat=*]  :: encrypted with eddh key, hmac as iv
            =/  key  (get-key-for her.tyl hyf.tyl)
            =/  cyf  (encrypt-path:crypt key pat.tyl)
            tyl(pat ~[(scot %uv cyf)])
          [%shut kid=@ pat=*]  :: encrypted with group key, sig as iv
            =/  key  (need (get-group-key-for kid.tyl)) :: XX handle ~
            =/  cyf  (encrypt-path:crypt key pat.tyl)
            tyl(pat ~[(scot %uv cyf)])
        ==
      =/  nam
        [[ship.p *rift] [13 ~] pat] :: XX rift from peer-state
      ?~  q
        [%peek nam]
      ::  XX if path will be too long, put in [tmp] and use that path
      ::  =/  has  (shax u.u.res)
      ::  =.  tmp.ax  (~(put by tmp.ax) has [%some-envelope original-path u.u.res])
      ::  //ax/[$ship]//1/temp/[hash]
      =/  man
        [[our *rift] [13 ~] u.q]      :: XX our rift
      [%poke nam man *data:pact]  :: XX resolve /init
    ::
    [[[[/ ~] %give %send ~ p:(fax:plot (en:^pact pact))] ~] ax]
  ::
  ++  ev-make-peek
    |=  p=spar:ames
    (ev-make-mess p ~)
  ::
  ++  ev-make-poke
    |=  [p=spar:ames q=path]
    (ev-make-mess p `q)
  --
::
++  call
  |=  [hen=duct dud=(unit goof) wrapped-task=(hobo task)]
  ^-  [(list move) _..^$]
  ::
  =/  task=task  ((harden task) wrapped-task)
  ?<  ?=(^ dud)
  =^  mov  ax
    ?-  -.task
      %hear       (~(ev-hear ev hen) p.task q.task)
      %mess       (~(ev-mess ev hen) p.task q.task)
      %make-peek  (~(ev-make-peek ev hen) p.task)
      %make-poke  (~(ev-make-poke ev hen) p.task q.task)
    ==
  [mov ..^$]
::
++  take
  |=  [=wire =duct dud=(unit goof) =sign]
  ^-  [(list move) _..^$]
  ?<  ?=(^ dud)
  !!
::
++  load
  |=  old=axle
  ^+  ..^$
  ..^$(ax old)
::
++  stay  `axle`ax
::
++  scry
  ^-  roon
  |=  [lyc=gang pov=path car=term bem=beam]
  ^-  (unit (unit cage))
  ?:  ?&  =(our p.bem)
          =(%$ q.bem)
          =([%ud 1] r.bem)
          =(%x car)
      ==
    =/  tyl=(pole knot)  s.bem
    ?+    tyl  ~
    ::
    ::  publisher-side, protocol-level
    ::
        [%mess ryf=@ res=*]
      =/  ryf  (slaw %ud ryf.tyl)
      ?~  ryf  [~ ~]
      ?.  =(*rift u.ryf)      :: XX our rift, XX unauthenticated
        ~
      =*  rif  u.ryf
      =/  nex
        ^-  $@  ~
            $:  pat=path
                $=  pac       ::  XX control packet serialization
                $@  ~
                $:  boq=bloq
                    ser=?
                    wan=$@(~ [typ=?(%auth %data) fag=@ud])
            ==  ==
        ?+    res.tyl  ~
            [%$ pat=*]  [pat.res.tyl ~]
        ::
            [%pact boq=@ ser=?(%etch %pure) %init pat=*]
          ?~  boq=(slaw %ud boq.res.tyl)
            ~
          [pat.res.tyl u.boq ?=(%etch ser.res.tyl) ~]
        ::
            [%pact boq=@ ser=?(%etch %pure) typ=?(%auth %data) fag=@ pat=*]
          =/  boq  (slaw %ud boq.res.tyl)
          =/  fag  (slaw %ud fag.res.tyl)
          ?:  |(?=(~ boq) ?=(~ fag))
            ~
          [pat.res.tyl u.boq ?=(%etch ser.res.tyl) typ.res.tyl u.fag]
        ==
      ::
      ?~  nex
        [~ ~]
      =*  pat  pat.nex
      =/  res  $(lyc ~, pov /ames/mess, s.bem pat)
      ?.  ?&  ?=([~ ~ %message *] res)
        :: ...validate that it's really a message
        :: =>  [%message tag=?(sig hmac) ser=@]
          ==
        ~
      ?~  pac.nex  res
      ::
      ::  packets
      ::
      =*  boq  boq.pac.nex
      ?.  ?=(%13 boq)
        ~ :: non-standard fragments for later
      =/  msg  ;;([typ=?(%sign %hmac) aut=@ ser=@] q.q.u.u.res)  :: XX types
      =/  mes=auth:mess  ?:(?=(%sign typ.msg) &+aut.msg |+aut.msg)
      =*  ser  ser.msg
      =/  wid  (met boq ser)
      ?<  ?=(%0 wid)  :: XX is this true?
      =/  nit=?  |    :: XX refactor
      |-  ^-  (unit (unit cage))
      ?~  wan.pac.nex
        $(nit &, wan.pac.nex [?:((gth wid 4) %auth %data) 0])
      ::
      =*  fag  fag.wan.pac.nex
      ?.  (gth wid fag)
        [~ ~]
      ?:  ?&  ?=(%auth typ.wan.pac.nex)
              !=(0 fag)
          ==
        ~  :: non-standard proofs for later
      =;  [nam=name:pact dat=data:pact]
        =/  pac=pact:pact  [%page nam dat ~]
        ?.  ser.pac.nex
          ``[%packet !>(pac)]
        ``[%atom !>(p:(fax:plot (en:pact pac)))]
      ::
      ?-    typ.wan.pac.nex
          %auth
        =/  nam  [[our rif] [boq ?:(nit ~ [%auth fag])] pat]
        ::  NB: root excluded as it can be recalculated by the client
        ::
        =/  aut  [%0 mes ~]
        =/  lss-proof  (build:lss (met 3 ser)^ser) ::  XX cache this
        =/  dat  [wid aut (rep 8 proof.lss-proof)]  :: XX types
        [nam dat]
      ::
          %data
        =/  lss-proof  (build:lss (met 3 ser)^ser)  :: XX cache this
        =/  nam  [[our rif] [boq ?:(nit ~ [%data fag])] pat]
        =/  aut=auth:pact
          ?:  &((lte wid 4) =(0 fag))
            :: inline (or absent) proof
            ::
            :+  %0  mes
            ?:  =(1 wid)  ~
            =/  tal  (tail proof.lss-proof)
            ?:  ?=(?(%1 %2) wid)
              ?>  ?=([* ~] tal)
              `i.tal
            ?>  ?=([* * ~] tal)
            `[i i.t]:tal
          ::
          ::  full proof; provide a pair of sibling hashes
          ::
          ?:  (gte fag (lent pairs.lss-proof))  ~
          [%1 (snag fag pairs.lss-proof)]
        ::
        =/  dat  [wid aut (cut boq [fag 1] ser)]
        [nam dat]
      ==
    ::
    ::  XX need a single namespace entrypoint to validate
    ::     generically any authentication tag for a message
    ::
    ::    /ax/[$ship]//1/validate-message/[auth-string]/[blake3-hash]/[path]
    ::
    ::
    ::  publisher-side, message-level
    ::
        [%publ lyf=@ pat=*]
      =/  lyf  (slaw %ud lyf.tyl)
      ?~  lyf  [~ ~]
      ?.  =(u.lyf *life) :: XX our life
        ~
      ?~  inn=(inner-path-to-beam our pat.tyl)
        [~ ~]
      ?~  res=(rof ~ /ames/publ vew.u.inn bem.u.inn)
        ~
      =/  gag  ?~(u.res ~ [p q.q]:u.u.res)  :: XX how does receiver distinguish these?
      =/  ful  (en-beam bem)
      =/  ryf  *rift :: XX our rift
      =|  sec=@uxI :: XX derive from rift??
      =/  ser  (jam gag)
      ``[%message !>([%sign (sign:crypt sec ful (root:lss ser)) ser])]
    ::
        [%chum lyf=@ her=@ hyf=@ cyf=@ ~]
      =/  lyf  (slaw %ud lyf.tyl)
      =/  her  (slaw %p her.tyl)
      =/  hyf  (slaw %ud hyf.tyl)
      =/  cyf  (slaw %uv cyf.tyl)
      ?:  |(?=(~ lyf) ?=(~ her) ?=(~ hyf) ?=(~ cyf))
        [~ ~]
      ?.  =(u.lyf *life) :: XX our life
        ~
      =/  key  (get-key-for u.her u.hyf)  :: eddh with our key
      =/  pat  (decrypt-path:crypt key u.cyf)
      ?~  inn=(inner-path-to-beam our pat)  ~
      ?~  res=(rof `[u.her ~ ~] /ames/chum vew.u.inn bem.u.inn)
        ~
      =/  gag  ?~(u.res ~ [p q.q]:u.u.res)
      =/  ful  (en-beam bem)
      =/  ser  (jam gag)
      ``[%message !>([%hmac (mac:crypt key ful (root:lss ser)) ser])]
    ::
        [%shut kid=@ cyf=@ ~]
      =/  kid  (slaw %ud kid.tyl)
      =/  cyf  (slaw %uv cyf.tyl)
      ?:  |(?=(~ kid) ?=(~ cyf))
        [~ ~]
      ?~  key=(get-group-key-for u.kid)
        ~
      =/  pat  (decrypt-path:crypt u.key u.cyf)
      ::  XX check path prefix
      ?~  inn=(inner-path-to-beam our pat)
        ~
      ?~  res=(rof [~ ~] /ames/shut vew.u.inn bem.u.inn)
        ~
      =/  gag  ?~(u.res ~ [p q.q]:u.u.res)
      =/  ful  (en-beam bem)
      =/  ser  (jam gag)
      ``[%message !>([%sign (sign:crypt u.key ful (root:lss ser)) ser])]
    ==
  ~
--
