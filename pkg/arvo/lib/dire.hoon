!:
|%
+$  plat
  $@  @                                       ::  measure atom
  $^  $%  [[%c ~] (pair (pair step step) @)]  ::  cut slice
          [[%m ~] (pair (pair step step) @)]  ::  measure slice
          [[%s ~] p=plot]                     ::  subslice
      ==                                      ::
  (pair step @)                               ::  prefix
+$  plot  $^  [l=plot r=plot]                 ::  concatenate
          [a=bloq b=(list plat)]              ::  serialize
--
::
|%
::  +pet: prefix +met
::
++  pet  |=([a=bloq b=@] `(pair step @)`[(met a b) b])
::
::  +rig: convert between bloqs
::
++  rig
  |=  [[a=bloq b=bloq] c=step]
  ^-  step
  ?:  =(a b)  c
  ?:  (gth a b)
    (lsh [0 (sub a b)] c)
  =/  d  [0 (sub b a)]
  =/  e  (rsh d c)
  ?:(=(0 (end d c)) e +(e))
::
::  +fax: encode a plot
::
++  fax
  :: ~/  %fax
  |=  p=plot
  ^-  (pair step @)
  =<  +
  |-  ^-  (trel bloq step @)
  ?^  -.p
    =/  l  $(p l.p)
    =/  r  $(p r.p)
    =/  s  (rig [p.l p.r] q.l)
    [p.r (add q.r s) (add r.l (lsh [p.r s] r.r))]
  ::
  ?~  b.p  [a.p 0 0]
  =;  c=(pair step @)
    =/  d  ^$(b.p t.b.p)
    [a.p (add p.c p.d) (add q.c (lsh [a.p p.c] q.d))]
  ::
  ?@  i.b.p  (pet a.p i.b.p)
  ?-  -.i.b.p
    @       [p.i.b.p (end [a.p p.i.b.p] q.i.b.p)]
    [%c ~]  [q.p.i.b.p (cut a.p [p q]:i.b.p)]
    [%m ~]  (pet a.p (cut a.p [p q]:i.b.p))
    [%s ~]  =/  e  $(p p.i.b.p)
            [(rig [p.e a.p] q.e) r.e]
  ==
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
++  popcount
  |=  a=@
  ?:  =(0 a)  0
  =|  n=@ud
  =/  m  (dec (met 0 a))
  |-  ^-  @ud
  =?  n  =(1 (cut 0 [m 1] a))
    +(n)
  ?:(=(0 m) n $(m (dec m)))
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
::    > :(add 4 340 1.128)
::    1.472
::
++  pact
  =>  |%
      +$  name  [p=ship q=path r=bloq s=num=@ud]
      +$  data  [tot=@ud aut=@ux dat=@]
      +$  pact  $%  [%page p=name q=data r=nex=@ux]  ::  nex=(unit (each [@if @udE] (list [@if @udE])))
                    [%peek p=name]
                    [%poke p=name q=name r=data]
                ==
      --
  ::
  |%
  ++  en
    |=  pak=pact
    ^-  plot
    =/  bod=plot
      ?-  -.pak
        %page  :: XX nex
               ::
               [(en:^name p.pak) (en:^data q.pak)]
      ::
        %peek  (en:^name p.pak)
        %poke  [(en:^name p.pak) (en:^name q.pak) (en:^data r.pak)]
      ==
    =/  hed=plot
      ::  XX nex: ?.(?=(%page -.pak) 0 r.pak)
      ::
      (en:head 0 -.pak 0 (mug (fax b)))
    [hed bod]
  ::
  ++  de
    |=  a=bite
    =/  b=[bloq step]  [0 ?@(a 0 (rig [bloq.a 0] step.a))]
    |=  dat=@
    ^-  [pact bloq step]
    =^  hed  b  ((de:head b) dat)
    ?-  typ.hed
      %page  =^  nam  b  ((de:^name b) dat)
             =^  dat  b  ((de:^data b) dat)
             ::  XX nex
             [[typ.hed nam dat *@ux] b]
    ::
      %peek  =^  nam  b  ((de:^name b) dat)
            [[typ.hed nam] b]
    ::
      %poke  =^  nam  b  ((de:^name b) dat)
             =^  dam  b  ((de:^name b) dat)
             =^  dat  b  ((de:^data b) dat)
             [[typ.hed nam dam dat] b]
    ==
  --
::
++  head
  |%
  ++  en
    |=  [nex=@B typ=?(%page %peek %poke) hop=@ gum=@F]
    ^-  plot
    =/  tip  ?-(typ %page 0b1, %peek 0b10, %poke 0b11)
    :-  bloq=0
    [[2 0] [2 nex] [3 ver=1] [2 tip] [3 (max 7 hop)] [20 gum] ~]
  ::
  ++  de
    |=  a=bite
    =/  b=[bloq step]  [0 ?@(a 0 (rig [bloq.a 0] step.a))]
    |=  dat=@
    ^-  [[nex=@B typ=?(%page %peek %poke) hop=@ gum=@F] bloq step]
    =+  [[res nex ver tip hop gum] b]=((hew b dat) [2 2 3 2 3 20])
    ?>  =(0 res)
    ?>  =(1 ver)
    =/  typ  ?+(tip !! %0b1 %page, %0b10 %peek, %0b11 %poke)
    [[nex typ hop gum] b]
  --
::
::  +name: encoded-path
::
::  range:  { meta[1], her[2^1-4], typ[2^0-1], pat[2^0-16], boq[0-1], fag[1-4] }
::  max:    { meta[1], her[16],    typ[2],     pat[65.536], boq[1],   fag[4]   }
::          { meta-byte, address,  path-length, path,       bloq-size, fragment-number }
::  actual: { meta[1], her[16],    typ[2],     pat[317],    boq[1],   fag[4]   }
::
::    > :(add 1 16 2 317 4)
::    340
::
++  name
  |%
  ++  manual
    |=  [her=@pH pat=path boq=@D num=@udF]
    =/  ran  ?~(her 0 (dec (met 0 (met 4 (end 7 her)))))
    =/  tap  =-([p=(met 3 -) q=-] `@t`(rap 3 (join '/' pat)))
    ?>  (lth p.tap ^~((bex 16)))
    =/  typ  (dec (met 3 p.tap))
    =/  loq  ?:(=(13 boq) 0 1)
    =/  fag  =-([p=(met 3 -) q=-] (end 5 num))
    =/  byt  (can 0 [2 0] [2 ran] [1 typ] [1 loq] [2 p.fag] ~)
    (can 3 [1 byt] [(bex +(ran)) her] [+(typ) p.tap] tap [loq (end 3 boq)] fag ~)
  ::
  ++  en
    |=  [her=@pH pat=path boq=@D num=@udF]
    ^-  plot
    =/  ran  ?~(her 0 (dec (met 0 (met 4 (end 7 her)))))
    =/  tap  =-([p=(met 3 -) q=-] `@t`(rap 3 (join '/' pat)))
    ?>  (lth p.tap ^~((bex 16)))
    =/  typ  (dec (met 3 p.tap))
    =/  loq  ?:(=(13 boq) 0 1)
    =/  fag  =-([p=(met 3 -) q=-] (end 5 num))
    :+  bloq=3
      [s+~ 0 [2 0] [2 ran] [1 typ] [1 loq] [2 p.fag] ~]
    [[(bex +(ran)) her] [+(typ) p.tap] tap [loq (end 3 boq)] fag ~]
  ::
  ++  len
    |=  pat=@
    ^-  @ud
    =+  [res ran typ loq fag]=-:((hew 0 pat) [2 2 1 1 2])
    ?>  =(0 res)
    =/  len  +((bex +(ran)))
    =/  nex  +(typ)
    =/  tap  (cut 3 [len nex] pat)
    :(add len nex tap loq fag)
  ::
  ++  de
    |=  a=bite
    =/  b=[bloq step]  [0 ?@(a 0 (rig [bloq.a 0] step.a))]
    |=  pat=@
    ^-  [[her=@p pat=path boq=bloq num=@udF] bloq step]
    =+  [[res ran typ loq fag] b]=((hew b pat) [2 2 1 1 2])
    ?>  =(0 res)
    =+  [len nex]=[(rig [bloq.b 3] step.b) (bex +(ran))]
    =/  her  (cut 3 [len nex] pat)
    =:  len  (add len nex)
        nex  +(typ)
      ==
    =/  tap  (cut 3 [len nex] pat)
    =:  len  (add len nex)
        nex  tap
      ==
    =/  tap
      %+  rash  (cut 3 [len nex] pat)
      (more fas (cook crip (star ;~(less fas prn))))
    =:  len  (add len nex)
        nex  loq
      ==
    =/  boq  ?~(loq 13 (cut 3 [len nex] pat))
    =:  len  (add len nex)
        nex  fag
      ==
    [[her tap boq (cut 3 [len nex] pat)] 3 (add len nex)]
  --
::
::  +data: response data
::
::  range:  { meta[1], tot[2^0-3], lut[0-1], aut[0-255], len[0-32], dat[0-2^2^32] }
::  max:    { meta[1], tot[8],     lut[1],   aut[255],   len[32],   dat[2^2^32]   }
::  actual: { meta[1], tot[4],     lut[1],   aut[96],    len[2],    dat[0-1024]   }
::
::    > :(add 1 4 1 96 2 1.024)
::    1.128
::
++  data
  |%
  ++  manual
    |=  [tot=@ud aut=@ux dat=@]
    =/  mot  (dec (met 0 (met 3 (end 6 tot))))
    =/  mut  ?:(=(0 aut) 0 1)
    =/  lut  (end 3 (met 3 aut))
    =/  len  (met 3 dat)
    =/  men  (met 3 len)
    =/  byt
      (can 0 [2 mot] [1 mut] [5 men] ~)
    (can 3 [1 byt] [(bex mot) tot] [mut lut] [lut aut] [men len] [len dat] ~)
  ::
  ++  en
    |=  [tot=@ud aut=@ux dat=@]
    ^-  plot
    =/  mot  (dec (met 0 (met 3 (end 6 tot))))
    =/  mut  ?:(=(0 aut) 0 1)
    =/  lut  (end 3 (met 3 aut))
    =/  len  (met 3 dat)
    =/  men  (met 3 len)
    :+  bloq=3
      [s+~ 0 [2 mot] [1 mut] [5 men] ~]
    [[(bex mot) tot] [mut lut] [lut aut] [men len] [len dat] ~]
  ::
  ++  len
    |=  dat=@
    ^-  @ud
    =+  [mot mut men]=-:((hew 0 dat) [2 1 5])
    =/  tob  +((bex mot))
    =+  [len nex]=[tob mut]
    =/  lut  (cut 3 [len nex] dat)
    =:  len  :(add len nex lut)
        nex  men
      ==
    =/  lat  (cut 3 [len nex] dat)
    :(add tob mut lut men lat)
  ::
  ++  de
    |=  a=bite
    =/  b=[bloq step]  [0 ?@(a 0 (rig [bloq.a 0] step.a))]
    |=  dat=@
    ^-  [[tot=@ud aut=@ux dat=@] bloq step]
    =+  ^=  [[mot mut men] b]  ((hew b dat) [2 1 5])
    =+  ^=  [len nex]          [(rig [bloq.b 3] step.b) (bex mot)]
    =/  tot  (cut 3 [len nex] dat)
    =:  len  (add len nex)
        nex  mut
      ==
    =/  lut  (cut 3 [len nex] dat)
    =:  len  (add len nex)
        nex  lut
      ==
    =/  aut  (cut 3 [len nex] dat)
    =:  len  (add len nex)
        nex  men
      ==
    =/  lat  (cut 3 [len nex] dat)
    =:  len  (add len nex)
        nex  lat
      ==
    [[tot aut (cut 3 [len nex] dat)] 3 (add len nex)]
  --
--
