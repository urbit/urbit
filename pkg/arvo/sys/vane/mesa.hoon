!:
=,  mesa
=/  packet-size  13
::  %dire helpers   ::  XX move to lull.hoon
::
=>  |%
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
          +$  auth  (each @uxJ @uxI) :: &+sig, |+hmac
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
    :: > :(add 8 305 1.159)
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
    --
::  helper core
::
=>  |%
    +|  %helpers
    ::
    ++  chain
      =<  mop
      |%
      ++  on   ((^on ,@ ,[key=@ =path]) lte)
      +$  mop  chain:ames
      --
    ::
    ++  parse-inner-path
      |=  [our=ship p=path]
      ^-  (unit [[@tas @tas] beam])
      ?.  ?=([@ @ @ @ *] p)  ~
      ?~  des=?~(i.t.t.p (some %$) (slaw %tas i.t.t.p))
        ~
      ?~  ved=(de-case i.t.t.t.p)  ~
      `[[i.p i.t.p] [[our u.des u.ved] t.t.t.t.p]]
    ::
    ++  decrypt
      |=  [p=@t key=@]
      ^-  (unit path)
      (rush `@t`(dy:crub:crypto key (slav %uv p)) stap)
    ::
    ++  check-fine-key
      |=  [c=chain:ames =balk key-idx=@]
      ^-  ?
      ?~  link=(get:on:chain c key-idx)
        |
      =/  gol  path.u.link
      =/  =path  [van.balk car.balk spr.balk]
      |-  ^-  ?
      ?~  gol   &
      ?~  path  |
      ?.  =(i.path i.gol)
        |
      $(path t.path, gol t.gol)
    ::
    ++  derive-symmetric-key
      ~/  %derive-symmetric-key
      |=  [public-key=@uw private-key=@uw]
      ^-  symmetric-key
      ::
      ?>  =('b' (end 3 public-key))
      =.  public-key  (rsh 8 (rsh 3 public-key))
      ::
      ?>  =('B' (end 3 private-key))
      =.  private-key  (rsh 8 (rsh 3 private-key))
      ::
      `@`(shar:ed:crypto public-key private-key)
        ::
    ::
    ++  parse-packet  |=(a=@ -:($:de:pact a))
    ++  is-auth-packet  |
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
    ++  parse-path         |=(@ *(unit path))
    ++  blake3             |=(* *@)
    ++  get-key-for        |=  [=ship =life]  *@
    ++  get-group-key-for  |=(@ud *(unit @))
    ++  crypt
      |%
      ++  sign     |=(* *@)
      ++  verify   |=(* *?)
      ++  hmac     |=(* *@)
      ++  encrypt  |=(@ @)
      ++  decrypt  |=(@ *(unit @))
      --
    ::
    ++  jim  |=(n=* ~>(%memo./mesa/jam (jam n)))
    --
::  vane types
::
=>  |%
    +|  %types
    ::
    +$  veb-all-off
      $:  snd=_`?`%.n  ::  sending packets
          rcv=_`?`%.n  ::  receiving packets
          odd=_`?`%.n  ::  unusual events
          msg=_`?`%.n  ::  message-level events
          ges=_`?`%.n  ::  congestion control
          for=_`?`%.n  ::  packet forwarding
          rot=_`?`%.n  ::  routing attempts
          kay=_`?`%.n  ::  is ok/not responding
          fin=_`?`%.n  ::  remote-scry
      ==
    +$  bug  [veb=veb-all-off ships=(set ship)]
    ::  $channel: combined sender and receiver identifying data
    ::
    +$  channel
      $:  [our=ship her=ship]
          now=@da
          ::  our data, common to all dyads
          ::
          $:  =our=life
              crypto-core=acru:ames
              =bug
          ==
          ::  her data, specific to this dyad
          ::
          $:  =symmetric-key
              =her=life
              =her=rift
              =her=public-key
              her-sponsor=ship
      ==  ==
    ::
    +$  peer-channel  [=channel =peer-state]
    ::
    +$  lane  $@  @ux
              $%  [%if p=@ifF q=@udE]
                  [%is p=@isH q=@udE]
              ==
    ::  $move: output effect; either request (to other vane) or response
    ::
    +$  move  [=duct card=(wite note gift)]
    ::  $note: request to other vane
    ::
    ::    Ames passes a %plea note to another vane when it receives a
    ::    message on a "forward flow" from a peer, originally passed from
    ::    one of the peer's vanes to the peer's Ames.
    ::
    ::    Ames passes a %deep task to itself to handle deferred calls
    ::    Ames passes a %private-keys to Jael to request our private keys.
    ::    Ames passes a %public-keys to Jael to request a peer's public keys.
    ::
    +$  note
      $~  [%b %wait *@da]
      $%  $:  %m
              $>(?(%make-peek %make-poke) task:mesa)
          ==
          $:  %b
              $>(?(%wait %rest) task:behn)
          ==
          $:  %c
              $>(%warp task:clay)
          ==
          $:  %d
              $>(%flog task:dill)
          ==
          $:  %g
              $>(%deal task:gall)
          ==
          $:  %j
              $>  $?  %private-keys
                      %public-keys
                      %turf
                      %ruin
                  ==
              task:jael
          ==
          $:  @tas
              $>(%plea vane-task)
      ==  ==
    ::
    ::  to %lull
    +$  sign
      $%  ::[%ames %response $>(%page mess)]   :: produce a response message
          [%mesa gift]
          sign-arvo
      ==
    ::
    +$  peer-task  ,*        ::  XX fill out
    ::
    +$  axle
      $:  peers=(map ship ship-state)
          =unix=duct
          =life
          =rift
          crypto-core=acru:ames
          =bug
          snub=[form=?(%allow %deny) ships=(set ship)]
          =chain
        ==
    --
::
::  vane gate
::
|=  our=ship
=|  ax=axle
::
|=  [now=@da eny=@uvJ rof=roof]
=*  mesa-gate  .
=>  ::  inner event-handling
    ::
    ::  XX flopping is only done in the ev-req/ev-res cores
    ::  XX have moves at to top-level core, with emit/emil helpers
    ::
    ::  poke/flow layer spec
    ::
    ::  - incoming-tasks:
        :: client :  send plea    =>  recv plea  : server
        ::          recv (n)ack   <=  send (n)ack
        ::
        ::           recv boon  ?(<=) send boon
    ::
    ::  "send plea/boon: both go into the sender-pump queue,
    ::  and bind their payload into the namespaces to be read,
    ::
    ::  every `=>` delivers a full message to the packet layer if the
    ::  sender window allows it
    ::  otherwise a timer will try again,
    ::
    ::  both client and server send and receive %pokes
    ::    client sends only %poke %plea  to %clay/%gall/%jael
    ::    server sends only %poke %boons to %clay/%gall/%jael
    ::
    ::  XX we need to track data for both sending %pokes and receiving them
    ::  (sequence numbers in the receiver enforce exactly-one message delivery),
    ::
    ::      ~zod (sends-plea to ~nec)                           to-vane
    ::    ----------                                               |
    ::   [%plea task]   ^  [%make-poke task] (1 packet)    +fo-core (%sink)
    ::        |         |         |                                |
    ::    +ev-req       | [%peek for-ack]  [%send %poke]        +pe-core
    ::        |         |         |                                |
    ::    +pe-core      |         |                            +ma:ev-res
    ::        |         |         |                                |
    ::  call:fo-core    |         |                           [%mess %poke]
    ::     (%done)      |         |                                 |
    ::        |_________|         |________________________________| unix
    ::                                                      ------------
    ::                                                          ~nec (gets %poke plea)
    ::
    ::          ~nec
    ::        ----------               |--------------              ^
    ::       [%done err=~]      give %response       |              |
    ::            |                    |             |           to-vane
    ::        +ev-res               +ma-page         |              |
    ::            |                    |             | +take-ack:fo-core(%response)
    ::        +pe-core             +ma:ev-res        |              |
    ::            |                    |             |       +take:ma:ev-res
    :: +take:fo-core(%done)            |             |              |
    ::            |                [%mess %page]     |     /[wire-of-ack]/%int
    ::         emit ack (unix)         | unix        |              |
    ::                                ~zod           |_______[%response =page]
    ::
    ::
    =|  moves=(list move)
    =|  per=[=ship sat=peer-state]    ::  XX %alien?
    ::
    |_  hen=duct
    ::
    +|  %helpers
    ::
    ++  ev-core  .
    ++  ev-abet  moves^ax
    ++  ev-emit  |=(=move ev-core(moves [move moves]))
    ++  ev-emil  |=(mos=(list move) ev-core(moves (weld mos moves)))
    :: ++  ev-abort    ev-core  :: keeps moves, discards state changes
    ++  ev-chan  [[our ship.per] now [life crypto-core bug]:ax -.sat.per]
    ::
    +|  %flow-wires
    ::
    +$  ev-flow-wire
      $:  %flow
          were=?(%out %ext %int)
          dire=?(%for %bak)
          [%p her=@p]
          [%ud rift=@ud]
          [%ud bone=@ud]
          [%ud seq=@ud]
          ~
      ==
    ::
    ++  ev-pave
      |=  =path
      ^-  pith
      %+  turn  path
      |=  i=@ta
      (fall (rush i spot:stip) i)
    ::
    +|  %top-level-paths
    ::
    ::  /ax/~ship//ver=1/mess/rift=1//[...]
    ::
    ::  /ax/~ship//ver=1/mess/rift=1/pact/bloq=13/ init/[...]
    ::
    ::  /ax/~ship//ver=1/mess/rift=1/pact/bloq=13/pure/auth/frag=1/[...]
    ::  /ax/~ship//ver=1/mess/rift=1/pact/bloq=13/pure/data/frag=1/[...]
    ::
    +$  res-scry-head  [%ax [%p her=@p] %'1' res=*]
    +$  res-mess-head  [%mess [%ud ryf=@ud] res=*]
    +$  res-pact-head  [%pact [%ud boq=@ud] ser=?(%etch %pure) pat=*]
    +$  res-pure-pith  [typ=?(%auth %data) [%ud fag=@ud] pat=*]
    ::
    +|  %namespace-paths
    ::
    ::  /[..]/publ/life=1/[...]
    ::  /[..]/chum/life=1/her=@p/hyf=@ud/encrypted-path=@uv
    ::  /[..]/shut/key=1/encrypted-path=@uv
    ::
    +$  publ-pith  [%publ [%ud lyf=@ud] pat=*]
    +$  chum-pith  [%chum [%ud lyf=@ud] [%p her=@p] [%ud hyf=@ud] [%uv cyf=@uv] ~]
    +$  shut-pith  [%shut [%ud kid=@ud] [%uv cyf=@uv] ~]
    ::
    +|  %message-flow-paths
    ::
    +$  res-mess-pith
      $:  %flow
          [%ud bone=@ud]
          [%p sndr=@p]
          load=?(%poke %ack %nax)
          [%p rcvr=@p]
          dire=?(%for %bak)
          [%ud mess=@ud]
          ~
      ==
    ::
    ++  ev-validate-wire
      |=  [hen=duct =wire]
      ^-  [(unit ev-flow-wire) _ev-core]
      =>  .(wire `(pole iota)`(ev-pave wire))
      ?.   ?=(ev-flow-wire wire)
        ~>  %slog.0^leaf/"mesa: malformed wire: {(spud (pout wire))}"  `ev-core
      :_  (ev-got-per hen her.wire)
      ?:  (lth rift.wire rift.sat.per)
        ~  ::  ignore events from an old rift
      `wire
    ::
    +|  %entry-points
    ::
    ++  ev-call
      =>  |%  +$  req-task
                $%  [%hear lane blob=@]
                    [%mess (unit lane) =mess dud=(unit goof)]
                    $>(?(%plea %keen %cork) task)
                ==
          --
      |=  task=req-task
      ^+  ev-core
      ?-  -.task
      ::  %request-entry-points
        %plea  (ev-req-plea [ship plea]:task)
        %keen  (ev-req-peek +>.task)  ::  XX sec
        %cork  (ev-req-plea ship.task %$ /cork %cork ~)
      ::  %packet-response-entry-point
          %hear
        =/  =pact:pact  (parse-packet blob.task)
        ?-  -.pact
          %page  (ev-pact-page +.pact)
          %peek  (ev-pact-peek +.pact)
          %poke  (ev-pact-poke +.pact)
        ==
      ::  %message-response-entry-point
          %mess
        ?-  -.mess.task
          %page  (ev-mess-page +.mess.task)
          %peek  (ev-mess-peek +.mess.task)
          %poke  (ev-mess-poke [dud +.mess]:task)
        ==
      ==
    ::
    ++  ev-take
      =>  |%  +$  res-task
                $:  =wire
                    $=  take
                    $%  $>(?(%done %response %boon) gift) :: XX
                ==  ==
          --
      |=  task=res-task
          :: task=[=wire %boon payload=*]
      |^  ^+  ev-core
      ?-  -.take.task
            %boon  take-boon
            %done  (ev-poke-done wire.task +.take.task)
        %response  (ev-response wire.task +.take.task)
      ==
      ::
      ++  take-boon
        =^  u-bone-her  ev-core
          (ev-validate-wire hen wire.task)
        ?~  u-bone-her  ev-core
        =,  u.u-bone-her
        ?>  ?=([%out %bak] [were dire])  ::  vane acks happen on backward flows
        ::(req-plea [ship $% / payload]:task)
        ~!  task
        (ev-req-boon bone ev-chan +.take.task)
      --
    ::
    +|  %request-flow
    ::
    ++  ev-req-plea
      |=  [=ship vane=@tas =wire payload=*]
          ::[load=$%($>(%boon gift:ames) $>(%plea task:ames)) wire=(unit wire)]
      ^+  ev-core
      =+  per-sat=(ev-get-per ship)
      ?.  ?=([~ ~ *] per-sat)
        ::  XX handle
        !!
      ::
      =.  per  ship^u.u.per-sat
      =^  bone  ossuary.sat.per        ::  XX  to arm?
        =,  ossuary.sat.per
        ?^  bone=(~(get by by-duct) hen)
          [u.bone ossuary.sat.per]
        :-  next-bone  ^+  ossuary.sat.per
        :+  +(next-bone)
          (~(put by by-duct) hen next-bone)
        (~(put by by-bone) next-bone hen)
      ::
      ::  handle cork
      ::
      =/  cork=?  =([%$ /flow %cork ~] vane^wire^payload)
      ?.  (~(has by by-bone.ossuary.sat.per) bone)
        ~&  "trying to cork {<bone=bone>}, not in the ossuary, ignoring"
        ev-core
      =^  moves  ax
        =<  fo-abet
        %.  plea/[vane wire payload]
        fo-call:(fo-abed:fo hen bone^dire=%for ev-chan `cork)
      (ev-emil moves)
    ::
    ++  ev-req-boon  :: XX refactor with req-plea
      |=  [=bone =channel load=*]
      ::  XX handle corked/closing bones
      ::
      =^  moves  ax
        fo-abet:(fo-call:(fo-abed:fo hen bone^dire=%bak channel ~) boon/load)
      (ev-emil moves)
    ::
    ++  ev-req-peek
      |=  spar
      ^+  ev-core
      =+  per-sat=(ev-get-per ship)
      ?.  ?=([~ ~ *] per-sat)
        ev-core  ::  %alien or missing
      =.  per  [ship u.u.per-sat]
      :: ::
      ?^  ms=(~(get by pit.sat.per) path)
        =.  peers.ax
          =/  pit
            (~(put by pit.sat.per) path u.ms(for (~(put in for.u.ms) hen)))
          (~(put by peers.ax) ship known/sat.per(pit pit))
        ev-core
      =|  new=request-state
      =.  for.new  (~(put in for.new) hen)
      =.  peers.ax
        %+  ~(put by peers.ax)  ship
        known/sat.per(pit (~(put by pit.sat.per) path new))
      ::  XX construct and emit initial request packet
      ::
      ev-core
    ::
    +|  %response-flow
    ::
    ++  ev-pact-poke
      |=  [=ack=name:pact =poke=name:pact =data:pact]
      ::  XX dispatch/hairpin &c
      ::
      ::  - pre-check that we want to process this poke (recognize ack path, ship not blacklisted, &c)
      ::  - initialize our own outbound request for the poke payload
      ::  - start processing the part of the poke payload we already have
      ::    - validation should crash event or ensure that no state is changed
      ::  XX  parse path to get: requester, rift, bone, message
      ::
      :: ?.  =(1 tot.payload)
      ::  !!  ::  XX  need to retrieve other fragments
      !!
    ::
    ++  ev-pact-peek
      |=  =name:pact
      ?.  =(our her.name)
        ev-core
      =/  res=(unit (unit cage))  (inner-scry ~ /ames %x (name-to-beam name))
      ?.  ?=([~ ~ ^] res)
        ev-core
      (ev-emit hen %give %send ~ !<(@ q.u.u.res))
    ::
    ++  ev-pact-page
      |=  [=name:pact =data:pact =next:pact]
      ^+  ev-core
      ::  check for pending request (peek|poke)
      ::
      =*  ship  her.name
      ?~  per=(~(get by peers.ax) ship)
        ev-core
      ?>  ?=([~ %known *] per)  ::  XX alien agenda
      ?~  res=(~(get by pit.u.per) pat.name)
        ev-core
      ::
      =/  [typ=?(%auth %data) fag=@ud]
        ?~  wan.name
          [?:((gth tot.data 4) %auth %data) 0]
        [typ fag]:wan.name
      ::
      ?-    typ
          %auth
        ?.  ?|  ?=(~ ps.u.res)
                =(0 fag)
                (gth tot.data 4)
            ==
          ev-core
        =/  proof=(list @ux)  (rip 8 dat.data)
        ?>  (verify:crypt (recover-root:lss proof) aut.data)
        ?~  state=(init:verifier:lss tot.data proof)
          ev-core
        =.  peers.ax
          %+  ~(put by peers.ax)  her.name
          =-  u.per(pit -)
          %+  ~(put by pit.u.per)  pat.name
          u.res(ps `[u.state ~])
        ::
        ::  request next fragment
        ::
        =/  =pact:pact  [%peek name(wan [%data 0])]
        (ev-emit unix-duct.ax %give %send ~ p:(fax:plot (en:^pact pact)))
      ::
          %data
        ?>  =(13 boq.name)  :: non-standard
        ::  do we have packet state already?
        ::
        ?~  ps.u.res
          ::  no; then this should be the first fragment, and auth should be present
          ::
          ?>  =(0 fag)
          ?>  ?=([%0 *] aut.data)
          ::  is this a standalone message?
          ::
          ?:  =(1 tot.data)
            ?>  (verify:crypt (blake3 dat.data) p.aut.data)
            =/  =spar:ames  [her.name pat.name]
            =/  =auth:mess  p.aut.data
            =/  =page  ;;(page (cue dat.data))
            %-  ~(rep in for.u.res)
            |=  [hen=duct c=_ev-core]
            (ev-emit:c hen %give %response [%page [spar auth page]])
          ::  no; then the proof should be inlined; verify it
          ::  (otherwise, we should have received an %auth packet already)
          ::
          ?>  (lte tot.data 4)
          =/  proof=(list @ux)
            =>  aut.data
            ?>  ?=([%0 *] .)
            ?~(q ~ ?@(u.q [u.q ~] [p q ~]:u.q))
          =.  proof  [(leaf-hash:lss fag dat.data) proof]
          ?>  (verify:crypt (recover-root:lss proof) p.aut.data)
          ?~  state=(init:verifier:lss tot.data proof)
            ev-core
          ?~  state=(verify-msg:verifier:lss u.state dat.data ~)
            ev-core
          ::  initialize packet state and request next fragment
          ::
          =.  peers.ax
            %+  ~(put by peers.ax)  her.name
            =-  u.per(pit -)
            %+  ~(put by pit.u.per)  pat.name
            u.res(ps `[u.state ~[dat.data]])
          =/  =pact:pact  [%peek name(wan [%data leaf.u.state])]
          (ev-emit unix-duct.ax %give %send ~ p:(fax:plot (en:^pact pact)))
        ::  yes, we do have packet state already
        ::
        =*  ps  u.ps.u.res
        ?.  =(leaf.los.ps fag)
          ev-core
        ::  extract the pair (if present) and verify
        ::
        =/  pair=(unit [l=@ux r=@ux])
          ?~  aut.data  ~
          `?>(?=([%1 *] .) p):aut.data
        ?~  state=(verify-msg:verifier:lss los.ps dat.data pair)
          ev-core
        ::  update packet state
        ::
        =.  los.ps  u.state
        =.  fags.ps  [dat.data fags.ps]
        =.  peers.ax
          %+  ~(put by peers.ax)  her.name
          =-  u.per(pit -)
          %+  ~(put by pit.u.per)  pat.name
          u.res
        ::  is the message incomplete?
        ::
        ?.  =(+(fag) leaves.los.ps)
          ::  request next fragment
          ::
          =/  =pact:pact  [%peek name(wan [%data leaf.u.state])]
          (ev-emit unix-duct.ax %give %send ~ p:(fax:plot (en:^pact pact)))
        ::  yield complete message
        ::
        =/  =spar:ames  [her.name pat.name]
        =/  auth  [%| *@uxI] :: XX should be stored in ps?
        =/  =page  ;;(page (cue (rep 13 (flop fags.ps))))
        (ev-emit hen %give %response [%page [spar auth page]])
      ==
    ::
    ++  ev-mess-page
      |=  [=spar auth:mess =gage:mess]
      =*  ship  ship.spar
      ?~  rs=(~(get by peers.ax) ship)
        ev-core
      ?>  ?=([~ %known *] rs)  ::  XX alien agenda
      ?~  ms=(~(get by pit.u.rs) path.spar)
        ev-core
      ::
      ::  XX validate response
      ::  XX give to all ducts in [for.u.ms]
      ::
      ::  [%give %response mess]
      ::
      ::  XX abet
      =.  pit.u.rs  (~(del by pit.u.rs) path.spar)
      =.  peers.ax  (~(put by peers.ax) ship.spar u.rs)
      ev-core
    ::
    ++  ev-mess-poke
      |=  [dud=(unit goof) =ack=spar =pok=spar auth:mess =gage:mess]
      ::  XX dispatch/hairpin &c
      ::
      ::  - check that we recognize ack-path
      ::  - validate inner payload message
      ::  - route message to inner module (ie, flow)
      ::  XX  wait for done from vane
      ::  XX  then, place ack in namespace,
      ::  XX  emit $page as an effect to vere to cache it
      ::  XX  wait for cork to clean the flow
      ::
      =+  ?~  dud  ~
          %-  %+  slog  leaf+"mesa: fragment crashed {<mote.u.dud>}"
              tang.u.dud
          ::  XX what if the crash is due to path validation
          ::  and we can't infer the sequence number?
          ~
      =/  ack=(pole iota)
        %-  ev-pave
        ?~  inn=(inner-path-to-beam *@p path.ack-spar)  ~  ::  XX to helper arm
        ?>  =([[%$ %x] *@p %$ ud+1] [vew -.bem]:u.inn)
        s.bem.u.inn
      =/  pok=(pole iota)
        %-  ev-pave
        ?~  inn=(inner-path-to-beam *@p path.pok-spar)  ~  ::  XX to helper arm
        ?>  =([[%$ %x] *@p %$ ud+1] [vew -.bem]:u.inn)
        s.bem.u.inn
      ~|  path-validation-failed/path.ack-spar^path.pok-spar
      ?>  &(?=(res-mess-pith ack) ?=(res-mess-pith pok))
      ::
      :: =/  [sndr=@p rcvr=@p]  [ship.pok-spar ship.ack-spar]  :: XX validated in the packet layer?
      ?.  =(sndr.ack our)  ::  do we need to respond to this ack?
        ~&  >>  %not-our-ack^sndr.ack^our
        ev-core
      ?.  =(rcvr.pok our)  ::  are we the receiver of the poke?
        ~&  >  %poke-for-other^[rcvr.pok our]
        ev-core
      :: =/  ship-state  (~(get by peers.ax) sndr.pok)
      :: ?.  ?=([~ %known *] ship-state)
      ::   ::  XX handle
      ::   !!
      :: ::
      :: =*  peer-state  +.u.ship-state
      :: =+  pe-core=(pe-abed-her:pe hen sndr.pok peer-state)
      =+  per-sat=(ev-get-per sndr.pok)
      ?.  ?=([~ ~ *] per-sat)
        ::  XX handle
        !!
      ::
      =.  per  sndr.pok^u.u.per-sat
      =/  dire=?(%for %bak)  :: flow swtiching
        ?:  =(%for dire.pok)  %bak
        ?>  =(%bak dire.pok)  %for
      =/  req=mesa-message
        ?>  ?=([%message *] gage)  :: XX ??
        ::  the bone will tell us if this is a %boon or a %plea
        ::  (corks are always sent on bone %0 and received by bone %1)
        ::
        ::  %boon(s) sink forward in the reverse %plea direction
        ?:  =(%for dire) :: ?:  =(%0 (mod bone 4))
                          ::  %boon(s) sink on bone %0
          boon/+.gage
        ::  %ples(s) (%corks as well) sink backward
        ?>  =(%bak dire)
        :: ?.  =(%1 (mod bone 4))  !!
        :: ::  %plea(s) and %cork(s) sink on bone %1
        plea/;;(plea +.gage)
      ::
      =/  fo-core
        %.  [%sink mess.pok req ?=(~ dud)]
        fo-call:(fo-abed:fo hen bone.pok^dire ev-chan ~)
      =^  moves  ax
        ?.  closing.state.fo-core
          fo-abet:fo-core
        :: if the flow changed to closing, we received a %cork;
        :: remove the flow and publish %cork %ack in the namespace
        ::
        ::  XX to arm
        =.  sat.per
          =,  sat.per
          %_  sat.per
            flows   (~(del by flows) bone.pok^dire)
            corked  (~(put in corked) bone.pok^dire) :: XX bind in namespace
          ==
        [moves:fo-core ax(peers (~(put by peers.ax) [ship known/sat]:per))]
      (ev-emil moves)
    ::
    ++  ev-mess-peek
      |=  =spar
      ?.  =(our ship.spar)
        ev-core
      =/  res=(unit (unit cage))
        !!  :: scry for path
      ?.  ?=([~ ~ ^] res)
        ev-core
      ::  XX [%give %response %page p.mess [p q.q]:u.u.res]
      ev-core
    ::
    +|  %responses
    ::
    ::  +ev-response: network responses
    ::
    ++  ev-response
      |=  [=wire load=[%page sage:mess]]
      ^+  ev-core
      ::  XX same as ma-poke-done; move to helper arm ?
      =^  u-bone-her  ev-core
        (ev-validate-wire hen wire)
      ?~  u-bone-her  ev-core
      ::  XX use $pith for this?
      =,  u.u-bone-her
      ::  XX replaced by the flow "dire"ction ?(%for %bak)
      ::  based on the bone we can know if this payload is an ack?
      ::  bone=0                                   bone=1
      ::  response   <=  ack payloads        =>       response
      ::             <=  boon/poke payloads  =>
      ::
      ::  bones for acks are "internal", -- triggered by internal requests
      ::  for %poke payloads "external" -- triggered by hearing a request
      ::
      ::  wires are tagged ?(%int %ext) so we can diferentiate if we are
      ::  proessing an ack or a naxplanation payload
      ::
      ::
      =/  fo-core
        ::  XX parse $ack payload in here, and call task instead?
        %.  [were response/[seq +.load]]
        fo-take:(fo-abed:fo hen bone^dire ev-chan ~)
      =^  moves  ax
        ?.  closing.state.fo-core
          fo-abet:fo-core
        :: if the flow changed to closing, we received an %ack for a %cork;
        :: remove the flow and it's associated bone in the ossuary
        ::
        ::  XX to arm
        =.  sat.per
          =,  sat.per
          %_  sat.per
            flows            (~(del by flows) bone^dire)
            corked           (~(put in corked) bone^dire)
            by-duct.ossuary  (~(del by by-duct.ossuary) (ev-got-duct bone))  ::  XX bone^side=%for
            by-bone.ossuary  (~(del by by-bone.ossuary) bone)                ::  XX bone^side=%for
          ==
        [moves.fo-core ax(peers (~(put by peers.ax) [ship known/sat]:per))]
      (ev-emil moves)
    ::  +ev-poke-done: vane responses
    ::
    ++  ev-poke-done
      |=  [=wire error=(unit error)]
      ^+  ev-core
      =^  u-bone-her  ev-core
        (ev-validate-wire hen wire)
      ?~  u-bone-her  ev-core
      =,  u.u-bone-her
      ?>  ?=([%out %bak] [were dire])  ::  vane acks happen on backward flows
      ::
      ::  relay the vane ack to the foreign peer
      ::
      =^  moves  ax
        =<  fo-abet
        ::  XX since we ack one message at at time, seq is not needed?
        ::  XX use it as an assurance check?
        ::
        %.  [%out done/error]
        fo-take:(fo-abed:fo hen bone^dire=%bak ev-chan ~)
      (ev-emil moves)
    ::
    +|  %messages
    ::
    ++  ev-make-mess
      |=  [p=spar q=(unit path) r=namespace]
      ^+  ev-core
      =/  per  (~(gut by peers.ax) ship.p *ship-state)
      ?>  ?=([%known *] per)  ::  XX alien agenda
      ?^  res=(~(get by pit.per) path.p)
        ?>  =(q pay.u.res)  ::  prevent overriding payload
        =-  ev-core(peers.ax -)
        %+  ~(put by peers.ax)  ship.p
        =-  per(pit -)
        %+  ~(put by pit.per)  path.p
        u.res(for (~(put in for.u.res) hen))
      ::
      ?:  ?&  ?=(^ q)
              =;  res=(unit (unit cage))
                !?=([~ ~ %message *] res)
              ?~  inn=(inner-path-to-beam our u.q)
                ~
              %-  inner-scry
              [~ /mesa [?>(?=(^ vew) car.vew) bem]:u.inn]  ::  XX (rof ...)
          ==
        ~|  q
        !! :: XX wat do?
      =|  new=request-state
      =.  for.new   (~(put in for.new) hen)
      =.  pay.new   q
      =.  path.p    (ev-mess-spac r pax/path.p)  :: XX (add-beam  ...
      =.  peers.ax  (~(put by peers.ax) ship.p per(pit (~(put by pit.per) path.p new)))
      ::
      =/  =pact:pact  (ev-make-pact p q rift.per r)
      (ev-emit unix-duct.ax %give %send ~ p:(fax:plot (en:^pact pact)))
    ::
    ++  ev-make-peek
      |=  [spac=namespace p=spar]
      (ev-make-mess p ~ spac)
    ::
    ++  ev-make-poke
      |=  [spac=namespace p=spar q=path]
      (ev-make-mess p `q spac)
    ::
    ++  ev-make-pact
      |=  [p=spar q=(unit path) =per=rift spac=namespace]
      ^-  pact:pact
      =/  nam
        [[ship.p per-rift] [13 ~] path.p]
      ?~  q
        [%peek nam]
      ::  XX if path will be too long, put in [tmp] and use that path
      :: (mes:plot:d (en:name:d [[her=~nec rif=40] [boq=0 wan=~] pat=['c~_h' ~]]))
      :: [bloq=q=3 step=r=12]
      ::  =/  has  (shax u.u.res)
      ::  =.  tmpeers.ax  (~(put by tmpeers.ax) has [%some-envelope original-path u.u.res])
      ::  //ax/[$ship]//1/temp/[hash]
      =/  man=name:pact  [[our rift.ax] [13 ~] u.q]
      =?  spac  ?=(?(%publ %chum) -.spac)
        ?>  ?=(%publ -.spac)  :: XX chum
        spac(life life.ax)  ::  our life for poke payloads
      =.  pat.man  (ev-mess-spac spac pax/u.q)  :: XX (add-beam  ...
      :^  %poke  nam  man
      =;  page=pact:pact  ?>(?=(%page -.page) q.page)
      %-  parse-packet
      =<  ;;(@ q.q)  %-  need  %-  need
      (inner-scry ~ /ames %x (name-to-beam man))  ::  XX rof
    ::
    ++  ev-mess-spac
      |=  [spac=namespace path=$%([%cyf cyf=@] [%pax pax=path])]
      ^-  ^path
      ::  :^  %mess  (scot %ud rift.ax)  %$  ::  XX
      ?-  -.spac
        %publ  %+  weld
                 /publ/[(scot %ud life.spac)]
               ?>(?=(%pax -.path) pax.path)
        %shut  %+  weld
                 /shut/[(scot %ud kid.spac)]
               /[?>(?=(%cyf -.path) (scot %ud cyf.path))]
        %chum  %+  weld  =,  spac
                 /chum/[(scot %ud life)]/[(scot %p her)]/(scot %ud kid)
               /[?>(?=(%cyf -.path) (scot %ud cyf.path))]
      ==
    ::
    +|  %peer-helpers
    ::
    ++  ev-gut-per
      |=  [=duct =ship]
      ^+  ev-core
      =.  per
        :-  ship
        :: %^  ev-abed-per  duct  per
        =/  ship-state  (~(get by peers.ax) ship)
        ?.(?=([~ %known *] ship-state) *peer-state +.u.ship-state)
      ev-core
    ::
    ++  ev-got-per
      |=  [=duct =ship]
      ^+  ev-core
      =.  per
        :-  ship
        :: %^  ev-abed-per  duct  per
        ~|  %freaky-alien^ship
        =-  ?>(?=(%known -<) ->)
        (~(got by peers.ax) ship)
      ev-core
    ::  +get-her-state: lookup .her state, ~ if missing, [~ ~] if %alien
    ::
    ++  ev-get-per
      |=  her=ship
      ^-  (unit (unit peer-state))
      ::
      ?~  per=(~(get by peers.ax) her)  ~
      :: XX %alien
      :: ?:  ?=([~ %alien *] per)
      ::   [~ ~]
      ``+.u.per
    ::
    ++  ev-got-duct
      |=  =bone
      ^-  duct
      ~|(%dangling-bone^ship.per^bone (~(got by by-bone.ossuary.sat.per) bone))
    ::
    ++  fo  ::  ++flow (fo)
      =>  |%
          ::     using the bone to know the directtion of the flow ?
          ::     (bone numbers as see from the point of view of "our")
          ::
          ::       bone 0: sub (our) -> pub (her)  :: %plea: %poke, %watch, ...
          ::       bone 1: sub (her) <- pub (our)  :: %plea: %boon
          ::
          ::       bone 2: sub (our) <- pub (her)  :: %nack, on the receiver
          ::       bone 3: sub (her) <- pub (our)  :: orphaned; naxplanations are read via %peek
          ::
          ::
          +$  message-sign
            $%  [%done error=(unit error)]  ::  hear (n)ack for %poke, can trigger %peek for naxplanation
                ::[%boon ~]  :: XX handle
                [%response seq=@ud sage:mess]
            ==
          ::
          ::  XX move to top leve data-types core
          ::
          +$  gift  [seq=@ud =spar =path]  ::[p=spar q=(unit path)]
          --
      ::
      ::  key = /from=~zod/poke/to=~nec/flow/[bone]/[seq]
      ::  val = flow-state
      ::
      ::  bone is (mix 1 bone) if it comes via a %sink
      ::  XX (mix 1 bone) when sending instead?
      ::
      ::  XX add seq=message-num as [bone seq] to be +abed'ed?
      ::  currently passed in in ++call
      ::
      |_  [[hen=duct =side =channel] state=flow-state]
      ::
      +*  veb         veb.bug.channel
          her         her.channel
          bone        bone.side
          dire        dire.side
          peer-state  sat.per
      ::
      +|  %helpers
      ::
      ++  fo-core  .
      :: +$  fo-incoming  $~  [%incoming 0 | ~]
      ::                  $>(%incoming flow-state)  :: XX  *$>(%incoming flow-state)    works
      :: ::
      :: +$  fo-outbound  $~  [%outbound ~ 1 1]
      ::                  $>(%outbound flow-state)  :: XX  *$>(%outbound flow-state)  fails
      ::
      ++  fo-abed
        |=  [=duct =^side =^channel cork=(unit ?)]  :: XX remove channel
        =.  state  (~(gut by flows.peer-state) side *flow-state)
        =?  closing.state  ?=(^ cork)  u.cork
        fo-core(hen duct, side side, channel channel)
      ::
      ++  fo-abet  ::moves^state  :: XX (flop moves) done outside?
                                  :: XX (flop gifts) ??
        ^+  [moves ax]
        ::
        =.  flows.peer-state  (~(put by flows.peer-state) bone^dire state)
        [moves ax(peers (~(put by peers.ax) her known/peer-state))]
      ::
      ++  fo-emit      |=(=move fo-core(moves [move moves]))
      ++  fo-emil      |=(mos=(list move) fo-core(moves (weld mos moves)))
      ++  fo-ack-path  |=([seq=@ud =dyad] (fo-path seq %ack dyad))
      ++  fo-pok-path  |=([seq=@ud =dyad] (fo-path seq %poke dyad))
      ++  fo-nax-path  |=([seq=@ud =dyad] (fo-path seq %nax dyad))
      ++  fo-mop       ((on ,@ud mesa-message) lte)
      ++  fo-corked    (~(has in corked.peer-state) side)
      ++  fo-closing   closing.state
      ++  fo-is-naxed  |=(seq=@ud (~(has by nax.state) seq))
      ++  fo-to-close
        ::  if the flow is in closing, only allow sending the %cork %plea
        ::
        |=(poke=mesa-message ?&(fo-closing !=(poke [%plea %$ /flow %cork ~])))
      ::
      ++  fo-flip-dire  ?:(=(dire %for) %bak %for)
      ::
      +|  %builders
      ::
      ++  fo-path
        |=  [seq=@ud path=?(%ack %poke %nax) dyad]
        ^-  ^path
        %-  fo-view-beam
        :~  ::  %mess  %'0'
            :: %pact  (scot %ud packet-size)  %etch
            :: %init  ::  %pure  %data  %'0'  :: XX  frag=0
            :: %publ  %'0'  ::  XX
            %flow  (scot %ud bone)
            reqr=(scot %p sndr)  path  rcvr=(scot %p rcvr)
        ::  %ack(s) and %naxplanation(s) are on the other side, and not bounded
        ::  on our namespace
            ?:(=(%poke path) dire fo-flip-dire)
            (scot %ud seq)
        ==
      ::
      ++  fo-view-beam  |=(=path `^path`[vane=%$ care=%x case='1' desk=%$ path])
      ::
      ++  fo-make-wire
        |=  [were=?(%int %ext %out) seq=@ud]  ::  XX better names ?(for-acks=%int for-nax-payloads=%ext to-vane=%out)
        ^-  wire
        ::  %for: %plea(s) are always sent forward, %boon(s) %bak
        ::  both .to-vane and .dire are asserted when receiving the vane %ack
        ::  since they will always be %out and %bak
        ::
        :~  %flow  were  dire
            rcvr=[(scot %p her)]
          :: add rift to avoid dangling bones from previous eras
          ::
            rift=[(scot %ud rift.peer-state)]
            bone=[(scot %ud bone)]
            seq=[(scot %ud seq)]
          ==
      ::
      +|  %entry-points
      ::
      ++  fo-call
        =>  |%
            +$  poke-task
              $%  [%sink seq=@ud mess=mesa-message ok=?]
                  ::  XX remove %fo-planation from lull
                  mesa-message
              ==
            --
        ::
        |=  poke=poke-task
        ^+  fo-core
        ::
        ?-  -.poke
          ?(%plea %boon %cork)  (fo-send-poke poke)
          ::
            %sink
          ~|  mess.poke
          ::  a %plea sinks on the backward receiver (from a forward flow)
          ::  a %boon sinks on the forward receiver (from a backward flow)
          ::
          ?-  dire
            %bak  ?>(?=(%plea -.mess.poke) (fo-sink-plea [seq +.mess ok]:poke))
            %for  ?>(?=(%boon -.mess.poke) (fo-sink-boon [seq +.mess ok]:poke))
          ==
        ==
      ::
      ++  fo-take
        |=  [were=?(%ext %int %out) sign=message-sign]
        ^+  fo-core
        ::
        ?-  -.sign
             %done   ?>(?=(%out were) (fo-take-done +.sign))  :: ack from client vane
          ::
          %response   ?+  were  !!
                        :: XX payload given by the packet layer
                        :: via the wire used when %pass %a peek-for-poke
                        :: and only handled there?
                        %ext  (fo-take-naxplanation +.sign)
                        %int  (fo-take-ack +.sign)
        ==            ==
      ::
      ++  fo-peek
        |=  [load=?(%poke %ack %nax) mess=@ud]
        ^-  (unit page)
        ::  XX assert flow direction?
        ::  %ack and %nax can be both %for (%plea) and %bak (%boon)
        ::
        ?-  load
          ::  if mess > gth 10, no-op ?
          %ack   ?.(=(mess last-acked.state) ~ `ack/!(fo-is-naxed mess))
          %nax   ?~(nax=(~(get by nax.state) mess) ~ `nax/u.nax)
          %poke  ?~  v=(get:fo-mop loads.state mess)  ~
                 ?+  -.u.v  ~  :: XX cork?
                     %plea  `plea/[vane path payload]:u.v
                     %boon  `boon/payload.u.v
        ==       ==
      ::
      +|  %tasks
      ::
      ++  fo-send-poke
        |=  poke=mesa-message
        ?:  |((fo-to-close poke) fo-corked)  fo-core
        ::
        =.  loads.state  (put:fo-mop loads.state next-load.state poke)
        =;  core=_fo-core
          =.  next-wake.state.core  `(add now ~s30)
          ::  XX sets one timer for all the messsages in the flow
          ::
          %-  fo-emit:core
          :^    hen
              %pass
            /[(scot %p her)]/[(scot %ud bone)]/[(scot %ud rift.peer-state)]
          [%b %wait `@da`(need next-wake.state.core)]
        ::
        =+  loads=loads.state  ::  cache
        |-  ^+  fo-core
        =*  loop  $
        =+  num=(wyt:fo-mop loads)
        ?:  =(0 num)
          fo-core
        ?.  (lte num send-window.state)
          fo-core
        ::
        =^  [seq=@ud request=mesa-message]  loads  (pop:fo-mop loads)
        :: ?>  ?=(%plea -.request)  :: XX handle %cork
        :: ~!  +.state
        =:  send-window.state  (dec send-window.state)
            next-load.state    +(next-load.state)
          ==
        =/  paths=[spar path]
          [her^(fo-ack-path seq her our) (fo-pok-path seq our her)]
        =/  =wire  (fo-make-wire %int seq)
        ::  XX %ames call itself with a %make-poke task
        ::  on a wire used to infer the listener (the %poke %plea request; this)
        ::  when getting the %response $page with the %ack (tagged with %int)
        ::  and similarly for %naxplanation payloads (tagged with %ext)
        ::
        =/  spac=namespace  publ/life.peer-state  ::  XX %chum
        =.  fo-core  (fo-emit hen %pass wire %m make-poke/[spac paths])
        loop
      ::
      ++  fo-sink-boon
        |=  [seq=@ud message=* ok=?]
        ^+  fo-core
        =.  fo-core  (fo-emit (ev-got-duct bone) %give %boon message)
        ::  XX handle a previous crash
        :: =?  moves  !ok
        ::   ::  we previously crashed on this message; notify client vane
        ::   ::
        ::   %+  turn  moves
        ::   |=  =move
        ::   ?.  ?=([* %give %boon *] move)  move
        ::   [duct.move %give %lost ~]
        ::  XX emit ack to unix
        ::  ack unconditionally
        ::
        =.  last-acked.state  +(last-acked.state)
        fo-core
      ::
      ++  fo-sink-plea
        |=  [seq=@ud =plea ok=?]
        ^+  fo-core
        ::  receiver of a %plea request
        ::
        ::  XX check that the message can be acked (not in future, or far back past)
        ::
        ?.  ok
          %.  `*error
          fo-take-done:fo-core(pending-ack.state %.y)
        ::
        =/  =wire  (fo-make-wire %out seq)
        ?:  &(=(vane %$) ?=([%cork ~] payload) ?=([%flow ~] path)):plea
          ::  publisher receives %cork
          ::  mark flow as closing
          ::  publish %cork %ack (in +ev-mess-poke) in corked.peer-state
          ::
          =?  fo-core  ?=(^ next-wake.state)
            =/  =^wire
              /[(scot %p her)]/[(scot %ud bone)]/[(scot %ud rift.peer-state)]  :: XX to arm
            ::  reset timer for %boon(s)
            ::
            (fo-emit [[/ames]~ %pass wire %b %rest u.next-wake.state])
          ::  XX just fo-core(closing.state %.y) ?
          (fo-take-done:fo-core(closing.state %.y, pending-ack.state %.y) ~)
        =.  fo-core
          ?+  vane.plea  ~|  %mesa-evil-vane^our^her^vane.plea  !!
            ?(%c %e %g %j)  (fo-emit hen %pass wire vane.plea plea/her^plea)
          ==
        ::
        fo-core(pending-ack.state %.y)
      ::
      ++  fo-take-ack
        |=  [seq=@ud =spar auth:mess =gage:mess]
        ^+  fo-core
        :: ?>  ?=(%outbound -.state)
        ::  only handle acks for %pokes that have been sent
        ::
        ?:  (gth seq next-load.state)
          :: XX log?
          fo-core
        ::  if all pokes have been processed no-op
        ::
        ?~  first=(pry:fo-mop loads.state)
          fo-core
        ::XX  if the ack we receive is not for the first, no-op
        ::
        ?.  =(key.u.first seq)
          fo-core
        ?>  ?=([%message *] gage)
        =+  ;;(error=? +.gage)  ::  XX
        ?.  error
          ::  ack is for the first, oldest pending-ack sent message; remove it
          ::
          =^  *  loads.state  (del:fo-mop loads.state seq)
          ::  increase the send-window so we can send the next message
          ::
          =.  send-window.state  +(send-window.state)
          =/  =wire
            /[(scot %p her)]/[(scot %ud bone)]/[(scot %ud rift.peer-state)]  :: XX to arm
          :: ::  XX FIXME: have.?(%bak %for) -need.%for
          :: =?  fo-core  ?=(~ loads.state)  ::  no pending messages to be sent
          ::   (fo-emit [/ames]~ %pass wire %b %rest (need next-wake.state))
          =;  core=_fo-core
            ?^  loads.state  core
            ::  no pending messages to be sent
            ::
            (fo-emit:core [/ames]~ %pass wire %b %rest (need next-wake.state))
          ?:  ?|  closing.state  ::  %cork %ack; implicit ack
                  ?=(%bak dire)  ::  %boon %ack; assumed %acked from vane
              ==
            fo-core
          ::  XX we only get acks for the oldest message, since we only
          ::  send one (send-window=_1) at a time; when changing that,
          ::  the logic for knowing if the ack is for the cork will need
          ::  to be revisited
          ::
          (fo-emit (ev-got-duct bone) %give %done ~)
        ::  if error start %peek for naxplanation
        =/  =wire  (fo-make-wire %ext seq)
        =/  =path  (fo-nax-path seq her^our)
        ::  XX %ames call itself with a %make-peek task
        ::  on a wire used to infer the listener (the %poke %nax request; us)
        ::  when getting the %response $page with or %naxplanation payloads
        ::  (tagged with %ext)
        ::
        =/  spac=namespace  publ/life.peer-state  ::  XX %chum
        (fo-emit hen %pass wire %m make-peek/[spac her^path])
      ::
      ++  fo-take-done
        |=  error=(unit error)
        ^+  fo-core
        :: ?>  ?=(%incoming -.state)
        ::  if there's a pending-vane ack, is always +(last-acked)
        ::
        ?>  =(%.y pending-ack.state)
        =/  seq=@ud  +(last-acked.state)
        =:  last-acked.state   seq
            pending-ack.state  %.n
          ==
        =?  nax.state  ?=(^ error)
          =?  nax.state  (gth seq 10)
            ::  only keep the last 10 nacks
            ::
            (~(del by nax.state) (sub seq 10))
          (~(put by nax.state) seq u.error)
        ::  XX emit ack to unix
        fo-core
      ::
      ++  fo-take-naxplanation
        |=  [seq=@ud =spar auth:mess =gage:mess]
        ^+  fo-core
        ::  XX  the payload of the poke we sent has been removed already at this
        ::  point:
        ::
        ::  XX same as fo-take-ack refactor
        ::
        =/  next-load=@ud  ?~(next=(ram:fo-mop loads.state) 1 key.u.next)
        ?:  (gth seq next-load)
          :: XX log?
          fo-core
        ::  if all pokes have been processed no-op
        ::
        ?~  first=(pry:fo-mop loads.state)
          fo-core
        ::XX  if the ack we receive is not for the first, no-op
        ::
        ?.  =(key.u.first seq)
          fo-core
        ::  ack is for the first, oldest pending-ack set message, remove it
        ::
        =^  *  loads.state  (del:fo-mop loads.state seq)
        ::  XX check path.spar
        ::  XX path.spar will be the full namespace path, peel off before?
        ::
        ?>  ?=([%message %nax *] gage)
        =+  ;;(=error +>.gage)  ::  XX
        (fo-emit (ev-got-duct bone) %give %done `error)
      :: +|  %internals
      ::
      --
    ::
    ++  inner-scry
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
          ?.  =(rift.ax u.ryf)      ::  XX unauthenticated
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
              :: full proof; provide a pair of sibling hashes
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
          ?.  =(u.lyf life.ax)
            ~
          ?~  inn=(inner-path-to-beam our pat.tyl)
            [~ ~]
          ?~  res=(inner-scry ~ /ames/publ %x bem.u.inn)  :: XX back to roof
            ~
          =/  gag  ?~(u.res ~ [p q.q]:u.u.res)
          =/  ful  (en-beam bem)
          =/  ryf  rift.ax
          =/  ser  (jam gag)
          =/  rot  (blake3 ser)
          ``[%message !>([%sign (sign:crypt ryf ful rot) ser])]
        ::
            [%chum lyf=@ her=@ hyf=@ cyf=@ ~]
          =/  lyf  (slaw %ud lyf.tyl)
          =/  her  (slaw %p her.tyl)
          =/  hyf  (slaw %ud hyf.tyl)
          =/  cyf  (slaw %uv cyf.tyl)
          ?:  |(?=(~ lyf) ?=(~ her) ?=(~ hyf) ?=(~ cyf))
            [~ ~]
          ?.  =(u.lyf life.ax)
            ~
          ?~  key=(get-key-for u.her u.hyf)  :: eddh with our key
            ~
          ?~  tap=(decrypt:crypt u.cyf)  ~
          ?~  pat=(parse-path u.tap)  ~
          ?~  inn=(inner-path-to-beam our u.pat)  ~
          ?~  res=(rof `[u.her ~ ~] /ames/chum vew.u.inn bem.u.inn)
            ~
          =/  gag  ?~(u.res ~ [p q.q]:u.u.res)
          =/  ful  (en-beam bem)
          =/  ryf  rift.ax
          =/  ser  (jam gag)
          =/  rot  (blake3 ser)
          ``[%message !>([%hmac (hmac:crypt ryf ful rot) ser])]
        ::
            [%shut kid=@ cyf=@ ~]
          =/  kid  (slaw %ud kid.tyl)
          =/  cyf  (slaw %uv cyf.tyl)
          ?:  |(?=(~ kid) ?=(~ cyf))
            [~ ~]
          ?~  key=(get-group-key-for u.kid) :: symmetric key lookup
            ~
          ?~  tap=(decrypt:crypt u.cyf)  ~
          ?~  pat=(parse-path u.tap)  ~
          ::  XX check path prefix
          ?~  inn=(inner-path-to-beam our u.pat)
            ~
          ?~  res=(rof [~ ~] /ames/shut vew.u.inn bem.u.inn)
            ~
          =/  gag  ?~(u.res ~ [p q.q]:u.u.res)
          =/  ful  (en-beam bem)
          =/  ryf  rift.ax
          =/  ser  (jam gag)
          =/  rot  (blake3 ser)
          ``[%message !>([%sign (sign:crypt ryf ful rot) ser])]
        ::  publisher-side, flow-level
        ::
            ::res-mess-pith:ev-res  ::  /[~sndr]/[load]/[~rcvr]/flow/[bone]/[dire]/[mess]
            ::  XX drop sndr, it's always our
            [%flow bone=@ sndr=@ load=?(%poke %ack %nax) rcvr=@ dire=?(%for %bak) mess=@ ~]
          ::  XX remove typed-paths
          =>  .(tyl `(pole iota)`(ev-pave tyl))
          ?>  ?=(res-mess-pith tyl)
          ?.  =(our sndr.tyl)
            ~  :: we didn't send this poke
          ::  XX refactor block when +inner-scry arms goes back into +scry
          ::     to get all arms from ev-core
          ::
          =+  per-sat=(ev-get-per rcvr.tyl)
          ?.  ?=([~ ~ *] per-sat)
            ~  ::  %alien or missing
          =.  per  [rcvr.tyl u.u.per-sat]
          ?:  ?&  (~(has in corked.sat.per) [bone dire]:tyl)
                  =(%ack load.tyl)
              ==
              ::  if %ack for a %corked flow, produce %ack
              ::  XX when are corked bones evicted?
              ::
              ``[%message !>(`ack/%.y)]
          ::
          =/  res=(unit page)
            %.  [load mess]:tyl
            fo-peek:(fo-abed:fo ~[//scry] [bone dire]:tyl ev-chan ~)
          ?~(res ~ ``[%message !>(u.res)])
        ==
      ::  XX stub for app-level scries (e.g. /g/x/0/dap[...])
      ::
      ?:  ?&  =(our p.bem)
              ?|  =([%ud 0] r.bem)  :: XX
                  =([%ud 1] r.bem)
          ==  ==
        (rof [~ ~] /ames/dap %x bem)
      ::  only respond for the local identity, %$ desk, current timestamp
      ::
      ?.  ?&  =(our p.bem)
              =([%da now] r.bem)
              =(%$ q.bem)
          ==
        ~
      ::
      ::  /ax/peers/[ship]               ship-state
      ::
      ?.  ?=(%x car)  ~
      =/  tyl=(pole knot)  s.bem
      ::  private endpoints
      ::
      ?.  =([~ ~] lyc)  ~
      ?+    tyl  ~
            [%peers her=@ ~]
          =/  who  (slaw %p her.tyl)
          ?~  who  [~ ~]
          ?~  peer=(~(get by peers.ax) u.who)
            [~ ~]
          ``noun+!>(u.peer)
      ==
    ::
    +|  %system
    ::
    ++  sy  ::  system/internal: %born, %heed, %kroc, %prod...
      |_  hen=duct
      ++  sy-core  .
      ++  sy-abet  [moves ax]
      ++  sy-born  sy-core(ax ax(unix-duct hen))
      --
    ::
--
::
|%
::
++  call
  ::
  |=  [hen=duct dud=(unit goof) wrapped-task=(hobo task)]
  ^-  [(list move) _mesa-gate]
  =/  =task  ((harden task) wrapped-task)
  ::
  =^  moves  ax
    ::  handle error notification
    ::
    ?^  dud
      ?+  -.task  !!
          :: (on-crud:event-core -.task tang.u.dud)
        %hear  !!
        %mess  ev-abet:(~(ev-call ev-core hen) %mess p.task q.task dud)
      ==
    ::
    ?+  -.task  !!
      %vega  `ax
      %born  sy-abet:~(sy-born sy hen)
    ::
      %plea  ev-abet:(~(ev-call ev-core hen) %plea [ship plea]:task)
      %keen  ev-abet:(~(ev-call ev-core hen) %keen +.task)
    ::  from internal %ames request
    ::
      %make-peek  ev-abet:(~(ev-make-peek ev-core hen) [spac p]:task)
      %make-poke  ev-abet:(~(ev-make-poke ev-core hen) [spac p q]:task)
    ::  XX
    ::
      %hear  ev-abet:(~(ev-call ev-core hen) %hear [p q]:task)  ::  XX dud
      %mess  ev-abet:(~(ev-call ev-core hen) %mess p.task q.task ~)  ::  XX acks go direclty here
    ==
    ::
  [moves mesa-gate]
::
++  take
  |=  [=wire hen=duct dud=(unit goof) =sign]
  ^-  [(list move) _mesa-gate]
  ?^  dud
    ~|(%mesa-take-dud (mean tang.u.dud))
  ::
  =^  moves  ax
    ?:  ?=([%gall %unto *] sign)  :: XX from poking %ping app
      `ax
    ::
    ?+  sign  !!
      ::  XX handle
      :: [%behn %wake *]  (~(take ev-req hen) wire %wake error.sign)
    ::
      :: [%jael %turf *]          sy-abet:(~(on-take-turf sy hen) turf.sign)
      :: [%jael %private-keys *]  sy-abet:(~(on-priv sy hen) [life vein]:sign)
      :: [%jael %public-keys *]   sy-abet:(~(on-publ sy hen) wire public-keys-result.sign)
    ::  vane (n)ack
    ::
      [@ %done *]  ev-abet:(~(ev-take ev-core hen) wire %done error.sign)
    ::
    ::  vane gifts
    ::
      [@ %boon *]  ev-abet:(~(ev-take ev-core hen) wire %boon payload.sign)
    ::
    ::  network responses: acks/naxplanation payloads
    ::                     reentrant from %ames (either message or packet layer)
    ::
      [%mesa %response *]
    ::
      =/  response-pith  `(pole iota)`(ev-pave wire)
      =<  ev-abet
      %.  [wire %response +>.sign]
      ?+    response-pith   ~|  %mesa-evil-response-wire^wire  !!
          ::  %acks come directly into the message layer since they are always one
          ::  packet, and then given back to the flow layer that called them
          ::
          ev-flow-wire
        ~(ev-take ev-core hen)  ::  %ack and %naxplanation payload
      ==
    ::
    ==
  [moves mesa-gate]
::
++  stay  [%0 ax]
::
++  load
  |=  old=*
  mesa-gate
::
++  scry  inner-scry  ::  XX  just for testing, with a custom roof
--
