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
      =/  s  (rig [q.l q.r] r.l)
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
              [p.e (rig [q.e a.p] r.e)]
    ==
  --
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
::    > :(add 8 336 1.128)
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
      +$  data  [tot=@udF aut=auth:pact dat=@]
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
    ^-  [pact bloq step]
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
      (end [0 20] (mug (cut -.c [(rig [-.b -.c] +.b) +.c] dat)))
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
    =/  b=[bloq step]  [0 ?@(a 0 (rig [bloq.a 0] step.a))]
    |=  dat=@
    ^-  [[nex=@B typ=?(%page %peek %poke) hop=@ gum=@F] bloq step]
    =+  [[res nex ver tip hop gum tok] b]=((hew b dat) [2 2 3 2 3 20 32])
    ?>  =(0 res)
    ?>  =(1 ver)
    ?>  =(~tasfyn-partyv tok)
    =/  typ  ?+(tip !! %0b1 %page, %0b10 %peek, %0b11 %poke)
    [[nex typ hop gum] b]
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
    =/  c=[bloq step]  [3 ?@(a 0 (rig [bloq.a 3] step.a))]
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
::  range:  { meta[1], her[2^1-4], rif[1-4], boq[1],    fag[1-4], typ[2],      pat[2^0-16] }
::  max:    { meta[1], her[16],    rif[4],   boq[1],    fag[4],   typ[2],      pat[8.192] }
::          { meta-byte, address,  rift,     bloq-size, fragment, path-length, path }
::  actual: { meta[1], her[16],    rif[4],   boq[1],    fag[4],   typ[2],      pat[309]   }
::
::  meta: { rank[2], rift[2], init[1], is-auth[1], fag[2] }
::
::    > :(add 1 16 4 2 309 4)
::    336
::
++  name
  |%
  ++  en
    |=  name:pact
    ^-  plot
    =/  ran  ?~(her 0 (dec (met 0 (met 4 (end 7 her)))))
    =/  ryf  ?~(rif 0 (dec (met 3 rif)))  :: XX is rift always non-zero?
    =+  ^=  [nit tau gaf gyf fag]
      ?~  wan  [0b1 0b0 0b0 0 0]
      =/  gyf  ?~(fag.wan 1 (met 3 (end 5 fag.wan)))
      [0b0 ?:(?=(%auth typ.wan) 0b1 0b0) (dec gyf) gyf fag.wan]
    ::
    =/  tap  =-([p=(met 3 -) q=-] `@t`(rap 3 (join '/' pat)))
    ?>  (lth p.tap ^~((bex 16))) :: XX truncate instead?
    :+  bloq=3
      [s+~ 0 [2 ran] [2 ryf] [1 nit] [1 tau] [2 gaf] ~]
    [[(bex +(ran)) her] [+(ryf) rif] [1 boq] [gyf fag] [2 p.tap] tap ~]
  ::
  ++  de
    |=  a=bite
    =/  b=[bloq step]  [0 ?@(a 0 (rig [bloq.a 0] step.a))]
    |=  pat=@
    ^-  [name:pact bloq step]
    =+  [[ran ryf nit tau gaf] b]=((hew b pat) [2 2 1 1 2])
    =+  [len nex]=[(rig [bloq.b 3] step.b) (bex +(ran))]
    =/  her  (cut 3 [len nex] pat)
    ::
    =:  len  (add len nex)
        nex  +(ryf)
      ==
    =/  rif  (cut 3 [len nex] pat)
    ::
    =:  len  (add len nex)
        nex  1
      ==
    =/  boq  (cut 3 [len nex] pat)
    ::
    =:  len  (add len nex)
        nex  ?:(=(0b1 nit) 0 +(gaf))
      ==
    =/  fag  (cut 3 [len nex] pat)
    ::
    =:  len  (add len nex)
        nex  2
      ==
    =/  tap  (cut 3 [len nex] pat)
    ::
    =:  len  (add len nex)
        nex  tap
      ==
    =/  pat
      %+  rash  (cut 3 [len nex] pat)
      (more fas (cook crip (star ;~(less fas prn))))
    ::
    =/  wan
      ?.  =(0b1 nit)
        [?:(=(1 tau) %auth %data) fag]
      ?>(&(=(0 tau) =(0 fag)) ~)
    ::
    [[[her rif] [boq wan] pat] 3 (add len nex)]
  --
::
::  +data: response data
::
::  range:  { meta[1], tot[1-4], lut[0-1], aut[0-255], len[0-32], dat[0-2^252-1] }
::  max:    { meta[1], tot[4],   lut[1],   aut[255],   len[32],   dat[2^252-1]   }
::  actual: { meta[1], tot[4],   lut[1],   aut[96],    len[2],    dat[0-2^10-1]  }
::
::    > :(add 1 4 1 96 2 1.024)
::    1.128
::
++  data
  |%
  ++  en
    |=  [tot=@udF aut=auth:pact dat=@]
    ^-  plot
    =/  lot  (met 3 (end 5 tot))
    ::
    =/  [[aul=@ubB aum=plat:plot] aur=@ubB aup=plat:plot]
      ?~  aut           [[0b0 0] 0b0 0]
      ?:  ?=(%1 -.aut)  [[0b1 [32 p]] 0b0 32 q]:p.aut
      :-  =>  p.aut
          ?:(?=(%& -) [0b10 64 p] [0b11 32 p])
      =/  [aur=@ubB has=(list plat:plot)]
        ?~    q.aut  [0b0 ~]
        ?@  u.q.aut  [0b1 [1 u.q.aut] ~]
        [0b10 [[1 p] 1 q ~]:u.q.aut]
      [aur s+~ 8 has]
    ::
    =/  len  (met 3 dat)
    =/  men
      ?:((lth len 3) [len 0] [0b11 (met 3 len)])
    :+  bloq=3
      [s+~ 0 [2 (dec lot)] [2 aul] [2 aur] [2 -.men] ~]
    [[lot tot] aum aup [+.men len] [len dat] ~]
  ::
  ++  de
    |=  a=bite
    =/  b=[bloq step]  [0 ?@(a 0 (rig [bloq.a 0] step.a))]
    |=  dat=@
    ^-  [[tot=@udF aut=auth:pact dat=@] bloq step]
    =+  ^=  [[bot [aul aur] men] b]  ((hew b dat) [2 [2 2] 2])
    =+  ^=  [len nex]                [(rig [bloq.b 3] step.b) +(bot)]
    =/  tot  (cut 3 [len nex] dat)
    =.  len  (add len nex)
    =^  mes=(unit auth:mess)  nex
      ?+  aul  !!
        %0b0   ?>(=(0b0 aur) [~ nex])
        %0b1   ?>(=(0b10 aur) [~ nex])
        %0b10  [`&+(cut 3 [len 64] dat) 64]
        %0b11  [`|+(cut 3 [len 32] dat) 32]
      ==
    =.  len  (add len nex)
    =^  pac=(unit $@(@uxI (pair @uxI @uxI)))  nex
      ?+  aur  !!
        %0b0   [~ 0]
        %0b1   [`(cut 3 [len 32] dat) 32]
        %0b10  [`[(cut 3 [len 32] dat) (cut 3 [(add len 32) 32] dat)] 64]
      ==
    =/  aut=auth:pact
      ?~  mes
        ?:  =(0b0 aul)  ~
        ?>  &(=(0b1 aul) ?=([~ @ @] pac))
        [%1 u.pac]
      [%0 u.mes pac]
    =.  len  (add len nex)
    =^  lat  len
      ?.  =(3 men)  [men len]
      [(cut 3 [len 1] dat) +(len)]
    =.  nex  lat
    [[tot aut (cut 3 [len nex] dat)] 3 (add len nex)]
  --
::
++  name-to-path
  |=  name:pact
  ^-  path
  :*  %ax
      (scot %p her)  %$  '1'
      %mess  (scot %ud rif)
      %pact  (scot %ud boq)  %pure
      ?~  wan  [%init pat]
      [typ.wan (scot %ud fag.wan) pat]
  ==
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
  ::  +verifier: stateful core for sequentially verifying messages
  ::
  ++  verifier
    =<
      |%
      ::
      ++  init
        |=  [leaves=@ root=@ux proof=(list @ux)]
        ^-  (unit state)
        ?~  proof
          ::  need at least two leaves to have a proof
          ::
          ?.  (lte leaves 1)  ~
          `[leaves 0 [0 1] ~ ~]
        ::  recover root from proof
        ::
        ?.  ?=([@ @ *] proof)  ~
        =*  l0  i.proof
        =*  l1  i.t.proof
        =/  rut
          %-  root-hash
          %+  roll  t.t.proof
          |:  [p=0x0 n=(parent-output:blake3 l0 l1)]
          (parent-output:blake3 (output-cv:blake3 n) p)
        ?.  =(rut root)  ~
        ::  initialize leaf queue and parent stack with proof hashes;
        ::  after the first two leaves, the next subtree is [2 4]
        ::
        =/  state  [leaves 0 [0 1] ~ t.t.proof]
        `(push-leaves state [l0 l1])
      ::
      ++  verify-msg
        |=  [=state [leaf=octs pair=(unit [l=@ux r=@ux])]]
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
      |=  [=state leaf=octs]
      ^-  (unit _state)
      =/  cv  (output-cv:blake3 (chunk-output:blake3 leaf.state leaf))
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
+$  move  (wite note gift)
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
  $:  nex=(each %auth @ud)
      tot=@ud
      los=state:verifier:lss
  ==
::
++  parse-packet  |=(a=@ -:($:de:pact a))
++  is-auth-packet  |
++  inner-path-to-beam
  |=  [=ship =path]
  ^-  (unit [vew=view bem=beam])
  `[*view *beam]  :: XX
++  parse-path  |=(@ *(unit path))
++  blake3  |=(* *@)
++  get-key-for  |=([=ship =life] *@)
++  get-group-key-for  |=(@ud *(unit @))
++  crypt
  |%
  ++  sign  |=(* *@)
  ++  hmac  |=(* *@)
  ++  encrypt  |=(@ @)
  ++  decrypt  |=(@ *(unit @))
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
      ::  check for pending request (peek|poke)
      ::
      ?~  per=(~(get by p.ax) her.p.pac)
        [~ ax]
      ?~  res=(~(get by pit.u.per) pat.p.pac)
        [~ ax]
      ::
      =/  [typ=?(%auth %data) fag=@ud]
        ?~  wan.p.pac
          [?:((gth tot.q.pac 4) %auth %data) 0]
        [typ fag]:wan.p.pac
      ::
      ?:  =(0 fag)
        ?-    typ
            %auth
          ?.  ?|  ?=(~ ps.u.res)
                  ?=([%& %auth] nex.u.ps.u.res)
              ==
            [~ ax]
          ::  XX cut+validate sig/hmac
          =|  root=@ux   :: XX compute from proof
          =/  proof=(list @ux)  (rip 8 dat.q.pac)
          ?~  state=(init:verifier:lss tot.q.pac root proof)
            [~ ax]
          =.  ps.u.res
            ?~  ps.u.res
              `[[%| 0] tot.q.pac u.state]
            ps.u.res(los.u u.state)
          ::
          ::  XX request next fragment
          ::
          !!
        ::
            %data
          ?.  =(~ ps.u.res)
            [~ ax]
          ::
          ?:  =(1 tot.q.pac)    :: complete
            ::  XX produce as message w/ auth tag
            !!
          ::
          ?:  (gth tot.q.pac 4) :: auth-packet-needed
            ::  XX authenticate at message level with given root hash
            ::  XX request-auth-packet
            ::     by setting %auth in ps.request-state, regenerating next packet
            !!
          ::  proof is inlined
          ::  XX cut+validate sig/hmac
          =/  proof=(list @ux)
            =>  aut.q.pac
            ?>  ?=([%0 *] .)
            ?~(q ~ ?@(u.q [u.q ~] [p q ~]:u.q))
          =.  proof  [(leaf-hash:lss fag dat.q.pac) proof]
          =|  root=@ux :: XX compute from proof + leaf
          ?~  state=(init:verifier:lss tot.q.pac root proof)
            [~ ax]
          =.  ps.u.res  `[[%| 1] tot.q.pac u.state]
          ::
          ::  XX request next fragment
          !!
        ==
      ::
      ?:  ?=(%auth typ)  :: auth packets for subsequent fragments are not requested
        [~ ax]
      ?~  ps.u.res
        [~ ax]
      ?.  &(=(13 boq.p.pac) ?=(%| -.nex.u.ps.u.res) =(p.nex.u.ps.u.res fag))
        [~ ax]
      ::
      =/  pair=(unit [l=@ux r=@ux])
        ?~  aut.q.pac  ~
        `?>(?=([%1 *] .) p):aut.q.pac
      =/  msg  (met 3 dat.q.pac)^dat.q.pac
      ?~  state=(verify-msg:verifier:lss los.u.ps.u.res msg pair)
        [~ ax]
      =.  los.u.ps.u.res  u.state
      ::
      ::  XX persist fragment
      ::
      ?:  =(+(fag) tot.u.ps.u.res)  :: complete
        ::  XX produce as already-validated message
        !!
      ::  XX request next fragment
      ::     by incrementing fragment number in ps.request-state, regenerating next packet
      !!
    ::
        %peek
      ?.  =(our her.p.pac)
        [~ ax]
      =/  res=(unit (unit cage))
        !!  :: scry for path
      ?.  ?=([~ ~ ^] res)
        [~ ax]
      ::  XX [%give %send-response q.q.u.u.res]
      [~ ax]
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
    ::  XX resolve path to validate
    ::
    =/  res  *(unit (unit cage)) :: (rof ~ /ames/foo [[our ...] u.q])
    ?.  ?=([~ ~ %message *] res)
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
      =/  nam
        [[ship.p *rift] [13 ~] path.p] :: XX rift from peer-state
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
    [~ ax]
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
      |-  ^-  (unit (unit cage))
      ?~  wan.pac.nex
        $(wan.pac.nex [?:((gth wid 4) %auth %data) 0])
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
        =/  nam  [[our rif] [boq %auth fag] pat]
        ::  NB: root excluded as it can be recalculated by the client
        ::
        =/  aut  [%0 mes ~]
        =/  lss-proof  (build:lss (met 3 ser)^ser) ::  XX cache this
        =/  dat  [wid aut (rep 8 proof.lss-proof)]  :: XX types
        [nam dat]
      ::
          %data
        =/  lss-proof  (build:lss (met 3 ser)^ser)  :: XX cache this
        =/  nam  [[our rif] [boq %data fag] pat]
        =/  aut=auth:pact
          ?:  =(0 fag)
            :+  %0  mes
            ?:  =(1 wid)  ~  ::  single fragment
            ?:  (gth wid 4)  `root.lss-proof
            =/  tal  (tail proof.lss-proof)
            ?:  ?=(?(%1 %2) wid)
              ?>  ?=([* ~] tal)
              `i.tal
            ?>  ?=([* * ~] tal)
            `[i i.t]:tal
          ::
          ::  subsequent fragment; provide a pair of sibling hashes
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
      =/  gag  ?~(u.res ~ [p q.q]:u.u.res)
      =/  ful  (en-beam bem)
      =/  ryf  *rift :: XX our rift
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
      ?.  =(u.lyf *life) :: XX our life
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
      =/  ryf  *rift :: XX our rift
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
      =/  ryf  *rift :: XX our rift
      =/  ser  (jam gag)
      =/  rot  (blake3 ser)
      ``[%message !>([%sign (sign:crypt ryf ful rot) ser])]
    ==
  ~
--
