!:
=,  mesa
=/  packet-size  13
::  %plxt core
::
=>  |%
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
::  %dire helpers
::
=>  |%
    ::  atom ops
    ::
    +|  %atomics
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
      ?@  i.b.p
        [(met a.p i.b.p) i.b.p]
      ?-  -.i.b.p
        @       [p.i.b.p (end [a.p p.i.b.p] q.i.b.p)]
        [%c ~]  [q.p.i.b.p (cut a.p [p q]:i.b.p)]
        [%m ~]  =+((cut a.p [p q]:i.b.p) [(met a.p -) -])
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
          +$  sage  (trel spar auth gage)
          --
      $%  [%page sage]
          [%peek p=spar]
          [%poke p=spar q=sage]
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
          +$  name  [p=ship q=rift r=path s=bloq t=num=@udF]
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
          (en:head nex -.pak hop (mug q:(fax bod)))
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
        |-  ^-  (list plat)
        =;  one=(list plat)
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
    ::  range:  { meta[1], her[2^1-4], rif[1-4], typ[2^0-1], pat[2^0-16], boq[0-1], fag[1-4] }
    ::  max:    { meta[1], her[16],    rif[4],   typ[2],     pat[65.536], boq[1],   fag[4]   }
    ::          { meta-byte, address,  rift,    path-length, path,       bloq-size, fragment-number }
    ::  actual: { meta[1], her[16],    rif[4],   typ[2],     pat[309],    boq[1],   fag[4]   }
    ::
    ::    > :(add 1 16 4 2 309 4)
    ::    336
    ::
    ++  name
      |%
      ++  en
        |=  [her=@pH rif=@udF pat=path boq=@D num=@udF]
        ^-  plot
        =/  ran  ?~(her 0 (dec (met 0 (met 4 (end 7 her)))))
        =/  ryf  ?~(rif 0 (dec (met 3 rif)))  :: XX is rift always non-zero?
        =/  tap  =-([p=(met 3 -) q=-] `@t`(rap 3 (join '/' pat)))
        ?>  (lth p.tap ^~((bex 16)))
        =/  typ  (dec (met 3 p.tap))
        =/  loq  ?:(=(13 boq) 0 1)
        =/  fag  =-([p=(met 3 -) q=-] (end 5 num))
        :+  bloq=3
          [s+~ 0 [2 ran] [2 ryf] [1 typ] [1 loq] [2 (dec p.fag)] ~]
        [[(bex +(ran)) her] [+(ryf) rif] [+(typ) p.tap] tap [loq (end 3 boq)] fag ~]
      ::
      ++  de
        |=  a=bite
        =/  b=[bloq step]  [0 ?@(a 0 (rig [bloq.a 0] step.a))]
        |=  pat=@
        ^-  [[her=@p rif=@udF pat=path boq=bloq num=@udF] bloq step]
        =+  [[ran ryf typ loq fag] b]=((hew b pat) [2 2 1 1 2])
        =+  [len nex]=[(rig [bloq.b 3] step.b) (bex +(ran))]
        =/  her  (cut 3 [len nex] pat)
        =:  len  (add len nex)
            nex  +(ryf)
          ==
        =/  rif  (cut 3 [len nex] pat)
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
            nex  +(fag)
          ==
        [[her rif tap boq (cut 3 [len nex] pat)] 3 (add len nex)]
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
        =/  [[aul=@ubB aum=plat] aur=@ubB aup=plat]
          ?~  aut           [[0b0 0] 0b0 0]
          ?:  ?=(%| -.aut)  [[0b1 [32 p]] 0b0 32 q]:p.aut
          :-  =>  p.aut
              ?:(?=(%& -) [0b10 64 p] [0b11 32 p])
          =/  [aur=@ubB has=(list plat)]
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
    ++  encode-packet  |=(p=pact:pact (en:pact p))
    ++  is-auth-packet  |
    ++  inner-path-to-beam
      |=  [=ship =path]
      ^-  (unit [vew=view bem=beam])
      !!
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
::  vane types  ::  XX move to lull.hoon
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
              :: $>(?(%deep %keen) task:ames)
          $%  [%make-peek p=spar]           :: initiate %peek request
              [%make-poke p=spar q=path]    :: initiate %poke request
          ==  ==
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
          ::sign-arvo
          [%mesa gift]
          sign-arvo
      ==
    ::
    +$  peer-task  ,*        ::  XX fill out
    ::
    +$  spac  ?(%pact %publ %mess %chum %shut)  :: XX remove
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
=>  ::  inner event-handling cores
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
                                                                :: ^
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
    ::        ----------                      |--------------              ^
    ::       [%done err=~]             give %response       |              |
    ::            |                           |             |           to-vane
    ::        +ev-res                      +ma-page         |              |
    ::            |                           |             | +take-ack:fo-core(%response)
    ::        +pe-core                    +ma:ev-res        |              |
    ::            |                           |             |       +take:ma:ev-res
    :: +take:fo-core(%done)                   |             |              |
    ::            |                       [%mess %page]     |     /[wire-of-ack]/%int
    ::         emit ack (unix)                | unix        |              |
    ::                                       ~zod           |_______[%response =page]
    ::  ++fo core focuses on (map bone=@ud flow-state)
    ::
    |%
    ::
    +|  %helpers
    ::
    ++  ev-validate-wire
      |=  [hen=duct =wire]
      ^-  (unit [=bone seq=@ud channel peer-state])   ::  XX add parsing for flow-source = ?(%int %ext %out)
      |^  ?~  parsed=(parse-bone-wire wire)
        ::  no-op
        ::
        ~>  %slog.0^leaf/"mesa: dropping malformed wire: {(spud wire)}"
        ~
      ?>  ?=([@ her=ship *] u.parsed)
      =*  her  her.u.parsed
      =+  pe-core=(pe-abed-got:pe hen her)
      ?:  ?&  ?=([%new *] u.parsed)
              (lth rift.u.parsed rift.peer-state.pe-core)
          ==
        ::  ignore events from an old rift
        ::
        ~
      =/  [=bone seq=@ud]
          ?>  ?=([%new *] u.parsed)
          [bone seq]:u.parsed
      ::   ?-(u.parsed [%new *] bone.u.parsed, [%old *] bone.u.parsed)
      ~?  ?=([%old *] u.parsed)
        ::  XX log if this is an old wire
        *
      ::  XX add bone to pe-chan?
      `[bone seq pe-chan:pe-core]
      ::  +parse-bone-wire: decode ship, bone and rift from wire from local vane
      ::  XX %old handling still needed?
      ::
      ++  parse-bone-wire
        |=  =^wire
        ^-  %-  unit
            $%  [%old her=ship =bone]
                [%new her=ship =rift =bone seq=@ud]
            ==
        ?.  ?|  ?=([%flow @ @ @ @ ?(%out %ext %int) ?(%for %bak) ~] wire) ::?|  ?=([%bone @ @ @ ~] wire)
                ?=([%flow @ @ @ ?(%out %ext %int) ?(%for %bak) ~] wire)   ::    ?=([%bone @ @ ~] wire)
            ==                           ::
          ::  ignore malformed wires
          ::
          ~
        ?+    wire  ~
            [%flow @ @ @ @ @ ~]
          ~
          ::`[%old `@p`(slav %p i.t.wire) `@ud`(slav %ud i.t.t.wire)]
        ::
            [%flow @ @ @ @ @ @ ~]
          %-  some
          :*      %new
                her=`@p`(slav %p i.t.wire)
              rift=`@ud`(slav %ud i.t.t.wire)
            bone=`@ud`(slav %ud i.t.t.t.wire)
          seq=`@ud`(slav %ud i.t.t.t.t.wire)
        ==  ==
      --
    ::
    +|  %entry-points
    ::
    ++  ev-req  ::  send %peek/%poke requests
      =|  moves=(list move)
      ~%  %event-gate  ..ev-req  ~
      |_  hen=duct
      :: ~%  %req-core  ..$  ~
      +|  %helpers
      ::
      ++  req-core  .
      ++  req-abet  (flop moves)^ax
      ++  req-emit  |=(=move req-core(moves [move moves]))
      ++  req-emil  |=(mos=(list move) req-core(moves (weld (flop mos) moves)))
      ::
      +|  %tasks
      ::
      ++  call
        =>  |%  +$  req-task  $%  $>(?(%plea %keen) task)
                              ==
            --
        |=  task=req-task
        ^+  req-core
        ?-  -.task
          %plea  (req-poke [ship plea]:task)
          %keen  (req-peek +>.task)  ::  XX sec
        ==
      ::
      ::  should ack and payloads responses be handled here instead of by ev-res?
      ++  take
        =>  |%  +$  res-task  $:  =wire
                                  $%  $>(%boon gift:ames)
                              ==  ==
            --
        |=  task=res-task
        ^+  req-core
        ?~  u-bone-her=(ev-validate-wire hen wire.task)
          req-core
        =/  [=bone seq=@ud pe-chan=[=channel =peer-state]]
          u.u-bone-her
        =/  dire=?(%for %bak)  %bak
        ?>  ?=(%bak dire)  :: XX add more of these checks ?
        ::(req-poke [ship $% / payload]:task)
        (req-boon-poke bone pe-chan payload.task)
      ::
      +|  %internals
      ::
      ++  req-boon-poke  :: XX refactor with req-poke
        |=  [=bone pe-chan=[=channel =peer-state] load=*]
        ::  XX handle corked/closing bones
       ::
        ::  XX add seq to the gift, for tracking it in the wire
        ::
        =^  [gifts=(list [seq=@ud spar path]) moves-flow=_moves]  ax
          =<  fo-abut
          (fo-call:(fo-abed:fo hen bone flow=%out dire=%bak pe-chan) boon/load)
        ::
        ::  XX this can be done internally in fo-call:fo-core
        %+  roll  gifts
        |=  [gift=[seq=@ud =spar =path] co=_(req-emil moves-flow)]  :: XX =gift
        ::  XX %ames call itself with a %make-poke tasks
        ::  on a wire used to infer the listener (the %poke %plea request; this)
        ::  when getting the %response $page with the %ack (tagged with %int)
        ::  and similarly for %boon payloads (tagged with %ext)
        ::
         =/  =wire
          :~  %flow               :: flow request triggered "internally"
              rcvr=[(scot %p her.channel.pe-chan)]
              rift=[(scot %ud rift.peer-state.pe-chan)]
              bone=[(scot %ud bone)]
              seq=[(scot %ud seq.gift)]
              flow=%int      ::  ?(for-acks=%int for-payloads=%ext to-vanes=%out)
                        ::  XX skip, but use a %deep task instead?
              dire=%bak      ::  %boon(s) always sent backward
           ==
        (req-emit:co hen %pass wire %m make-poke/[spar path]:gift)
      ::
      ++  req-poke
        |=  [=ship vane=@tas =wire payload=*]
            ::[load=$%($>(%boon gift:ames) $>(%plea task:ames)) wire=(unit wire)]
        ^+  req-core
        =/  ship-state  (~(get by peers.ax) ship)
        ::
        ?.  ?=([~ %known *] ship-state)
          ::  XX handle
          !!
        ::
        =*  peer-state  +.u.ship-state
        =+  pe-core=(pe-abed-her:pe hen ship peer-state)
        ::
        =^  =bone  pe-core
          ?^  bone=(pe-get-bone:pe-core hen)
            [u.bone pe-core]
          ::  defer pe-abet:pe-core since we pass peer-state to the fo-core
          ::
          [pe-nex-bone:pe-core (pe-new-duct:pe-core hen)]
        ::
        ::  XX handle corked/closing bones
        =^  [gifts=(list [seq=@ud spar path]) moves-flow=_moves]  ax
          =<  fo-abut
          %.  plea/[vane wire payload]
          fo-call:(fo-abed:fo hen bone %out dire=%for pe-chan:pe-core)
        ::
        ::  XX this can be done internally in fo-call:fo-core
        %+  roll  gifts
        |=  [gift=[seq=@ud =spar =path] co=_(req-emil moves-flow)]  :: XX =gift
        ::  XX %ames call itself with a %make-poke tasks
        ::  on a wire used to infer the listener (the %poke %plea request; this)
        ::  when getting the %response $page with the %ack (tagged with %int)
        ::  and similarly for %boon payloads (tagged with %ext)
        ::
         =/  =^wire
          :~  %flow               :: flow request triggered "internally"
              rcvr=[(scot %p ship)]
              rift=[(scot %ud rift.peer-state)]
              bone=[(scot %ud bone)]
              seq=[(scot %ud seq.gift)]
              flow=%int      ::  ?(for-acks=%int for-payloads=%ext to-vanes=%out)
                        ::  XX skip, but use a %deep task instead?
              dire=%for      ::  %for; %plea(s) are always sent forward
           ==
        (req-emit:co hen %pass wire %m make-poke/[spar path]:gift)
      ::
      ++  req-peek
        |=  spar
        ^+  req-core
        :: =/  ship-state  (~(get by peers) ship.p)
        :: :: ::
        :: ?.  ?=([~ %known *] ship-state)
        ::   ::  XX handle
        ::   !!
        :: :: ::
        =+  peer-core=(pe-abed:pe hen ship)
        =*  peer-state  peer-state.peer-core
        :: ::
        ?^  ms=(~(get by pit.peer-state) path)
          =.  peers.ax
            =/  pit
              (~(put by pit.peer-state) path u.ms(for (~(put in for.u.ms) hen)))
            (~(put by peers.ax) ship known/peer-state(pit pit))
          req-core
        =|  new=request-state
        =.  for.new  (~(put in for.new) hen)
        =.  peers.ax
          %+  ~(put by peers.ax)  ship
          known/peer-state(pit (~(put by pit.peer-state) path new))
        ::  XX construct and emit initial request packet
        ::
        req-core
      --
    ::
    ++  ev-res  ::  hear %pact(++pa)/%mess(++ma) requests, send response
      =|  moves=(list move)
      |_  hen=duct
      ::
      +|  %helpers
      ::
      ++  res-core  .
      ++  res-abet  [(flop moves) ax]
      ++  res-emit  |=(=move res-core(moves [move moves]))
      ++  res-emil  |=(mos=(list move) res-core(moves (weld (flop mos) moves)))
      ::
      :: /~nec/ack/~zod/flow/bone=0/message=1
      :: /~zod/poke/~nec/flow/bone=0/message=1
      ::
      ::  XX unify parse-ack-path and parse-pok-path
      ::  XX convert path to $pith
      ::
      ++  parse-ack-path
        |_  [=ship path=(pole knot)]
        ::  ?>  ?=([%ax her=@ vane=%$ ver=@ spac=@ rift=@ inner-path=*] path)
        ::
        ++  ship  ^ship
        ++  de
          ::  path validation
          ::  XX  validate that =(ship resp)
          ::
          =>  ?>  ?&  ?=([%ax her=@ vane=%$ ver=@ %mess rift=@ ack=*] path)
                    =(ver.path '1')
                    ?=  [rcvr=@ %ack reqr=@ %flow bone=@ dire=?(%for %bak) mess=@ *]
                        ack.path
                ==
              [path .]
          ::
          =,  ack.-
          |%  ++  bone  (rash ^bone dem)
              ++  rcvr  `@p`(slav %p ^rcvr)
              ++  reqr  `@p`(slav %p ^reqr)
              ++  rift  (rash ^rift dem)
              ++  mess  (rash ^mess dem)
              ++  dire  ^dire
          --
        --
      ::
      ++  parse-pok-path
        |_  [=ship path=(pole knot)]
        ::  ?>  ?=([%ax her=@ vane=%$ ver=@ spac=@ rift=@ inner-path=*] path)
        ::
        ++  ship  ^ship
        ++  de
          ::  path validation
          ::  XX  validate that =(ship rcvr)
          ::
          =>  ?>  ?&  ?=([%ax her=@ vane=%$ ver=@ %mess rift=@ pok=*] path)
                      =(ver.path '1')
                      ?=  [sndr=@ %poke rcvr=@ %flow bone=@ dire=?(%for %bak) mess=@ *]
                      pok.path
                ==
              [path .]
          ::
          =,  pok.-
          |%  ++  bone  (rash ^bone dem)
              ++  rcvr  `@p`(slav %p ^rcvr)
              ++  sndr  `@p`(slav %p ^sndr)
              ++  rift  (rash ^rift dem)
              ++  mess  (rash ^mess dem)
              ++  dire  ^dire
          --
        --
      ::
      +|  %internals
      ::
      ++  pa  :: XX use +abet
        |%
        ::
        +|  %entry-points
        ::
        ++  call
          |=  [lane blob=@]
          ^+  res-core
          =/  =pact:pact  (parse-packet blob)
          ?-  -.pact
            %page  (pa-page +.pact)
            %peek  (pa-peek +.pact)
            %poke  (pa-poke +.pact)
          ==
        ::
        +|  %internals
        ::
        ++  pa-poke
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
        ++  pa-peek
          |=  =name:pact
          ?.  =(our p.name)
            res-core
          =/  res=(unit (unit cage))
            !!  :: scry for path
          ?.  ?=([~ ~ ^] res)
            res-core
          ::  XX [%give %send-response q.q.u.u.res]
          res-core
        ::
        ++  pa-page
          |=  [=name:pact =data:pact =next:pact]
          ::  XX initialize message core
          ::
          :: ma-abet:ma-hear:(ma hen p.p.pact)
          :: (each (list move) mess)
          ::
          ::  check for pending request (peek|poke)
          ::
          =*  ship  p.name
          ?~  per=(~(get by peers.ax) ship)
            res-core
          ?>  ?=([~ %known *] per)  ::  XX alien agenda
          ?~  res=(~(get by pit.u.per) r.name)
            res-core
          ::
          ?:  =(0 t.name)        :: is-first-fragment
            ?^  ps.u.res
              res-core
            ?:  =(1 tot.data)    :: complete
              ::  XX produce as message w/ auth tag
              !!
            ::
            ?:  (gth tot.data 4) :: auth-packet-needed
              ::  XX authenticate at message level with given root hash
              ::  XX request-auth-packet
              ::     by setting %auth in ps.request-state, regenerating next packet
              !!
            ::
            ::  XX LSS: use inline merkle proof
            ::
            ::    - initialize hash-tree by hashing fragment, prepending to proof, and validating
            ::    - authenticate at message level with computed root hash
            ::
            ::  XX request next fragment
            !!
          ::
          ?~  ps.u.res
            res-core
          ?:  is-auth-packet
            ?.  ?=(%auth nex.u.ps.u.res)
              res-core
            ::
            ::  XX LSS: validate merkle proof, initialize hash-tree
            ::  XX request next fragment
            !!
          ::
          ?.  &(=(13 s.name) ?=(%| -.nex.u.ps.u.res) =(p.nex.u.ps.u.res t.name))
            res-core
          ::
          ::  XX LSS: get hash pair from packet, validate and add to tree
          ::  XX LSS: validate fragment
          ::
          ::  XX persist fragment
          ::
          ?:  =(t.name tot.u.ps.u.res)  :: complete
            ::  XX produce as already-validated message
            !!
          ::  XX request next fragment
          ::     by incrementing fragment number in ps.request-state, regenerating next packet
          !!
       --
      ::
      ++  ma  :: XX use +abet
        |%
        ::
        +|  %entry-points
        ::
        ++  call
          |=  [(unit lane) =mess]
          ^+  res-core
          ~!  mess
          ?-  -.mess
            %page  (ma-page +.mess)
            %peek  (ma-peek +.mess)
            %poke  (ma-poke +.mess)
          ==
        ::
        ++  take
          =>  |%  +$  take-response
                    $%  $>(?(%response %done) gift) ::  acks (frome vane and network)
                    ==
              --
          |=  [=wire take=take-response]
          ?-  -.take
                %done  (ma-poke-done wire +.take)
            %response  (ma-response wire +.take)
          ==
        ::
        +|  %message-tasks
        ::
        ++  ma-page
          |=  [=spar auth:mess =gage:mess]
          =*  ship  ship.spar
          ?~  rs=(~(get by peers.ax) ship)
            :: [~ ax]
            res-core
          ?>  ?=([~ %known *] rs)  ::  XX alien agenda
          ?~  ms=(~(get by pit.u.rs) path.spar)
            ::[~ ax]
            res-core
          ::
          ::  XX validate response
          ::  XX give to all ducts in [for.u.ms]
          ::
          ::  [%give %response mess]
          ::
          ::  XX abet
          =.  pit.u.rs  (~(del by pit.u.rs) path.spar)
          =.  peers.ax  (~(put by peers.ax) ship.spar u.rs)
          res-core
        ::
        ++  ma-poke
          |=  [=ack=spar =pok=spar auth:mess =gage:mess]
          =*  mess  +<
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
          ::  XX  fake decoding
          :: ack-spar  =  /~nec/ack/~zod/flow/bone=0/?(plea=%for boon=%bak)/message=1
          :: poke-spar =  /~zod/poke/~nec/flow/bone=0/?(plea=%for boon=%bak)/message=1
          ::
          =+  ack=~(. parse-ack-path [ship path]:ack-spar)
          =+  pok=~(. parse-pok-path [ship path]:pok-spar)
          ::
          =/  [sndr=@p rcvr=@p]  [ship.pok-spar ship.ack-spar]
          ::  XX move all validation to parse-ack-path
          ::
          ?.  =(rcvr our)
            ~&  >>  %wrong-rcvr^rcvr^our
            res-core
          ?.  =(rcvr:de:ack our)  ::  do we need to respond to this ack?
            ~&  >>  %not-our-ack^rcvr:de:ack^our
            res-core
          ?.  =(rcvr:de:pok our)  ::  are we the receiver of the poke?
            ~&  >  %poke-for-other^[rcvr:de:pok our]
            res-core
          =/  ship-state  (~(get by peers.ax) sndr)
          ?.  ?=([~ %known *] ship-state)
            ::  XX handle
            !!
          ::
          =*  peer-state  +.u.ship-state
          =+  pe-core=(pe-abed-her:pe hen sndr peer-state)
          ::
          ::  XX not supported (flip bone's last bit to account for flow switching)
          ::
          =/  =bone  bone:de:pok  ::  (mix 1 bone:de:pok)
          =/  dire=?(%for %bak)  :: flow swtiching
            ?:  =(%for dire:de:pok)  %bak
            ?.  =(%bak dire:de:pok)  !!
            %for
            :: ?+  dire:de:pok  !!
            ::   %for  %bak
            ::   %bak  %for
            :: ==
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
          =^  moves  ax
            =<  fo-abet
            %.  [%sink mess:de:pok req]
            fo-call:(fo-abed:fo hen bone %in dire pe-chan:pe-core)
          (res-emil moves)
        ::
        ++  ma-peek
          |=  =spar
          ?.  =(our ship.spar)
            :: [~ ax]
            res-core
          =/  res=(unit (unit cage))
            !!  :: scry for path
          ?.  ?=([~ ~ ^] res)
            :: [~ ax]
            res-core
          ::  XX [%give %response %page p.mess [p q.q]:u.u.res]
          ::[~ ax]
          res-core
        ::
        +|  %responses
        ::
        ++  ma-response
          |=  [=wire load=$>(%page mess)]
          ^+  res-core
          ::  XX same as ma-poke-done; move to helper arm ?
          ?~  u-bone-her=(ev-validate-wire hen wire)
            res-core
          ::  XX use $pith for this?
          =/  [=bone seq=@ud pe-chan=[channel peer-state]]  u.u-bone-her
          =/  flow=?(%int %ext %out)  %int  ::  XX support %ext payloads?
                                       ::  are these always handled by the
                                       ::  packet layer?
                                       ::  or is it here that we produce
                                       ::  $peek or $poke complete messages for
                                       ::  the flow layer?
          ::  based on the bone we can know if this payload is an ack?
          ::  bone=0                                   bone=1
          ::  response   <=  ack payloads        =>       response
          ::             <=  boon/poke payloads  =>
          ::
          ::  bones for acks are "internal", -- triggered by internal requests
          ::  for %poke payloads "external" -- triggered by hearing a request
          ::
          ::  wires are tagged ?(%int %ext) so we can diferentiate if we are
          ::  proessing an ack or a payload
          ::
          ::
          =/  dire=?(%for %bak)  %for  ::  XX get from wire to know if ack for %plea or %boon
          =^  moves  ax
            =<  fo-abet
            ::  XX parse $ack payload in here, and call task instead?
            (fo-take:(fo-abed:fo hen bone %out dire pe-chan) flow response/[seq +.load])
          (res-emil moves)
        ::
        ++  ma-poke-done
          |=  [=wire error=(unit error)]
          ^+  res-core
          ?~  u-bone-her=(ev-validate-wire hen wire)
            res-core
          ::  XX use $pith for this
          =/  [=bone seq=@ud pe-chan=[channel peer-state]]  u.u-bone-her
          =/  flow=?(%int %ext %out)  %out  ::  XX parse wire
          =/  dire=?(%for %bak)       %bak  ::  XX from wire?
                                            ::  vane acks only happe on backward flows
          ::  relay the vane ack to the foreign peer
          ::
          =^  moves  ax
            =<  fo-abet
            ::  XX since we ack one message at at time, seq is not needed?
            ::  XX use it as an assurance check?
            ::
            (fo-take:(fo-abed:fo hen bone %in dire pe-chan) %out done/error)
          (res-emil moves)
        ::
        :: +|  %internals
        ::
        --
      ::
      --
    ::
    ++  ev-sys  ::  system/internal: %born, %heed, %kroc, %prod...
      =|  moves=(list move)
      |_  hen=duct
      ++  sys-core  .
      ++  sys-abet  [moves ax]
      ++  sys-born  sys-core(ax ax(unix-duct hen))
      --
    ::
    +|  %internals
    ::  +pe: per-peer processing
    ::
    ++  pe
      =|  moves=(list move)
      |_  [hen=duct =channel =peer-state]
      +*  veb    veb.bug.channel
          her    her.channel
          keens  keens.peer-state
      ::
      +|  %helpers
      ++  pe-core      .
      ++  pe-emit      |=(=move pe-core(moves [move moves]))
      ++  pe-emil      |=(mos=(list move) pe-core(moves (weld mos moves)))
      ++  pe-abed      |=([d=duct s=@p] (pe-abed-her d s (pe-gut-her-state s)))
      ++  pe-abed-got  |=([d=duct s=@p] (pe-abed-her d s (pe-got-her-state s)))
      ++  pe-abed-her
        |=  [=duct =ship peer=^peer-state]
        %_  pe-core
                hen   duct
          peer-state  peer
             channel  [[our ship] now [life crypto-core bug]:ax -.peer]
        ==
      ::
      :: ++  pe-abort    pe-core  :: keeps moves, discards state changes
      ++  pe-abet
        ^+  [moves ax]
        =.  peers.ax  (~(put by peers.ax) her known/peer-state) ::  XX outside?
        ~&  >>  next-bone/pe-nex-bone
        [moves ax]
      ::
      ++  pe-chan  [channel peer-state]  :: XX add type for this
      ::  +get-her-state: lookup .her state or ~
      ::
      ++  pe-get-her-state
        |=  her=ship
        ^-  (unit ^peer-state)
        ::
        =-  ?.  ?=([~ %known *] -)
              ~
            `+.u
        (~(get by peers.ax) her)
      ::  +got-her-state: lookup .her state or crash
      ::
      ++  pe-got-her-state
        |=  her=ship
        ^+  peer-state
        ::
        ~|  %freaky-alien^her
        =-  ?>(?=(%known -<) ->)
        (~(got by peers.ax) her)
      ::  +gut-her-state: lookup .her state or default
      ::
      ++  pe-gut-her-state
        |=  her=ship
        ^+  peer-state
        =/  ship-state  (~(get by peers.ax) her)
        ?.  ?=([~ %known *] ship-state)
          *^peer-state
        +.u.ship-state
      ::
      :: ++  pe-trace
      ::   |=  [verb=? print=(trap tape)]
      ::   ^+  same
      ::   (ev-trace verb her print)
      ::  +pe-got-duct: look up $duct by .bone, asserting already bound
      ::
      ++  pe-got-duct
        |=  =bone
        ^-  duct
        ~|(%dangling-bone^her^bone (~(got by by-bone.ossuary.peer-state) bone))
      ::
      ++  pe-nex-bone  next-bone.ossuary.peer-state
      ++  pe-get-bone
        |=  =duct
        ^-  (unit bone)
        (~(get by by-duct.ossuary.peer-state) duct)
      ::  +pe-new-duct:  make new $bone for .duct in .ossuary
      ::
      ++  pe-new-duct
        |=  =duct
        ^+  pe-core
        =*  ossa  ossuary.peer-state
        =.  ossa
          :+  +(next-bone.ossa)
            (~(put by by-duct.ossa) duct next-bone.ossa)
          (~(put by by-bone.ossa) next-bone.ossa duct)
        pe-core
      ::
      +|  %tasks
      ::
      ++  pe-call
        |=  [=spac task=peer-task]  ::  XX any namespace task?
        ^+  pe-core
        =^  moves  peer-state  ::  XX ... save in state
          [~ peer-state]
        (pe-emil moves)
      ::
      :: +|  %internals
      --
    ::
    ++  fo  ::  ++flow (fo) ||  ++bone (bo)
      =>  |%
          :: XX to lull.hoon,
          ::  - part of peer-state => (map bone flow-state)
          ::  - reshape message-pump/sink states into flow-states per bone
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
          ::  XX -- TODO simplify packet-state for each message (next=@ud)
          ::
          ::  - packet-pump-state in message-pump gone; moved ouf ot arvo into vere
          ::    - collapsed into the message level since we "send" full messages here
          ::    - XX only next-wake for backing off resends of expired messages
          ::  - message-sink-state only deals with messages, removes .partial-rcv-message
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
      =|  moves=(list move)
      =|  gifts=(list gift)
      ::  key = /from=~zod/poke/to=~nec/flow/[bone] {seq-no in ++pa layer}
      ::  val = flow-state
      ::
      ::  bone is (mix 1 bone) if it comes via a %sink
      ::  XX (mix 1 bone) when sending instead?
      ::
      ::  XX add seq=message-num as [bone seq] to be +abed'ed?
      ::  currently passed in in ++call
      ::
      |_  [[hen=duct =bone dire=?(%bak %for) peer-channel] state=flow-state]
      ::
      +*  veb    veb.bug.channel
          her    her.channel
          keens  keens.peer-state
          pe-ca  [channel peer-state]
      ::
      +|  %helpers
      ::
      ++  fo-core  .
      +$  fo-inbound   $~  [%in 0 | ~]
                       $>(%in flow-state)
      ::
      +$  fo-outbound  $~  [%out ~ 1 1]
                       $>(%out flow-state)
      ++  fo-abed
        |=  [=duct =^bone flow=?(%in %out) dire=?(%bak %for) peer-channel]
        =.  state
          %+  ~(gut by flows.peer-state)  bone^dire
          ?:  ?=(%out flow)
            *fo-outbound  :: XX *$>(flow flow-state)
          *fo-inbound
        fo-core(hen duct, bone bone, channel channel, peer-state peer-state, dire dire)
      ::
      ++  fo-abet  ::moves^state  :: XX (flop moves) done outside?
                                  :: XX (flop gifts) ??
        ^+  [moves ax]
        ::
        =.  flows.peer-state  (~(put by flows.peer-state) bone^dire state)
        =.  peers.ax          (~(put by peers.ax) her known/peer-state)
        [moves ax]
      ::
      ++  fo-abut      =^(moves ax fo-abet [(flop gifts)^moves ax])
      ++  fo-emit      |=(=move fo-core(moves [move moves]))
      ++  fo-emil      |=(mos=(list move) fo-core(moves (weld mos moves)))
      ++  fo-give      |=(=gift fo-core(gifts [gift gifts]))
      ++  fo-gifs      |=(gis=(list gift) fo-core(gifts (weld gis gifts)))
      ++  fo-ack-path  |=([dire=?(%for %bak) seq=@ud =dyad] (fo-path seq %ack dire dyad))
      ++  fo-pok-path  |=([dire=?(%for %bak) seq=@ud =dyad] (fo-path seq %poke dire dyad))
      ++  fo-mop       ((on ,@ud mesa-message) lte)
      ::
      +|  %gifts
      ::
      ::  XX FIXME +encs: path encodings
      ::  XX not used, remove?
      ++  encs
        |%  ++  her   =-  ~&(- -)  (scot %p her)
            ++  our   =-  ~&(- -)  (scot %p our)
            ++  bone  =-  ~&(- -)  (scot %ud bone)
            ++  mess  =-  ~&(- -)  |=(mess=@ud (scot %ud mess))
        --
      ::
      ++  fo-path
        |=  [seq=@ud path=?(%ack %poke) dire=?(%for %bak) =dyad]
        ::  %+  fo-en-spac  %mess  ::  XX remove, done by ++pa
        ^-  ^path
        :~  reqr=(scot %p sndr.dyad)  path  rcvr=(scot %p rcvr.dyad)
            %flow  (scot %ud bone)  dire  (scot %ud seq)
        ==
      ::
      ++  fo-pek-path  !!
      ::
      +|  %entry-points
      ::
      ++  fo-call
        =>  |%
            +$  poke-task
              $%  [%sink seq=@ud mess=mesa-message]
                  ::  XX remove %fo-planation from lull
                  mesa-message
              ==
            --
        ::
        |=  poke=poke-task
        ^+  fo-core
        ::
        ?-  -.poke
          ::  requests
          ::
          ?(%plea %boon %cork)
            ::  XX move to a separate arm
            ::  XX can we (re)use a sequence number after poping a previous one
            ::  off the queue?
            ::
            ?>  ?=(%out -.state)
            =/  next-load=@ud  ?~(next=(ram:fo-mop loads.state) 1 +(key.u.next))
            =.  loads.state  (put:fo-mop loads.state next-load poke)
            =;  core=_fo-core
              ::  XX sets one timer for all the messsages in the bone
              ::
              %-  fo-emit:core
              :^  hen
                %pass
              /[(scot %p her)]/[(scot %ud bone)]/[(scot %ud rift.peer-state)]
              [%b %wait `@da`(add now ~s30)]
            ::  XX does it help?
            ::  =>  ?>(?=(%out -.state) .)
            |-  ^+  fo-core
            =*  loop  $
            =+  num=(wyt:fo-mop loads.state)
            ?:  =(0 num)
              fo-core
            ?.  (lte num send-window.state)
              fo-core
            =/  [[seq=@ud request=mesa-message] load=_loads.state]
              (pop:fo-mop loads.state)
            :: ?>  ?=(%plea -.request)  :: XX handle %cork
            :: ~!  +.state
            =:  send-window.state  (dec send-window.state)
                loads.state        load
              ==
            =/  dire=?(%for %bak)
              ?+  -.poke  !!  :: XX handle %cork
                %plea  %for
                %boon  %bak
              ==
            :: XX FIXME
            :: =.  fo-core  fo-core
              :: %-  fo-give  ^-  gift
            =.  gifts  :_  gifts  ^-  gift
              [seq her^(fo-ack-path dire seq her our) (fo-pok-path dire seq our her)]
            loop
          ::  XX responses: (n)acks, %poke payloads
          ::
          %sink  (fo-sink +.poke)
        ==
      ::
      ++  fo-take
        |=  [flow=?(%ext %int %out) sign=message-sign]
        ^+  fo-core
        ::
        ?-  -.sign
             %done   ?>(?=(%out flow) (fo-take-done +.sign))  :: ack from client vane
          ::
          %response   ?+  flow  !!
                        :: XX payload given by the packet layer
                        :: via the wire used when %pass %a peek-for-poke
                        :: and only handled there?
                        %ext  !!  ::  (fo-take-load +.sign)
                        %int  (fo-take-ack +.sign)
        ==            ==
      ::
      +|  %tasks
      ::
      ++  fo-sink
        |=  [seq=@ud mess=mesa-message]
        ::  a %plea sinks on the backward receiver (coming from a forward flow)
        ::  a %boon sinks on the forward receiver (coming from a backward flow)
        ::
        ~|  mess
        ?-  dire
          %bak  ?>(?=(%plea -.mess) (fo-sink-plea seq +.mess))
          %for  ?>(?=(%boon -.mess) (fo-sink-boon seq +.mess))
        ==
        ::  use the -.mess instead?
        :: ?+  -.mess  !!
        ::   %plea  (fo-sink-plea seq +.mess)
        ::   %boon  (fo-sink-boon seq +.mess)
        ::   ::  %cork  (fo-sink-cork seq +.mess)
        :: ==
      ::
      ++  fo-sink-boon
        |=  [seq=@ud message=*] :: XX =error
        ^+  fo-core
        ?>  ?=(%in -.state)
        =/  =duct
          :: XX +pe-got-duct
          ~|(%dangling-bone^her^bone (~(got by by-bone.ossuary.peer-state) bone))
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
        ::  XX
        :: =.  fo-core
          (fo-emit duct %give %boon message)
        :: fo-core
      ::
      ++  fo-sink-plea
        |=  [seq=@ud =plea]
        ^+  fo-core
        ::  receiver of a %plea request
        ::
        ::  XX check that the message can be acked (not in future, or far back past)
        ::
        :: add rift to avoid dangling bones from previous eras
        ::
        =/  =wire   ::  XX to helper arm
          :~  %flow
              [(scot %p her)]
              [(scot %ud rift.peer-state)]
              [(scot %ud bone)]
              [(scot %ud seq)]    ::  XX  seq not needed  :: XX $% ?
              %out  ::  ?(for-acks=%int for-payloads=%ext to-vanes=%out)
                    ::  move to begining of wire
              dire  ::  %bak; %plea(s) always sink backward
          ==
        ?:  =(vane.plea %$)
          fo-core  ::  XX handle pre-cork ships
                   ::  XX maybe when checking path/protocol version
        =.  fo-core
          ?+  vane.plea  ~|  %mesa-evil-vane^our^her^vane.plea  !!
            ?(%c %e %g %j)  (fo-emit hen %pass wire vane.plea plea/her^plea)
          ==
        ::
        ?>  ?=(%in -.state)
        fo-core(pending-ack.state %.y)
      ::
      ++  fo-take-ack
        |=  [seq=@ud =spar auth:mess =gage:mess]
        ^+  fo-core
        ?>  ?=(%out -.state)
        ::  only handle acks for %poke that have been sent
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
        ::  XX handle closing and corked bones
        ::
        ?:  ?=(%bak dire)  fo-core   ::  %boon %ack, no-op
        :: ?:  =(%1 (mod bone 2))  fo-core  ::  %boon %ack, no-op
        ?>  ?=([%message *] gage)
        =+  ;;(error=(unit error) +.gage)
        ::  XX FIXME: have.?(%bak %for) -need.%for
        :: =?  fo-core  ?=(^ error)
        ::   ::  XX if error start %peek for naxplanation
        ::   fo-core
        =/  =duct
          :: XX +pe-got-duct
          ~|(%dangling-bone^her^bone (~(got by by-bone.ossuary.peer-state) bone))
        (fo-emit duct %give %done error)
      ::
      ++  fo-take-done
        |=  error=(unit error)
        ^+  fo-core
        ?>  ?=(%in -.state)
        ::  if there's a pending-vane ack, is always +(last-acked)
        ?>  =(%.y pending-ack.state)
        =/  seq=@ud  +(last-acked.state)
        =.  last-acked.state  +(last-acked.state)
        =?  nax.state  ?=(^ error)
          =?  nax.state  (gth seq 10)
            ::  only keep the last 10 nacks
            ::
            (~(del by nax.state) (sub seq 10))
          (~(put by nax.state) seq u.error)
        ::  XX emit ack to unix
        fo-core
      ::
      :: +|  %internals
      ::
      --
    ::
    ++  ev  ::  XX ++me?
      |_  hen=duct
      ::
      ++  ev-make-mess
        |=  [p=spar q=(unit path)]
        ^-  [(list move) axle]
        =/  per  (~(gut by peers.ax) ship.p *peer-state)  :: XX alien-agenda
        ?>  ?=([%known *] per)  ::  XX alien agenda
        ?^  res=(~(get by pit.per) path.p)
          :: XX check that payload is the same
          =.  for.u.res   (~(put in for.u.res) hen)
          =.  pit.per     (~(put by pit.per) path.p u.res)
          =.  peers.ax    (~(put by peers.ax) ship.p per)
          [~ ax]
        ::
        ::  XX resolve path to validate
        ::
        =/  res=(unit (unit cage)) :: (rof ~ /ames/foo [[our ...] u.q])
          ``message/!>(*@ud)  :: XX retrieve payload from loads.state
        ::   (rof ~ /ev-make-mess %a [our %$ ud+1] (need q))
        ?.  ?=([~ ~ %message *] res)
          !! :: XX wat do?
        ::
        =|  new=request-state
        =.  for.new  (~(put in for.new) hen)
        =.  pay.new  q
        =.  peers.ax  (~(put by peers.ax) ship.p per(pit (~(put by pit.per) path.p new)))
        ::
        ::  XX construct and emit initial request packet
        ::
        =/  =pact:pact
          =/  nam
            [ship.p *rift path.p 13 0] :: XX rift from peer-state
          ?~  q
            [%peek nam]
          ::  XX if path will be too long, put in [tmp] and use that path
          ::  =/  has  (shax u.u.res)
          ::  =.  tmp.ax  (~(put by tmp.ax) has [%some-envelope original-path u.u.res])
          ::  //ax/[$ship]//1/temp/[hash]
          =/  man
            [our *rift u.q 13 0]      :: XX our rift
          [%poke nam man *data:pact]  :: XX first-fragment or auth from payload
        ::
        ::(req-emit unix-duct.ax %give %send ~ blob=0)  :: XX use en:pact for blob
        :: ~&  >>>  pact/(encode-packet pact)
        :_  ax
        [unix-duct.ax %give %send ~ blob=0]~
      ::
      ++  ev-make-peek
        |=  p=spar
        (ev-make-mess p ~)
      ::
      ++  ev-make-poke
        |=  [p=spar q=path]
        (ev-make-mess p `q)
      --
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
    ::  XX  handle error notifications
    ::
    ?^  dud
      !!
    ::
    ?+  -.task  !!
      %vega  `ax
      %born  sys-abet:~(sys-born ev-sys hen)
    ::
      %plea  req-abet:(~(req-poke ev-req hen) [ship plea]:task)
      %keen  req-abet:(~(req-peek ev-req hen) +>.task)  ::  XX sec
    ::  from internal %ames request
    ::
      %make-peek  (~(ev-make-peek ev hen) p.task)
      %make-poke  (~(ev-make-poke ev hen) p.task q.task)
    ::  XX
    ::
      %hear  res-abet:(call:pa:~(. ev-res hen) [p q]:task)
      %mess  res-abet:(call:ma:~(. ev-res hen) [p q]:task)
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
      :: [%jael %turf *]          sys-abet:(~(on-take-turf ev-sys hen) turf.sign)
      :: [%jael %private-keys *]  sys-abet:(~(on-priv ev-sys hen) [life vein]:sign)
      :: [%jael %public-keys *]   sys-abet:(~(on-publ ev-sys hen) wire public-keys-result.sign)
    ::  vane (n)ack
    ::
      [@ %done *]  res-abet:(take:ma:~(. ev-res hen) wire %done error.sign)
    ::
    ::  vane gifts
    ::
      [@ %boon *]  req-abet:(take:~(. ev-req hen) wire %boon payload.sign)
    ::
    ::  network responses: acks/poke payloads
    ::                     reentrant from %ames (either message or packet layer)
    ::
      [%mesa %response *]
    ::
      =<  res-abet
      ::  XX  check the wire here if this is internal (ack) or external (payload)
      %.  [wire %response +>.sign]
      ?+  wire   ~|  %mesa-evil-response-wire^wire  !!
        [%flow @ @ @ @ %int ~]  take:ma:~(. ev-res hen)  ::  %ack
        [%flow @ @ @ @ %out ~]  !!  :: take:pa:~(. ev-res hen)  ::  %poke payload
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
::  XX import scry endpoints from lib/dire
::
++  scry
  |=  [lyc=gang pov=path car=term bem=beam]
  ^-  (unit (unit cage))
  =,  ax
  =*  ren  car
  =*  why=shop  &/p.bem
  =*  syd  q.bem
  =*  lot=coin  $/r.bem
  =*  tyl  s.bem
  ?:  ?&  =(&+our why)
          =([%ud 1] r.bem)
          =(%$ syd)
          =(%x ren)
      ==
    =>  .(tyl `(pole knot)`tyl)
    ?+    tyl  ~
    ::
        [%mess rift=@ path=^]
      ?.  =(rift (slav %ud rift.tyl))
        ~
      =/  bem  [[our %$ ud+1] path.tyl]
      =/  res  (rof ~ /ames/mess %ax bem)
      ?.  ?=([~ ~ %message *] res)
        :: TODO validate message
        ~
      res
    ::
        [%publ life=@ path=^]
      ?.  =(life (slav %ud life.tyl))
        ~
      =/  p=(unit [[@tas @tas] beam])  (parse-inner-path our path.tyl)
      ?~  p  ~
      =/  res  (rof ~ /ames/publ u.p)
      ?-  res
        ~        ~
        [~ ~]    ``[%message !>([0x0 0])] :: sign empty response
        [~ ~ *]  ``[%message !>([0x0 0])] :: sign response
      ==
    ::
        [%chum life=@ who=@ her-life=@ encrypted-path=@]
      ?.  =(life (slav %ud life.tyl))
        ~
      =/  key=@  (get-key-for (slav %p who.tyl) (slav %ud her-life.tyl))
      =/  pat=(unit path)  (decrypt encrypted-path.tyl key)
      ?~  pat  [~ ~]
      =/  p=(unit [[@tas @tas] beam])  (parse-inner-path our u.pat)
      ?~  p  ~
      =/  res  (rof `[(slav %p who.tyl) ~ ~] /ames/chum u.p)
      ?-  res
        ~        ~
        [~ ~]    ``[%message !>([0x0 0])] :: hmac empty response
        [~ ~ *]  ``[%message !>([0x0 0])] :: hmac response
      ==
    ::
        [%shut key-id=@ encrypted-path=@]
      =/  key-idx=@  (slav %ud key-id.tyl)
      =/  key  (got:on:^chain chain key-idx)
      =/  pat=(unit path)  (decrypt encrypted-path.tyl key.key)
      ?~  pat  [~ ~]
      ?~  blk=(de-part:balk our rift life u.pat)
        [~ ~]
      ?.  (check-fine-key chain u.blk key-idx)
        [~ ~]
      =/  res  (rof [~ ~] /ames/shut (as-omen:balk u.blk))
      ?-  res
        ~        ~
        [~ ~]    ``[%message !>([0x0 0])] :: hmac empty response
        [~ ~ *]  ``[%message !>([0x0 0])] :: hmac response
      ==
    ==
  ::
  ::  only respond for the local identity, %$ desk, current timestamp
  ::
  ?.  ?&  =(&+our why)
          =([%$ %da now] lot)
          =(%$ syd)
      ==
    ~
  ::
  ::  /ax/peers/[ship]               ship-state
  ::
  ?.  ?=(%x ren)  ~
  =>  .(tyl `(pole knot)`tyl)
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
--
