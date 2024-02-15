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
        ?>  =(gum.hed gum)
        :-  ^-  pact  pac
        c
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
    ::  range:  { meta[1], her[2^1-4], rif[1-4], boq[1],    fag[1-4], typ[2^0-1],  pat[2^0-16] }
    ::  max:    { meta[1], her[16],    rif[4],   boq[1],    fag[4],   typ[2],      pat[65.536] }
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
    |%
    ::
    +|  %helpers
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
    ++  ev-validate-wire
      |=  [hen=duct =wire]
      ^-  (unit [ev-flow-wire =channel =peer-state])
      =>  .(wire `(pole iota)`(ev-pave wire))
      ?.   ?=(ev-flow-wire wire)
        ~>  %slog.0^leaf/"mesa: malformed wire: {(spud (pout wire))}"  ~
      =+  pe-core=(pe-abed-got:pe hen her.wire)
      ?:  (lth rift.wire rift.peer-state.pe-core)
        ~  ::  ignore events from an old rift
      `[wire pe-chan:pe-core]
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
          %plea  (req-plea [ship plea]:task)
          %keen  (req-peek +>.task)  ::  XX sec
        ==
      ::
      ::  should ack and payloads responses be handled here instead of by ev-res?
      ++  take
        =>  |%  +$  res-task  $:  =wire
                                  $%  $>(%boon gift:ames) :: XX
                              ==  ==
            --
        |=  ::task=res-task
            task=[=wire %boon payload=*]
        ^+  req-core
        ?~  u-bone-her=(ev-validate-wire hen wire.task)
          req-core
        =,  u.u-bone-her
        ?>  ?=(%bak dire)  :: XX add more of these checks ?
        ::(req-plea [ship $% / payload]:task)
        (req-boon-poke bone [channel peer-state] payload.task)
      ::
      +|  %internals
      ::
      ++  req-plea
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
          fo-call:(fo-abed:fo hen bone^dire=%for %outbound pe-chan:pe-core)
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
          :~  %flow      :: flow request triggered "internally"
              were=%int  ::  XX better names ?(for-acks=%int for-nax-payloads=%ext to-vane=%out)
              dire=%for  ::  %for; %plea(s) are always sent forward
              rcvr=[(scot %p ship)]
              rift=[(scot %ud rift.peer-state)]
              bone=[(scot %ud bone)]
              seq=[(scot %ud seq.gift)]
           ==
        (req-emit:co hen %pass wire %m make-poke/[spar path]:gift)
      ::
      ++  req-boon-poke  :: XX refactor with req-plea
        |=  [=bone pe-chan=[=channel =peer-state] load=*]
        ::  XX handle corked/closing bones
        ::
        ~&  req-boon-poke/load
        =^  [gifts=(list [seq=@ud spar path]) moves-flow=_moves]  ax
          =<  fo-abut
          (fo-call:(fo-abed:fo hen bone^dire=%bak %outbound pe-chan) boon/load)
        ::
        ::  XX this can be done internally in fo-call:fo-core
        %+  roll  gifts
        |=  [gift=[seq=@ud =spar =path] co=_(req-emil moves-flow)]  :: XX =gift
        ::  XX %ames call itself with a %make-poke tasks
        ::  on a wire used to infer the listener (the %poke %plea request; this)
        ::  when getting the %response $page with the %ack (tagged with %int)
        ::  and similarly for %boon payloads (tagged with %ext)
        ::
         =/  =wire  ::  (fo-make-wire %int seq.gift)
          :~  %flow      :: flow request triggered "internally"
              were=%int  ::  XX better names ?(for-acks=%int for-nax-payloads=%ext to-vanes=%out )
              dire=%bak  ::  %boon(s) always sent backward
              rcvr=[(scot %p her.channel.pe-chan)]
              rift=[(scot %ud rift.peer-state.pe-chan)]
              bone=[(scot %ud bone)]
              seq=[(scot %ud seq.gift)]
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
      +$  res-mess-pith
        $:  ::[%ud rift=@ud]  :: XX ?
            [%p sndr=@p]
            ?(%poke %ack)
            [%p rcvr=@p]
            %flow
            [%ud bone=@ud]
            dire=?(%for %bak)
            [%ud mess=@ud]
            ~
        ==
      ::  /ax/~snip//ver=1/mess/rift=1/pact/bloq=13/init/[...]
      ::  /ax/~snip//ver=1/mess/rift=1/pact/bloq=13/pure/auth/frag=1/[...]
      ::  /ax/~snip//ver=1/mess/rift=1/pact/bloq=13/pure/data/frag=1/[...]
      ::
      +$  res-pact-head
        $:  %ax
            [%p her=@p]
            %'1'
            %mess
            [%ud rift=@ud]
            %pact
            [%ud bloq=@ud]
            inner=?(%init %pure)
            *  ::  if %pure, $res-pure-pith
        ==
      ::
      +$  res-pure-pith
        $:  ?(%auth %data)
            [%ud frag=@ud]
            *
        ==
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
          ?.  =(our her.name)
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
          ^+  res-core
          ::  XX initialize message core
          ::
          :: ma-abet:ma-hear:(ma hen p.p.pact)
          :: (each (list move) mess)
          ::
          ::  check for pending request (peek|poke)
          ::
          =*  ship  her.name
          ?~  per=(~(get by peers.ax) ship)
            res-core
          ?>  ?=([~ %known *] per)  ::  XX alien agenda
          ?~  res=(~(get by pit.u.per) pat.name)
            res-core
          ::
          =/  [typ=?(%auth %data) fag=@ud]
            ?~  wan.name
              [?:((gth tot.data 4) %auth %data) 0]
            [typ fag]:wan.name
          ?:  =(0 fag)
            ?-    typ
                %auth
              ?.  ?|  ?=(~ ps.u.res)
                      ?=([%& %auth] nex.u.ps.u.res)
                  ==
                res-core
              ::  XX cut+validate sig/hmac
              =|  root=@ux   :: XX compute from proof
              =/  proof=(list @ux)  (rip 8 dat.data)
              ?~  state=(init:verifier:lss tot.data root proof)
                res-core
              =.  ps.u.res
                ?~  ps.u.res
                  `[[%| 0] tot.data u.state]
                ps.u.res(los.u u.state)
              ::
              ::  XX request next fragment
              ::
              !!
            ::
                %data
              ?.  =(~ ps.u.res)
                res-core
              ::
              ?:  =(1 tot.data)    :: complete
                ::  XX produce as message w/ auth tag
                !!
              ::
              ?:  (gth tot.data 4) :: auth-packet-needed
                ::  XX authenticate at message level with given root hash
                ::  XX request-auth-packet
                ::     by setting %auth in ps.request-state, regenerating next packet
                !!
              ::  proof is inlined
              ::  XX cut+validate sig/hmac
              =/  proof=(list @ux)
                =>  aut.data
                ?>  ?=([%0 *] .)
                ?~(q ~ ?@(u.q [u.q ~] [p q ~]:u.q))
              =.  proof  [(leaf-hash:lss fag dat.data) proof]
              =|  root=@ux :: XX compute from proof + leaf
              ?~  state=(init:verifier:lss tot.data root proof)
                res-core
              =.  ps.u.res  `[[%| 1] tot.data u.state]
              ::
              ::  XX request next fragment
              !!
            ==
          ::
          ?:  ?=(%auth typ)  :: auth packets for subsequent fragments are not requested
            res-core
          ?~  ps.u.res
            res-core
          ?.  &(=(13 boq.name) ?=(%| -.nex.u.ps.u.res) =(p.nex.u.ps.u.res fag))
            res-core
          ::
          =/  pair=(unit [l=@ux r=@ux])
            ?~  aut.data  ~
            `?>(?=([%1 *] .) p):aut.data
          =/  msg  (met 3 dat.data)^dat.data
          ?~  state=(verify-msg:verifier:lss los.u.ps.u.res msg pair)
            res-core
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
          ~|  +.take
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
          =/  ack=(pole iota)  (ev-pave path.ack-spar)
          =/  pok=(pole iota)  (ev-pave path.pok-spar)
          ~|  path-validation-failed/path.ack-spar^path.pok-spar
          ?>  &(?=(res-mess-pith ack) ?=(res-mess-pith pok))
          ::
          :: =/  [sndr=@p rcvr=@p]  [ship.pok-spar ship.ack-spar]  :: XX validated in the packet layer?
          ?.  =(sndr.ack our)  ::  do we need to respond to this ack?
            ~&  >>  %not-our-ack^sndr.ack^our
            res-core
          ?.  =(rcvr.pok our)  ::  are we the receiver of the poke?
            ~&  >  %poke-for-other^[rcvr.pok our]
            res-core
          =/  ship-state  (~(get by peers.ax) sndr.pok)
          ?.  ?=([~ %known *] ship-state)
            ::  XX handle
            !!
          ::
          =*  peer-state  +.u.ship-state
          =+  pe-core=(pe-abed-her:pe hen sndr.pok peer-state)
          ::
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
          =^  moves  ax
            =<  fo-abet
            %.  [%sink mess.pok req]
            fo-call:(fo-abed:fo hen bone.pok^dire %incoming pe-chan:pe-core)
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
          |=  [=wire load=[%page sage:mess]]
          ^+  res-core
          ::  XX same as ma-poke-done; move to helper arm ?
          ?~  u-bone-her=(ev-validate-wire hen wire)
            res-core
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
          ::  proessing an ack or a payload
          ::
          ::
          =^  moves  ax
            =<  fo-abet
            ::  XX parse $ack payload in here, and call task instead?
            %.  [were response/[seq +.load]]
            fo-take:(fo-abed:fo hen bone^dire %outbound channel peer-state)
          (res-emil moves)
        ::
        ++  ma-poke-done
          |=  [=wire error=(unit error)]
          ^+  res-core
          ?~  u-bone-her=(ev-validate-wire hen wire)
            res-core
          ::  XX use $pith for this
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
            fo-take:(fo-abed:fo hen bone^dire=%bak %incoming channel peer-state)
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
      |_  [[hen=duct =side peer-channel] state=flow-state]
      ::
      +*  veb    veb.bug.channel
          her    her.channel
          keens  keens.peer-state
          pe-ca  [channel peer-state]
          bone   bone.side
          dire   dire.side
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
        |=  [=duct =^side part=?(%incoming %outbound) peer-channel]
        ~?  (~(has by flows.peer-state) side)
          part^state/state
        =.  state  (~(gut by flows.peer-state) side *flow-state)
        fo-core(hen duct, side side, channel channel, peer-state peer-state)
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
      ++  fo-ack-path  |=([seq=@ud =dyad] (fo-path seq %ack dyad))
      ++  fo-pok-path  |=([seq=@ud =dyad] (fo-path seq %poke dyad))
      ++  fo-mop       ((on ,@ud mesa-message) lte)
      ::
      +|  %builders
      ::
      ++  fo-path
        |=  [seq=@ud path=?(%ack %poke) dyad]
        ::  %+  fo-en-spac  %publ  ::  XX %publ, %chum, %chut ?
        ^-  ^path
        :~  reqr=(scot %p sndr)     path  rcvr=(scot %p rcvr)
            %flow  (scot %ud bone)  dire  (scot %ud seq)
        ==
      ::  XX use instead of manual construction
      ::
      ++  fo-make-wire
        |=  [were=?(%int %ext %out) seq=@ud]  ::  XX better names ?(for-acks=%int for-nax-payloads=%ext to-vane=%out)
        ^-  wire
        ::  %for: %plea(s) are always sent forward, %boon(s) %bak
        :~  %flow  were  dire
            rcvr=[(scot %p her)]
            rift=[(scot %ud rift.peer-state)]
            bone=[(scot %ud bone)]
            seq=[(scot %ud seq)]
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
          ?(%plea %boon %cork)  (fo-send-poke poke)
          ::  XX responses: (n)acks
          ::
            %sink
          ~|  mess.poke
          ::  a %plea sinks on the backward receiver (from a forward flow)
          ::  a %boon sinks on the forward receiver (from a backward flow)
          ::
          ?-  dire
            %bak  ?>(?=(%plea -.mess.poke) (fo-sink-plea [seq +.mess]:poke))
            %for  ?>(?=(%boon -.mess.poke) (fo-sink-boon [seq +.mess]:poke))
          ==
          ::  use the -.mess instead?
          :: ?+  -.mess  !!
          ::   %plea  (fo-sink-plea seq +.mess)
          ::   %boon  (fo-sink-boon seq +.mess)
          ::   ::  %cork  (fo-sink-cork seq +.mess)
          :: ==
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
                        %ext  !!  ::  (fo-take-load +.sign)
                        %int  (fo-take-ack +.sign)
        ==            ==
      ::
      +|  %tasks
      ::
      ++  fo-send-poke
        |=  poke=mesa-message
        ::  XX can we (re)use a sequence number after poping a previous one
        ::  off the queue?
        ::
        :: ?>  ?=(%outbound -.state)
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
        ::  =>  ?>(?=(%outbound -.state) .)
        =+  loads=loads.state  ::  cache
        |-  ^+  fo-core
        =*  loop  $
        =+  num=(wyt:fo-mop loads)
        ?:  =(0 num)
          fo-core
        ?.  (lte num send-window.state)
          fo-core
        :: =/  [[seq=@ud request=mesa-message] load=_loads.state]
        =^  [seq=@ud request=mesa-message]   loads
          (pop:fo-mop loads)
        :: ?>  ?=(%plea -.request)  :: XX handle %cork
        :: ~!  +.state
        =.  send-window.state  (dec send-window.state)
        :: XX FIXME
        :: =.  fo-core  fo-core
          :: %-  fo-give  ^-  gift
        ::  XX don't use gifts
        =.  gifts  :_  gifts  ^-  gift
          [seq her^(fo-ack-path seq her our) (fo-pok-path seq our her)]
        loop
      ::
      ++  fo-sink-boon
        |=  [seq=@ud message=*] :: XX =error
        ^+  fo-core
        :: ?>  ?=(%incoming -.state)
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
              ::  both .to-vane and .dire are asserted when receiving the vane %ack
              ::  since they will always be %out and %bak
              %out  ::  XX better names ?(for-acks=%int for-nax-payloads=%ext to-vane=%out)
                    ::  XX move to begining of wire
              dire  ::  %bak; %plea(s) always sink backward
                    ::  used internally when receiving vane acked
              [(scot %p her)]
              [(scot %ud rift.peer-state)]
              [(scot %ud bone)]
              [(scot %ud seq)]    ::  XX  seq not needed  :: XX $% ?
          ==
        ?:  =(vane.plea %$)
          fo-core  ::  XX handle pre-cork ships
                   ::  XX maybe when checking path/protocol version
        =.  fo-core
          ?+  vane.plea  ~|  %mesa-evil-vane^our^her^vane.plea  !!
            ?(%c %e %g %j)  (fo-emit hen %pass wire vane.plea plea/her^plea)
          ==
        ::
        :: ?>  ?=(%incoming -.state)
        fo-core(pending-ack.state %.y)
      ::
      ++  fo-take-ack
        |=  [seq=@ud =spar auth:mess =gage:mess]
        ^+  fo-core
        :: ?>  ?=(%outbound -.state)
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
        =+  ;;(error=? +.gage)  ::  XX
        ::  XX FIXME: have.?(%bak %for) -need.%for
        ?:  error
          ::  XX if error start %peek for naxplanation
          =/  =wire  (fo-make-wire %ext seq)
          fo-core
        =/  =duct
          :: XX +pe-got-duct
          ~|(%dangling-bone^her^bone (~(got by by-bone.ossuary.peer-state) bone))
        (fo-emit duct %give %done ~)
      ::
      ++  fo-take-done
        |=  error=(unit error)
        ^+  fo-core
        :: ?>  ?=(%incoming -.state)
        ::  if there's a pending-vane ack, is always +(last-acked)
        ::
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
        ::(req-emit unix-duct.ax %give %send ~ blob=0)  :: XX use  (en:^pact pact) for blob
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
      %plea  req-abet:(~(req-plea ev-req hen) [ship plea]:task)
      %keen  req-abet:(~(req-peek ev-req hen) +>.task)  ::  XX sec
    ::  from internal %ames request
    ::
      %make-peek  (~(ev-make-peek ev hen) p.task)
      %make-poke  (~(ev-make-poke ev hen) p.task q.task)
    ::  XX
    ::
      %hear  res-abet:(call:pa:~(. ev-res hen) [p q]:task)
      %mess  res-abet:(call:ma:~(. ev-res hen) [p q]:task)  ::  XX acks go direclty here
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
      =/  response-pith  `(pole iota)`(ev-pave wire)
      =<  res-abet
      ::  XX  check the wire here if this is internal (ack) or external (naxplanation payload)
      %.  [wire %response +>.sign]
      ?+    response-pith   ~|  %mesa-evil-response-wire^wire  !!
          ::  %acks come directly into the message layer since they are always one
          ::  packet, and then given back to the flow layer that called them
          ::
          ev-flow-wire
        ?:  ?=(%out were.response-pith)  !!  ::  %naxplanation payload
        take:ma:~(. ev-res hen)              ::  %ack
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
        ``[%atom !>(q:(fax (en:pact pac)))]
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
--
