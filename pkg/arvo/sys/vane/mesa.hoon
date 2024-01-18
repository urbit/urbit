!:
=,  ames
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
          +$  data  [tot=@udF aut=@ux dat=@]
          +$  lane  $@  @ux
                    $%  [%if p=@ifF q=@udE]
                        [%is p=@isH q=@udE]
                    ==
          +$  next  (list lane)
          +$  pact  $%  [%page p=name q=data r=next]
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
        |=  [tot=@udF aut=@ux dat=@]
        ^-  plot
        =/  mot  (met 3 (end 5 tot))
        =/  mut  ?:(=(0 aut) 0 1)
        =/  lut  (end 3 (met 3 aut))
        =/  len  (met 3 dat)
        =/  men  (met 3 len)
        :+  bloq=3
          [s+~ 0 [2 (dec mot)] [1 mut] [5 men] ~]
        [[mot tot] [mut lut] [lut aut] [men len] [len dat] ~]
      ::
      ++  de
        |=  a=bite
        =/  b=[bloq step]  [0 ?@(a 0 (rig [bloq.a 0] step.a))]
        |=  dat=@
        ^-  [[tot=@udF aut=@ux dat=@] bloq step]
        =+  ^=  [[bot mut men] b]  ((hew b dat) [2 1 5])
        =+  ^=  [len nex]          [(rig [bloq.b 3] step.b) +(bot)]
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
::  helper core
::
=>  |%
    +|  %helpers
    ::
    ++  chain
      =<  mop
      |%
      ++  on   ((^on ,@ ,[key=@ =path]) lte)
      +$  mop  ^chain
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
    ++  get-key-for
      |=  [who=ship life=@]
      0x0
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
    ++  is-first-fragment  |
    ++  auth-packet-needed  |
    ++  complete  |
    ++  is-auth-packet  |
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
      $%  $:  %a
              $>(?(%deep %keen) task:ames)
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
    +$  gift
    ::+$  client-gift  :: gifts emitted when sending requests/responses
      $%  ::gift:ames
          [%send =lane =blob]                  :: old packet send
          [%push lanes=(list lane) =blob]      :: new packet send
          [%pres lanes=(list lane) message=*]  :: new message send
    ::  ==
    ::+$  publisher-gift  :: gifts emitted when hearing requests
      ::$%
          [%boon message=*]                    :: assembled %boon
          [%done error=(unit error)]           :: ack to client vane
      ==
    +$  mesa-message
      $%  [%plea plea]       ::  client vane remote request
          [%boon payload=*]  ::  %facts, subscription updates
          [%cork ~]          ::  client vane is done, close flow
      ==
    +$  message-task
      $%  [%poke =bone message=mesa-message]
          [%sink =bone =mess]  ::  message=mess
      ==
    ::
    +$  sig    @uxJ    :: (ed25519)
    +$  hmac   @uxI   :: (keyed blake3)
    +$  proof  (list @uxI)
    +$  root  @uxI
    +$  once  [tot=@udF tag=?(sig hmac) aut=?(root proof) dat=@]
    +$  more  [aut=$@(~ [@uxI @uxI]) dat=@]
    +$  spac  ?(%pact %publ %mess %chum %shut)
    +$  name  [p=ship q=path r=bloq s=num=@udF]
    +$  data  [tot=@udF aut=@ux dat=@]
    +$  next  (list lane)
    +$  pact
      $%  [%page p=name q=data r=next]  :: [%page p=name q=(each once more) r=next]
          [%peek p=name]
          [%poke p=name q=name r=data]  :: [%poke p=name q=name r=once]
      ==
    ::
    +$  mess
      $%  [%page p=spar:ames q=page]        :: XX need auth data on %page and %poke
          [%peek p=spar:ames]
          [%poke p=spar:ames q=spar:ames r=page]
      ==
    ::
    +$  message-state
      $:  for=(set duct)
          ps=(unit packet-state)
          :: XX put poke payload or path to it here
      ==
    ::
    +$  packet-state
      $:  nex=(each %auth fragment=@ud)
      ==
    ::
    +$  ship-state
      $+  ship-state
      $%  [%known new-peer-state]
          $<(%known peer-state:ames)
      ==
    ::
    +$  peer-state  new-peer-state
    +$  new-peer-state
      $+  peer-state
      $:  $:  =symmetric-key
              =life
              =rift
              =public-key
              sponsor=ship
          ==
          route=(unit [direct=? =lane])  ::  XX (list)
          =qos
          =ossuary
          snd=(map bone message-pump-state)
          rcv=(map bone message-sink-state)
          nax=(set [=bone =message-num])     ::  XX never used
          heeds=(set duct)
          closing=(set bone)
          corked=(set bone)
          keens=(map path keen-state)
          =chain
          pit=(map path message-state)
      ==
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
    |%
    ::
    +|  %helpers
    ::
    ++  ev-get-them
      |=  =mess
      ^-  [sndr=(unit ship) rcvr=ship]  :: XX dyad?
      ?-  -.mess
        %peek  [~ ship.p.mess]
        %page  [~ ship.p.mess]
        %poke  [`ship.q.mess ship.p.mess]
      ==
    ::
    ++  ev-is-flow-path
      |=  path=(pole knot)
      ^-  ?
      ?=([sndr=@ vane=%$ rcvr=@ %flow bone=@ message=@] path)
    ::
    ++  ev-get-bone
      |=  =mess
      |^  ^-  bone
      ?:  ?=(%poke -.mess)
        (parse-path `(pole knot)`path.q.mess)
      :: ?>  ?=(?(%page %peek) -.pact)
      *bone  :: XX
      ::=/  =name  p.pact
      ::(parse-path `(pole knot)`q.name)
      ::
      ++  parse-path
        |=  path=(pole knot)
        ^-  bone
        ?>  ?=([%ax her=@ vane=%$ ver=@ spac=@ rift=@ inner-path=*] path)
        ?>  =(ver.path '1')
        :: ?>  =(spac spac.path)
        ?>  ?=([reqr=@ ?(%ack %poke) rcvr=@ %flow bone=@ *] inner-path.path)
        (rash bone.inner-path.path dem)
      --
    ::
    +|  %entry-points
    ::
    ++  ev-req  ::  send %peek/%poke requests
      =|  moves=(list move)
      ~%  %event-gate  ..ev-req  ~
      |_  =duct  ::  XX hen=duct
      :: ~%  %ev-core  ..$  ~
      +|  %helpers
      ::
      ++  ev-core  .
      ++  ev-abet  (flop moves)^ax
      ++  ev-emit  |=(=move ev-core(moves [move moves]))
      ++  ev-emil  |=(mos=(list move) ev-core(moves (weld (flop mos) moves)))
      ::
      +|  %tasks
      ::
      ++  on-born  ev-core(ax ax(unix-duct duct))  ::  XX move to ev-sys
      ++  on-poke
        |=  [=ship =plea]
        ^+  ev-core
        =/  ship-state  (~(get by peers.ax) ship)
        ::
        ?.  ?=([~ %known *] ship-state)
          ::  XX handle
          !!
        ::
        =*  peer-state  +.u.ship-state
        =+  peer-core=(pe-abed-her:pe duct ship peer-state)
        ::
        =^  =bone  peer-core  (pe-bind-duct:peer-core duct)
        ::  XX handle corked/closing bones
        ::
        =^  moves  peer-state
          pe-abet:(pe-call:peer-core %mess %poke bone %plea plea)  ::  XX namespace
        =.  peers.ax
          ::  XX already done in pe-abet
          (~(put by peers.ax) ship known/peer-state)
        (ev-emil moves)
      ::
      ++  on-peek
        |=  p=spar:ames
        ^+  ev-core
        :: =/  ship-state  (~(get by peers) ship.p)
        :: :: ::
        :: ?.  ?=([~ %known *] ship-state)
        ::   ::  XX handle
        ::   !!
        :: :: ::
        =+  peer-core=(pe-abed:pe duct ship.p)
        =*  peer-state  peer-state.peer-core
        :: ::
        ?^  ms=(~(get by pit.peer-state) path.p)
          =.  peers.ax
            =/  pit
              (~(put by pit.peer-state) path.p u.ms(for (~(put in for.u.ms) duct)))
            (~(put by peers.ax) ship.p known/peer-state(pit pit))
          ev-core
        =|  new=message-state
        =.  for.new  (~(put in for.new) duct)
        =.  peers.ax
          %+  ~(put by peers.ax)  ship.p
          known/peer-state(pit (~(put by pit.peer-state) path.p new))
        ::  XX construct and emit initial request packet
        ::
        ev-core
      :: +|  %internals
      ::
      --
    ::
    ++  ev-res  ::  hear %pact/%mess requests, send response
      =|  moves=(list move)
      |_  =duct  ::  XX hen=duct
      ::
      ++  res-core  .
      ++  res-abet  [(flop moves) ax]
      ++  res-emit  |=(=move res-core(moves [move moves]))
      ++  res-emil  |=(mos=(list move) res-core(moves (weld (flop mos) moves)))
      ++  res-pact  :: XX use +abet
        |=  [=lane:^pact blob=@]
        ::^-  [(list move) axle]
        ^+  res-core
        =/  pac  (parse-packet blob)
        ~!  peers.ax
        ?-  -.pac
            %page
          ::
          ::  XX initialize message core
          ::
          :: pa-abet:pa-hear:(pa hen p.p.pact)
          :: (each (list move) mess)
          ::
          ::  check for pending request (peek|poke)
          ::
          =*  ship  p.p.pac
          ?~  rs=(~(get by peers.ax) ship)
            :: [~ ax]
            res-core
          ?>  ?=([~ %known *] rs)  ::  XX alien agenda
          ?~  ms=(~(get by pit.u.rs) r.p.pac)
            :: [~ ax]
            res-core
          ::
          ?:  is-first-fragment
            ?^  ps.u.ms
              ::[~ ax]
              res-core
            ::  XX authenticate at message level
            ::  XX initialize hash-tree
            ::
            ?:  auth-packet-needed
              ::  XX request-auth-packet
              !!
            ?:  complete
              ::  XX produce as message
              !!
            ::
            ::  XX request next fragment
            !!
          ::
          ?~  ps.u.ms
            :: [~ ax]
            res-core
          ?:  is-auth-packet
            ?.  ?=(%auth nex.u.ps.u.ms)
              ::[~ ax]
              res-core
            ::  XX initialize hash-tree
            ::  XX request next fragment
            !!
          ::
          ?.  &(=(13 s.p.pac) ?=(%| -.nex.u.ps.u.ms) =(p.nex.u.ps.u.ms t.p.pac))
            :: [~ ax]
            res-core
          ::  XX get hash pair from packet, validate and add to tree
          ::  XX validate fragment
          ::  XX persist fragment
          ::
          ?:  complete
            ::  XX produce as message
            !!
          ::  XX request next fragment
          !!
        ::
            %peek
          ?.  =(our p.p.pac)
            :: [~ ax]
            res-core
          =/  res=(unit (unit cage))
            !!  :: scry for path
          ?.  ?=([~ ~ ^] res)
            :: [~ ax]
            res-core
          ::  XX [%give %send-response q.q.u.u.res]
          :: [~ ax]
          res-core
        ::
            %poke
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
          ::  XX  wait for done from vane
          ::  XX  then, place ack in namespace,
          ::  XX  emit $page as an effect to vere to cache it
          ::  XX  wait for cork to clean the flow
          !!
        ==
      ::
      ++  res-mess  :: XX use +abet
        |=  [(unit lane:^pact) =^mess]
        ^+  res-core
        ?-  -.mess
            %page
          =*  ship  ship.p.mess
          ?~  rs=(~(get by peers.ax) ship.p.mess)
            :: [~ ax]
            res-core
          ?>  ?=([~ %known *] rs)  ::  XX alien agenda
          ?~  ms=(~(get by pit.u.rs) path.p.mess)
            ::[~ ax]
            res-core
          ::
          ::  XX validate response
          ::  XX give to all ducts in [for.u.ms]
          ::
          ::  [%give %response mess]
          ::
          ::  XX abet
          =.  pit.u.rs  (~(del by pit.u.rs) path.p.mess)
          =.  peers.ax  (~(put by peers.ax) ship.p.mess u.rs)
          ::[~ ax(p (~(put by peers) ship.p.mess u.rs))]
          res-core
        ::
            %peek
          ?.  =(our ship.p.mess)
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
            %poke
          ::  XX dispatch/hairpin &c
          ::
          ::  - check that we recognize ack-path
          ::  - validate inner payload message
          ::  - route message to inner module (ie, flow)
          ::
          ::
          ::  XX  fake decoding
          :: /~nec/ack/~zod/flow/0/1/1
          :: /~zod/poke/~nec/flow/0/1/1
          =/  [sndr=(unit @p) rcvr=@p]  (ev-get-them mess)
          ?.  =(rcvr our)
            res-core  ::  XX no-op?
          =/  ship-state  (~(get by peers.ax) (need sndr))
          ?.  ?=([~ %known *] ship-state)
            ::  XX handle
            !!
          ::
          =*  peer-state  +.u.ship-state
          =+  peer-core=(pe-abed-her:pe duct (need sndr) peer-state)
          =/  =bone  (ev-get-bone mess)
          =^  moves  peer-state
            pe-abet:(pe-call:peer-core %mess %sink bone %poke +.mess)
          =.  peers.ax
            ::  XX already done in pe-abet
            (~(put by peers.ax) (need sndr) known/peer-state)
          (res-emil moves)
        ==
      --
    ::
    ++  ev-sys  !!  ::  system/internal: %heed, %kroc, %prod...
    ::
    +|  %internals
    ::  +pe: per-peer processing
    ::
    ++  pe
      =|  moves=(list move)
      |_  [=duct =channel =peer-state]
      +*  veb    veb.bug.channel
          her    her.channel
          keens  keens.peer-state
          pe-ca  [channel peer-state]
      ::
      +|  %helpers
      ++  pe-core      .
      ++  pe-emit      |=(=move pe-core(moves [move moves]))
      ++  pe-emil      |=(mos=(list move) pe-core(moves (weld (flop mos) moves)))
      ++  pe-abed      |=([d=^duct s=@p] (pe-abed-her d s (pe-gut-her-state s)))
      ++  pe-abed-got  |=([d=^duct s=@p] (pe-abed-her d s (pe-got-her-state s)))
      ++  pe-abed-her
        |=  [=^duct =ship peer=^peer-state]
        %_  pe-core
                duct   duct
          peer-state   peer
              channel  [[our ship] now pe-channel -.peer]
        ==
      ::
      ++  pe-channel  [life crypto-core bug]:ax
      :: ++  pe-abort    pe-core  :: keeps moves, discards state changes
      ++  pe-abet
        ^+  [moves peer-state]
        =.  peers.ax  (~(put by peers.ax) her known/peer-state) ::  XX outside?
        [moves peer-state]
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
        |=  b=bone
        ^-  ^duct
        ~|(%dangling-bone^her^b (~(got by by-bone.ossuary.peer-state) b))
      ::
      ::  +pe-bind-duct: find or make new $bone for .duct in .ossuary
      ::
      ++  pe-bind-duct
        |=  =^duct
        =*  ossa  ossuary.peer-state
        ^+  [next-bone.ossa pe-core]
        ?^  existing=(~(get by by-duct.ossa) duct)
          [u.existing pe-core]
        :-  next-bone.ossa
        =.  ossa
          :+  (add 4 next-bone.ossa)  ::  XX  4 bones needed per flow?
            (~(put by by-duct.ossa) duct next-bone.ossa)
          (~(put by by-bone.ossa) next-bone.ossa duct)
        pe-core
      ::
      +|  %tasks
      ::
      ++  pe-call
        |=  [=spac task=message-task]  ::  XX any namespace task?
        ^+  pe-core
        =|  poke-state=*       ::  XX from state
        =^  moves  poke-state  ::  XX ... save in state
          =<  fo-abet
          ?-  -.task
            %poke  (fo-call:(fo-abed:fo duct bone.task pe-ca) spac message.task)
            %sink  (fo-call:(fo-abed:fo duct bone.task pe-ca) spac sink/mess.task)
          ==
        (pe-emil moves)
      ::
      :: +|  %internals
      --
    ::
    ++  fo  ::  ++flow (fo) ||  ++bone (bo)
        =>  |%
            :: XX to lull.hoon,
            ::  - part of peer-state => (map bone poke-state)
            ::  - reshape message-pump/sink states into poke-states
            ::     using the bone to know the directtion of the flow ?
            ::     (bone numbers as see from the point of view of "our")
            ::
            ::       bone 0: sub (our) -> pub (her)  :: %plea: %poke, %watch, ...
            ::       bone 1: sub (her) <- pub (our)  :: %plea: %boon
            ::       bone 2: sub (our) <- pub (her)  :: %nacks
            ::
            ::       bone 3: sub (her) <- pub (our)  :: orphaned; naxplanations are read via %peek
            ::
            ::  XX -- TODO simplify packet-state for each message (next=@ud)
            ::
            ::  - packet-pump-state in message-pump gone; moved ouf ot arvo into vere
            ::    - collapsed into the message level since we only "send" one packet fragment
            ::    - XX only next-wake for backing off resends of expired packets
            ::  - message-sink-state only deals with messages, removes .partial-rcv-message
            ::
            ::  - packet state is done per message by the |pu core
            ::
            +$  new-peer-state  ::  (map her=ship peer-state)
              $:
                  pokes=(map bone poke-state)  ::  sender+receiver
                  ::
                    pokes=(map bone poke-sender-state)
                    coups=(map bone poke-receiver-state)
                  ::
              ==
            ::
            +$  peeks  (map path bone)  :: XX not needed
                                        :: poke datums for our requests
                                        :: removed when the ack is received
            +$  acks   (map path bone)  :: XX not needed
                                        :: acks to responses we give
                                        :: kept for current-10 messages
                                        :: when ack for current is heard, current-10 is removed
                                        ::  XX how to distinguinsh between first time?
                                        :: path contains the message/fragment number
            ::
            +$  poke-sender-state  :: per-bone, XX distinguish between req/resp bone?
              $:  current=_`message-num`1
                  next=_`message-num`1
                  unsent-messages=(qeu pact)
                  $+  queued-message-acks
                  queued-message-acks=(map message-num ack)  ::  XX remove?
              ==
            +$  poke-receiver-state
              $:  last-acked=message-num
                  last-heard=message-num
                  pending-vane-ack=(qeu [=message-num message=*])  ::  XX remove?
                  nax=(set message-num)
              ==
            ::
            +$  poke-state  ::  XX
              $:  poke-sender-state
                  poke-receiver-state
              ==
            ::
            +$  message-sign
              $%  [%sign ~]  ::  hear (n)ack for %poke, can trigger %peek for naxplanation
                  [%xxxx ~]
              ==
            --
        ::
        =|  moves=(list move)
        |_  [[=duct =bone peer-channel] state=poke-state]
        +*  veb    veb.bug.channel
            her    her.channel
            keens  keens.peer-state
            pe-ca  [channel peer-state]
        ::
        +|  %helpers
        ++  fo-core  .
        ++  fo-abed
          |=  [=^duct =^bone peer-channel]
          fo-core(duct duct, bone bone, channel channel, peer-state peer-state)
        ::
        ++  fo-abet  moves^state  ::  ~(put by ...)  :: XX (flop moves) done outside
        ++  fo-emit  |=(=move fo-core(moves [move moves]))
        ++  fo-emil  |=(mos=(list move) fo-core(moves (weld (flop mos) moves)))
        ::
        +|  %gifts
        ++  fo-en-plea  |=(=plea ^-(blob (jam plea)))  ::  XX real encoding
        ::  +fo-en-gift: encode gift to send based on namespacce
        ::
        ++  fo-en-gift
          |=  [=spac payload=(each blob message=*)]
          ^-  gift  ::  client-gift
          =|  lanes=(list lane)  :: XX
          ?-    spac  :: XX old packet format [%send =lane =blob]
              ?(%chum %shut %publ %mess)
            [%pres lanes ?>(?=(%| -.payload) +.payload)]
          ::
              %pact
            [%push lanes ?>(?=(%& -.payload) +.payload)]
          ==
        ::  +fo-en-spac: encode namespace paths
        ::
        ++  fo-en-spac
          |=  [=spac =path]
          ^+  path
          =/  [her=@ta rift=@ta]
            [(scot %p her) (scot %ud rift.peer-state)]
          ?-  spac
            %chum  !!
            %shut  !!
            %publ  !!
            %pact  !!
            %mess  [%ax her %$ ~.1 %mess rift path]
          ==
        ::  XX FIXME +encs: path encodings
        ::
        ++  encs
          |%  ++  her   =-  ~&(- -)  (scot %p her)
              ++  our   =-  ~&(- -)  (scot %p our)
              ++  bone  =-  ~&(- -)  (scot %ud bone)
              ++  mess  =-  ~&(- -)  |=(mess=@ud (scot %ud mess))
          --
        ::
        +|  %entry-points
        ::
        ++  fo-call
          =>  |%  +$  poke-task
                    $%  [%sink =mess]  ::  XX  [%sink =mess]
                        ::  XX remove %naxplanation from lull
                        mesa-message
                    ==
              --
          ::
          |=  [=spac poke=poke-task]
          ^+  fo-core
          ::
          ?-  -.poke
            ::  requests
            ::
            %plea  (fo-plea spac +.poke)  ::  send %plea request
            %boon  (fo-boon spac +.poke)  ::  send %boon request
            %cork  fo-cork
            ::  XX responses: (n)acks, pact/mess
            ::
            %sink  (fo-sink spac +.poke)  ::  unix responses (XX %hear?)
          ==
        ::
        ++  fo-take
          |=  sign=message-sign
          ^+  fo-core
          ::
          ?+  -.sign  !!
            %sign  fo-hear-sign
          ==
        ::
        +|  %tasks
        ::
        ++  fo-sink
          |=  [=spac =mess] ::  XX remove spac
          ?+  -.mess  !!
            %poke  (fo-sink-poke spac [p q r]:mess)
          ==
        ::
        ++  fo-sink-poke
          |=  ::[=spac ack=name flow=name payload=data]  ::  XX remove spac
              [=spac =spar:ames =spar:ames =page]
          ^+  fo-core
          ::  receiver of a %poke request
          ::
          ::  XX parse `path`q.q.poke and check the bone to know if plea or boon
          ::
          ?>  ?=(%noun -.page)  :: XX ??
          =+  ;;(=plea +.page)
          ::  XX  pass plea/boon to gall
          :: =+  [her=~nec spac=%mess bone=1 rift=1]
          :: add rift to avoid dangling bones
          ::
          =/  =wire  /[spac]/flow/[(scot %p her)]/[(scot %ud bone)]/[(scot %ud rift.peer-state)]
          =.  fo-core
            ?:  =(vane.plea %$)
              fo-core  ::  XX handle pre-cork ships
                       ::  XX taken care before, when checking protocol version
            ?+  vane.plea  ~|  %ames-evil-vane^our^her^vane.plea  !!
              ?(%c %e %g %j)  (fo-emit duct %pass wire vane.plea plea/her^plea)
            ==
          fo-core
        ::
        ++  fo-boon
          |=  [=spac payload=*]
          fo-core
        ::
        ++  fo-cork  !!
        ::  +poke-plea: XX
        ::
        ++  fo-plea
          |=  [=spac =plea]
          ^+  fo-core
          ::
          ::  /ax/[$ship]//1/pact/[rift]/[$bloq]/[$frag]/data/[..$path]
          ::  /ax/[$ship]//1/mess/[rift]/[..$path]
          ::  /ax/[$ship]//1/chum/[ship-life]/[who]/[who-life]/[encrypted-path]
          ::
          ::  $path = /[$her]/ack/[$our]/flow/[$flow/bone]/[$mess]/[$frag]
          ::          /[$our]/poke/[$her]/flow/[$flow/bone]/[$mess]/[$frag]
          ::  (e.g.)  /~nec/ack/~zod/flow/0/1/1
          ::          /~zod/poke/~nec/flow/0/1/1
          ::
          =,  -.state
          :: =+  her:encs
          =/  ack-path=path
            %+  fo-en-spac  spac
            :~  reqr=(scot %p her)  %ack  rcvr=(scot %p our)  %flow
                (scot %ud bone)  (scot %ud next)  (scot %ud bone)
            ==
          =/  payload-path=path
            %+  fo-en-spac  spac
            :~  reqr=(scot %p our)  %poke  rcvr=(scot %p her)  %flow
                (scot %ud bone)  (scot %ud next)  (scot %ud bone)
            ==
          =/  =pact
            :^  %poke
              name=[her ack-path packet-size s=num=0]
              name=[our payload-path packet-size s=num=0]
            data=[tot=1 aut=0x0 dat=(fo-en-plea plea)]
          =?  fo-core  ?=(~ unsent-messages)
            %-  fo-emil
            :~  ^-  move  [unix-duct.ax give/(fo-en-gift spac %| pact)]  :: XX
                ^-  move  :^  duct
                  %pass
                /[spac]/[(scot %p her)]/[(scot %ud bone)]/[(scot %ud rift.peer-state)]
                [%b %wait `@da`(add now ~s30)]
            ==
          %_  fo-core
            unsent-messages.state  (~(put to unsent-messages) pact)
          ==
        ::
        ++  fo-hear-sign  !!
          ::  name=[p=ship q=path r=bloq s=num=@udF]
          :: |=  [%page =name q=(each once more) r=next]
          :: ::
          :: ::  check if path.name is one our pokes
          :: ::
          :: =^  next-pact  unsent-messages.state
          ::   (~(get to unsent-messages.state) pact)
          :: =.  fo-core
          ::   %-  emil
          ::   :~  [%magic-send next-pact]
          ::       make-next-packet-timer
          ::   ==
        ::
        :: +|  %internals
        ++  fi  !!  :: |fine core, sends %peeks and assembles responses
        ++  pu
          ::
          :: =>  |%
          ::     $+  packet-state  [~ ~]
          ::     --
          ::
          |_  [=message-blob state=*]
          ::  +|  helpers
          ++  pu-core  .
          ++  pu-abed  !!
          ++  pu-abet  !!
          ::  +|  entry-points
          ::  +|  tasks
          ::  +|  internals
          --
        --


    --
::
|%
++  call
  ::
  |=  [hen=duct dud=(unit goof) wrapped-task=(hobo task)]
  ^-  [(list move) _mesa-gate]
  =/  =task    ((harden task) wrapped-task)
  :: =/     (ev-req [now eny rof] duct axle)
  ::
  =^  moves  ax
    ::  XX  handle error notifications
    ::
    ?^  dud
      !!
    ::
    ?+  -.task  !!
      %born  ev-abet:~(on-born ev-req hen)  ::  XX ev-sys
      :: XX make-poke
    ::
      %plea  ev-abet:(~(on-poke ev-req hen) [ship plea]:task)
    ::
      :: %keen  (~(ev-make-peek ev-req hen) p.task)
    ::
      :: XX ev-res
      ::  %sink-mess
      ::  %sink-pact
      %sink  =<  res-abet
             ?-  -.request.task
               %&  (~(res-pact ev-res hen) (need lane.task) +.request.task)
               %|  (~(res-mess ev-res hen) [lane +.request]:task)
    ==       ==
    ::
  [moves mesa-gate]
::
++  take
  |=  [=wire =duct dud=(unit goof) sign=sign-arvo]
  ^-  [(list move) _mesa-gate]
  [~ mesa-gate]
::
++  stay  [%0 ax]
::
++  load
  |=  old=*
  mesa-gate
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
  ~
--
