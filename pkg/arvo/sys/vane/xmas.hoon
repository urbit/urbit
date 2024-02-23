!:
=,  xmas
=>
|%
++  dummy  ^~((reap 100.000 (bex 512)))
+$  move  (wite note gift)
::
+$  note
  $%  [%b %wait @da]
  ==
+$  sign
  $%  [%b %wake ~]
  ==
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
          ?~  state=(init:verifier:^lss tot.q.pac root proof)
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
          =.  proof  [(leaf-hash:^lss fag dat.q.pac) proof]
          =|  root=@ux :: XX compute from proof + leaf
          ?~  state=(init:verifier:^lss tot.q.pac root proof)
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
      ?~  state=(verify-msg:verifier:^lss los.u.ps.u.res msg pair)
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
      %heer       (~(ev-hear ev hen) p.task q.task)
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
        ``[%atom !>(q:(fax (en:pact pac)))]
      ::
      ?-    typ.wan.pac.nex
          %auth
        =/  nam  [[our rif] [boq %auth fag] pat]
        ::  NB: root excluded as it can be recalculated by the client
        ::
        =/  aut  [%0 mes ~]
        =/  lss-proof  (build:^lss (met 3 ser)^ser) ::  XX cache this
        =/  dat  [wid aut (rep 8 proof.lss-proof)]  :: XX types
        [nam dat]
      ::
          %data
        =/  lss-proof  (build:^lss (met 3 ser)^ser)  :: XX cache this
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

