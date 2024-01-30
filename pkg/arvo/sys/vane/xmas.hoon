!:
=,  xmas
=>
|%
++  dummy  ^~((reap 100.000 0xdead.beef))
+$  card  (wite note gift)
+$  move  [duct card]
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
      unix=duct
      :: XX tmp=(map @ux page)  :: temporary hash-addressed bindings
  ==
+$  peer-state
  $:  pit=(map path request-state)
  ==
+$  request-state
  $:  for=(set duct)
      pay=(unit path)
      ps=(unit packet-state)
  ==
+$  packet-state
  $:  nex=(each %auth @ud)
      tot=@ud
      :: XX lockstep state
  ==
::
:: XX need auth data on %page and %poke
::    but maybe only when injected externally
::
+$  mess-auth
  [typ=?(%sig %hmac) dat=@ux]
  :: [%page p=spar:ames q=auth r=page]
+$  gage  $@(~ page)
::
::
++  parse-packet  |=(a=@ -:($:de:pact a))
++  is-auth-packet  |
++  inner-path-to-beam
  |=  [=ship =(pole knot)]
  ^-  (unit [vew=view bem=beam])
  ?.  ?=([way=@ des=@ cas=@ res=*] pole)
    ~
  ?~  cas=(de-case cas.pole)
    ~
  `[way.pole [ship des.pole u.cas] res.pole]
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
  ++  ev-born
    `ax(unix hen)
  ++  ev-hear
    |=  [=lane:pact blob=@]
    ^-  [(list move) axle]
    =/  pac  (parse-packet blob)
    ~!  pac
    ~&  pac
    ?-  -.pac
        %page
      ::
      ::  check for pending request (peek|poke)
      ::
      ?~  per=(~(get by p.ax) p.p.pac)
        [~ ax]
      ?~  res=(~(get by pit.u.per) r.p.pac)
        [~ ax]
      ::
      ?:  =(0 t.p.pac)        :: is-first-fragment
        ?^  ps.u.res
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
        [~ ax]
      ?:  is-auth-packet
        ?.  ?=(%auth nex.u.ps.u.res)
          [~ ax]
        ::
        ::  XX LSS: validate merkle proof, initialize hash-tree
        ::  XX request next fragment
        !!
      ::
      ?.  &(=(13 s.p.pac) ?=(%| -.nex.u.ps.u.res) =(p.nex.u.ps.u.res t.p.pac))
        [~ ax]
      ::
      ::  XX LSS: get hash pair from packet, validate and add to tree
      ::  XX LSS: validate fragment
      ::
      ::  XX persist fragment
      ::
      ?:  =(t.p.pac tot.u.ps.u.res)  :: complete
        ::  XX produce as already-validated message
        !!
      ::  XX request next fragment
      ::     by incrementing fragment number in ps.request-state, regenerating next packet
      !!
    ::
        %peek
      ?.  =(our p.p.pac)
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
      [~ ax(p (~(put by p.ax) ship.p.mess (~(del by pit.u.per) path.p.mess)))]
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
    ^-  (quip move _ax)
    =/  per  (~(gut by p.ax) ship.p *peer-state)  :: XX alien-agenda
    :: ?^  res=(~(get by pit.per) path.p)
      :: XX check that payload is the same
      :: [~ ax(p (~(put by p.ax) ship.p (~(put by pit.per) path.p u.res(for (~(put in for.u.res) hen)))))]
    ::
    ::  XX resolve path to validate
    ::
    :: =/  res  *(unit (unit cage)) :: (rof ~ /ames/foo [[our ...] u.q])
    :: ?.  ?=([~ ~ %message *] res)
    ::  !! :: XX wat do?
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
    ?>  =(%czar (clan:title ship.p))
    =/  lanes=(list lane:^pact)
      ~[`@ux`ship.p]
    =/  =gift
      [%sent lanes `(pair @ @)`(fax (en:^pact pact))]
    :_  ax
    [unix.ax %give gift]~
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
  ?^  dud
    %-  (slog leaf/"xmas: {<mote.u.dud>}" tang.u.dud)
    ~^..^$

  =^  mov  ax
    ?-  -.task
      ?(%init %trim %vega)  `ax
      %born       ~(ev-born ev hen)
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
        [%foo any=*]
      ``noun+!>(dummy)

    ::
    ::  message-level entrypoints
    ::
        [%mess ryf=@ pat=*]
      =/  ryf  (slaw %ud ryf.tyl)
      ?~  ryf  [~ ~]
      ?.  =(*rift u.ryf)      :: XX our rift, XX unauthenticated
        ~
      =/  bem  [[our %$ ud+1] pat.tyl]
      =/  res  (rof ~ /ames/mess %xx bem)
      ?.  ?&  ?=([~ ~ %message *] res)
        :: ...validate that it's really a message
        :: =>  [%message tag=?(sig hmac) ser=@]
          ==
        ~
      res
    ::
    ::  XX need a single namespace entrypoint to validate
    ::     generically any authentication tag for a message
    ::
    ::    /ax/[$ship]//1/validate-message/[auth-string]/[blake3-hash]/[path]
    ::
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
    ::
    ::  packet-level entrypoints
    ::
        [%pact ryf=@ boq=@ fag=@ %data pat=*]
      =/  ryf  (slaw %ud ryf.tyl)
      =/  boq  (slaw %ud boq.tyl)
      =/  fag  (slaw %ud fag.tyl)
      ?:  |(?=(~ ryf) ?=(~ boq) ?=(~ fag))
        [~ ~]
      ?.  =(13 u.boq)  ~ :: non-standard fragments for later
      ?.  =(*rift u.ryf)      :: XX our rift
        ~
      =/  bem  [[our %$ ud+1] pat.tyl]
      =/  res  (rof ~ /ames/mess %xx bem)
      ?.  ?&  ?=([~ ~ %message *] res)
        :: ...validate that it's really a message
        :: =>  [tag=?(sig hmac) ser=@]
          ==
        ~
      =+  ;;([@ * ser=@] q.q.u.u.res) :: XX types
      =/  aut  *@ :: sig|hmac
      =/  wid  (met u.boq ser)
      ?<  =(0 wid)  :: XX is this true?
      ?.  (gth wid u.fag)
        [~ ~]
      =/  =pact:pact
        =/  nam
          [our u.ryf pat.tyl u.boq u.fag]
        =/  dat
          ?:  =(1 wid)
            [wid aut ser]
          ::
          ::  XX LSS: all %page (response) packets generated here
          ::
          =/  seq=@
            ?:  =(0 0)  0
            ?:  =(0 u.fag)
              ?:  (lte wid 4)
                ::  XX LSS: inline merkle proof to avoid extra roundtrip
                ::
                ::    1 or two hashes forming merkle-proof w/out leftmost leaf
                ::    (tail proof:(build:lss ...))
                ::
                !!
              ::  XX LSS: root hash only
              ::
              ::    root:(build:lss ...))
              ::
              !!
            ::  XX LSS: perform normal lock-step traversal
            ::
            ::    (snag u.fag pairs:(build:lss ...))
            ::
            !!
          :: XX [aut seq]
          [wid aut (cut u.boq [u.fag 1] ser)]
        ::
        [%page nam dat ~] :: XX dat
      ::  XX produce typed packet or serialized?
      ::
      ``[%packet !>((fax (en:^pact pact)))]
    ::
        [%pact ryf=@ boq=@ fag=@ %auth pat=*]
      =/  ryf  (slaw %ud ryf.tyl)
      =/  boq  (slaw %ud boq.tyl)
      =/  fag  (slaw %ud fag.tyl)
      ?:  |(?=(~ ryf) ?=(~ boq) ?=(~ fag))
        [~ ~]
      ?.  =(13 boq)  ~ :: XX LSS: non-standard fragments for later
      ?.  =(*rift u.ryf)      :: XX our rift
        ~
      =/  bem  [[our %$ ud+1] pat.tyl]
      =/  res  (rof ~ /ames/mess %xx bem)
      ?.  ?&  ?=([~ ~ %message *] res)
        :: ...validate that it's really a message
        :: =>  [tag=?(sig hmac) ser=@]
          ==
        ~
      =*  ser  (,@ q.u.u.res) :: XX types
      =/  aut  *@
      =/  wid  (met u.boq ser)
      ?<  =(0 wid)
      ?.  (gth wid u.fag)
        [~ ~]
      ?.  =(0 fag)  ~  :: non-standard proofs for later
      =/  =pact:pact
        =/  nam
          [our u.ryf pat.tyl u.boq u.fag]
        :: XX LSS: retrieve merkle proof for fragment
        ::
        ::    proof:(build:lss ...)
        ::
        =/  merk  *(list @ux)
        =/  dat
          [wid aut (rep 8 merk)]  :: XX types
        [%page nam dat ~]
      ::  XX produce typed packet or serialized?
      ::
      ``[%packet !>((fax (en:^pact pact)))]
    ==
  ~
--
