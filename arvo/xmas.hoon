::                                                      ::  ::
::::  /hoon/ames/arvo                                   ::::::  vane prelude
  !:                                                    ::  ::
|=  pit/vase                                            ::  kernel vase
=>  =~                                                  ::  
::                                                      ::  ::
::::                                                    ::::::  ames structures
  ::                                                    ::  ::
|%                                                      ::
++  bait  {p/skin q/@ud r/dove}                         ::  fmt nrecvd spec
++  bath                                                ::  per friend
          $:  det/pipe                                  ::  secure channel
              zam/scar                                  ::  outbound boles
              fon/(map bole lock)                       ::  inbound locks
              sal/(map bole colt)                       ::  outbound flows
          ==                                            ::
++  bole  bone                                          ::  inbound opaque
++  cake  {p/sock q/skin r/@}                           ::  top level packet
++  chan  path                                          ::  channel
++  clue                                                ::  live packet state
          $:  vig/?                                     ::  true iff virgin
              tel/part                                  ::  block identity
              fap/flap                                  ::  fragment hash
              dat/rock                                  ::  fragment data
          ==                                            ::
++  part  (pair frag tick)                              ::  fragment of packet
++  coal                                                ::  live packet state
          $:  out/@da                                   ::  sent date
              lod/@da                                   ::  lost-by deadline
              clu/clue                                  ::  packet to send
          ==                                            ::
++  stat                                                ::  pump statistics
          $:  $:  cur/@ud                               ::  window q length
                  max/@ud                               ::  max pax out
                  rey/@ud                               ::  retry q length
              ==                                        ::
              $:  rtt/@dr                               ::  roundtrip estimate
                  las/@da                               ::  last sent
                  lad/@da                               ::  last deadline
              ==                                        ::
          ==                                            ::
++  mind                                                ::
          $:  pol/(map ship bath)                       ::  cached cryptostate
          ==                                            ::
++  mini                                                ::  pump data
          $:  saw/stat                                  ::  statistics
              liv/(qeu coal)                            ::  live packets
              lop/(qeu clue)                            ::  lost packets
          ==                                            ::
++  colt                                                ::  outbound state
          $:  seq/tick                                  ::  next tick to fill
              lac/tick                                  ::  acked tick until
              cob/(map tick comb)                       ::  live messages
              myn/mini                                  ::  packet pump
          ==                                            ::
++  comb                                                ::  live message
          $:  cup/(unit coop)                           ::  final ack
              cha/path                                  ::  channel
              num/frag                                  ::  number of fragments
              ack/frag                                  ::  number acked
              cly/(list clue)                           ::  left to send
          ==                                            ::
++  silo                                                ::  network state
          $:  hen/duct                                  ::  
          ==
++  dove  {p/@ud q/(map @ud @)}                         ::  count 13-blocks
++  flap  @uvH                                          ::  network packet id
++  flea  (pair bole tick)                              ::  message id
++  frag  @ud                                           ::  fragment number
++  lock                                                ::  inbound sequencer
          $:  laq/tick                                  ::  acknowledged until
              nys/(map tick bait)                       ::  inbound partials
              laz/(unit (pair tick flap))               ::  awaiting app
              exc/(map tick ares)                       ::  negative acks
          ==                                            ::  
++  meal                                                ::  payload
          $%  {$back p/bone q/flap r/coop s/@dr}        ::  acknowledgment
              {$bond p/flea q/chan r/*}                 ::  message
              {$carp p/moan q/(pair @ud @)}             ::  fragment
              {$fore p/ship q/(unit lane) r/@}          ::  forwarded packet
          ==                                            ::
++  moan                                                ::  message invariant
          $:  {kos/bole liq/tick}                       ::  flow identity
              syn/@                                     ::  skin number
              cnt/@                                     ::  number of packets
          ==                                            ::
++  pipe                                                ::  secure channel
          $:  out/(unit (pair hand code))               ::  outbound key
              inn/(map hand code)                       ::  inbound keys
              cur/(pair life (unit life))               ::  versions: our their
              gyt/gyft                                  ::  our unshared cert
              pub/(map life pass)                       ::  their public keys
              war/(map life ring)                       ::  our private keys
          ==                                            ::
++  skin  ?($none $open $fast $full)                    ::  encoding stem
++  tick  @ud                                           ::  message sequence no
--                                                      ::
::                                                      ::  
::::                                                    ::::  arvo structures
  ::                                                    ::
|%                                                      ::
++  flam  |=(a/flap `@p`(mug a))                        ::  debug flap
++  msec  |=(a/@dr `@ud`(div a (div ~s1 1.000)))        ::  debug @dr
++  move  {p/duct q/(wind note-arvo gift-ames)}         ::  local move
::                                                      ::
::::  loft                                              ::::  main transceiver
  ::                                                    ::
++  loft                                                ::  
  =>  |%                                                ::
      ++  gift                                          ::  result
        $%  {$west p/ship q/chan r/bole s/*}            ::  outbound message
            {$east p/ship q/chan r/bole s/*}            ::  network response
            {$home p/lane q/@}                          ::  route to self
            {$rest p/coop}                              ::  message result
            {$send p/lane q/@}                          ::  transmit packet
            {$west p/ship q/chan r/bole s/*}            ::  outbound message
        ==                                              ::
      ++  kiss
        $%  {$clue p/ship q/pipe}                       ::  security change
            {$done p/ship q/bole r/coop}                ::  completion
            {$mess p/ship q/chan r/*}                   ::  transmission
        ==
      --
  =|  $:  $:  our/@p                                    ::  XX multihome
              see/$-(ship pipe)
          ==
          mind
          fex/(list gift-ames)
      == 
  |%
  ++  apex
    |=  job/kiss
    !!
  --
--
  ::
  ::::  inbound cores
    ::
::                                                      ::
::::  bite                                              ::::  packet format
  ::                                                    ::
|%
++  bite                                                ::  packet to cake
  |=  pac/rock  ^-  cake
  =+  [mag=(end 5 1 pac) bod=(rsh 5 1 pac)]
  =+  :*  vez=(end 0 3 mag)                             ::  protocol version
          chk=(cut 0 [3 20] mag)                        ::  checksum
          wix=(bex +((cut 0 [23 2] mag)))               ::  width of receiver
          vix=(bex +((cut 0 [25 2] mag)))               ::  width of sender
          tay=(cut 0 [27 5] mag)                        ::  message type
      ==
  ?>  =(7 vez)
  ?>  =(chk (end 0 20 (mug bod)))
  :+  [(end 3 wix bod) (cut 3 [wix vix] bod)]
    (kins tay)
  (rsh 3 (add wix vix) bod)
::
++  kins  |=(tay/@ (snag tay `(list skin)`[%none %open %fast %full ~]))
++  ksin  |=(sin/skin `@`?-(sin $none 0, $open 1, $fast 2, $full 3))
++  spit                                                ::  cake to packet
  |=  kec/cake  ^-  @
  =+  wim=(met 3 p.p.kec)
  =+  dum=(met 3 q.p.kec)
  =+  yax=?:((lte wim 2) 0 ?:((lte wim 4) 1 ?:((lte wim 8) 2 3)))
  =+  qax=?:((lte dum 2) 0 ?:((lte dum 4) 1 ?:((lte dum 8) 2 3)))
  =+  wix=(bex +(yax))
  =+  vix=(bex +(qax))
  =+  bod=:(mix p.p.kec (lsh 3 wix q.p.kec) (lsh 3 (add wix vix) r.kec))
  =+  tay=(ksin q.kec)
  %+  mix
    %+  can  0
    :~  [3 7]
        [20 (mug bod)]
        [2 yax]
        [2 qax]
        [5 tay]
    ==
  (lsh 5 1 bod)
--
::                                                      ::
::::  nose                                              ::::  packet decoder
  ::                                                    ::
|%
++  nose  !:
  =>  |%
      ++  gift                                          ::  side effect
        $%  {$link key/code}                            ::  learn symmetric key
            {$meet doy/gyft}                            ::  learn public key(s)
        ==                                              ::
      --
  |=  {our/@p him/@p det/pipe}
  |=  {syn/skin msg/@}
  ^-  (pair (list gift) {aut/? ham/meal})
  |^  ?-    syn
          $none  [~ | (maul msg)]
          $fast
        =+  [mag=`hand`(end 7 1 msg) bod=(rsh 7 1 msg)]
        =+  key=(~(got by inn.det) mag)
        =+  clr=(need (de:crub key bod))
        [~ & (maul clr)] 
      ::
          $full
        =+  mex=((hard {p/{p/life q/life} q/gyft r/@}) (cue msg))
        =+  rig=(~(got by war.det) p.p.mex)
        =+  pas=(whom q.p.mex q.mex)
        =+  [key out]=(need (tear:as:(nol:nu:crub rig) pas r.mex))
        :-  :~  [%link key]
                [%meet q.mex]
            ==
        [& (maul out)]
      ::
          $open
        =+  mex=((hard {p/{$~ q/life} q/gyft r/@}) (cue msg))
        =+  pas=(whom q.p.mex q.mex)
        =+  out=(need (sure:as:(com:nu:crub pas) *code r.mex))
        [[%meet q.mex]~ & (maul r.mex)]
      ==
  ++  maul  |=(@ `meal`((hard meal) (cue +<)))          ::  unpack message
  ++  whom                                              ::  select public key
    |=  {lyf/life gyt/gyft}
    ^-  pass
    ::
    ::  if we have the public key for this life, use it.
    ::  otherwise, use the key the sender sent, without
    ::  without checking its validity.  invalid public-key
    ::  data will crash the packet when we install it.
    ::
    %-  (bond |.(pub.dat:(~(got by (~(got by gyt) lyf)) him)))
    (~(get by pub.det) lyf)
  --
::                                                      ::
::::  hose                                              ::
  ::                                                    ::
++  hose                                                ::  input decoder
  =>  |%                                                ::
      ++  gift                                          ::  action
        $%  {$back kos/bole dam/flap cop/coop}          ::  report ack
            {$fore her/ship lun/(unit lane) pac/rock}   ::  send forward
            {$have kos/bole cha/chan val/*}             ::  report message
            {$link key/code}                            ::  learn symmetric key
            {$meet doy/gyft}                            ::  learn public key
            {$sack kos/bole dam/flap cop/coop}          ::  send ack
        ==                                              ::
      ++  kiss                                          ::  event
        $%  {$done kos/bole cop/coop}                   ::  commit message
            {$hear dam/flap syn/skin msg/@}             ::  raw packet
        ==                                              ::
      --                                                ::
  =|  $:  $:  our/ship                                  ::  XX singlehome
              him/ship                                  ::
              det/pipe                                  ::
          ==                                            ::
          fex/(list gift)                               ::
          fon/(map bole lock)                           ::
      ==
  |%                                                    ::
  ++  abet  [(flop fex) fon]                            ::  resolve
  ++  acme  |=(fic/gift +>(fex [fic fex]))              ::  effect
  ++  acts                                              ::  effects
    |=(fix/(list gift) +>(fex (weld (flop fix) fex)))   ::
  ::                                                    ::
  ++  apex                                              ::  input
    |=  job/kiss
    ^+  +>
    ?-    -.job
        $done
      =+  loc=(~(got by fon) kos.job)
      ?>  ?=(^ laz.loc)
      =<  hy-abet
      (~(hy-done hy [kos.job p.u.laz.loc] [& q.u.laz.loc] loc) cop.job)
    ::
        $hear
      =+  pet=((nose our him det) syn.job msg.job)
      =.  +>.$  (acts p.pet)
      ?-    -.ham.q.pet
          $back
        ~|  %unsecured-back 
        ?>(aut.q.pet (acme %back [p q r]:ham.q.pet))
      ::
          $bond
        =+  loc=((bond |.(*lock)) (~(get by fon) p.p.ham.q.pet))
        =<  hy-abet
        %.  [q r]:ham.q.pet
        ~(hy-bond hy p.ham.q.pet [aut.q.pet dam.job] loc)
      ::
          $carp
        =+  loc=((bond |.(*lock)) (~(get by fon) kos.p.ham.q.pet))
        =<  hy-abet
        %.  [(kins syn.p.ham.q.pet) cnt.p.ham.q.pet q.ham.q.pet]
        ~(hy-carp hy [kos liq]:p.ham.q.pet [aut.q.pet dam.job] loc)
      ::
          $fore  (acme %fore +.ham.q.pet)
      ==
    ==
  ::
  ++  hy                                                ::  message assembler
    =|  $:  $:  kos/bole                                ::  sender 
                liq/tick                                ::  message number
            ==
            $:  aut/?                                   ::  authenticated
                dam/flap                                ::  critical flap
            ==
            lock
        ==
    =*  loq  ->+
    |%                                                  ::
    ++  hy-abet  ..hy(fon (~(put by fon) kos loq))      ::  resolve
    ++  hy-acme  |=(fic/gift +>(+> (acme fic)))         ::  effect
    ++  hy-acts  |=(fix/(list gift) +>(+> (acts fix)))  ::  effects
    ++  hy-bond                                         ::  full message
      |=   {cha/chan val/*}
      ^+  +>
      ?:  (lth liq laq)  
        ::  we already acked this msg; ack it again
        ::  ~&  [%hi-bond-low [kos liq] laq]
        hy-cong
      ?:  (gth liq laq)  
        ::  later than the next msg; ignore
        ~&  [%hy-bond-after [kos liq] laq]
        +>
      ?:  !=(~ laz)
        ::  this msg is already being processed; ignore
        ~&  [%hy-bond-during [kos liq] laq]
        +>
      ::  report completed message
      %.  [%have kos cha val]
      %=  hy-acme
        ::  delete partial message
        nys  (~(del by nys) liq)
        ::  record message in application processing
        laz  `[liq dam]
      ==
    ::                                                  ::
    ++  hy-done                                         ::  message completed
      |=  cop/coop
      ^+  +>
      (hy-cone(laq +(laq), laz ~) cop)
    ::                                                  ::
    ++  hy-carp                                         ::  process fragment
      |=  {syn/skin cnt/@ud far/(pair @ud @)}
      ^+  +>
      ::  ~&  [%carp fap/`@p`(mug fap) syn/syn cnt/cnt far/p.far]
      ?:  (lth liq laq)
        ::  fragment of a message we've already acknowledged - ack it again.
        ::  ~&  [%hy-carp-late liq laq]
        hy-cong
      ?:  (gth liq laq)  
        ::  fragment of a message after the next we expect - drop it.
        ::  ~&  [%hy-carp-early liq laq]
        +>
      ::  neb: current incomplete message
      =+  neb=`bait`(fall (~(get by nys) liq) [syn 0 [cnt ~]])
      ::  all fragments must agree on the message parameters
      ?>  &(=(p.neb syn) (gth p.r.neb p.far) =(p.r.neb cnt))
      =+  doy=(~(get by q.r.neb) p.far)
      ?^  doy  
        ::  we've already heard this fragment
        (hy-conk ~)
      ::  install fragment 
      =:  q.r.neb  (~(put by q.r.neb) p.far q.far)
          q.neb    +(q.neb)
        ==
      ?.  =(q.neb p.r.neb)
        ::  message not yet complete, reinstall incomplete 
        (hy-conk(nys (~(put by nys) liq neb)) ~)
      ::  decode complete message
      =+  pet=((nose our him det) syn (hy-golf r.neb))
      ::  record decoder effects
      =.  +>.$  (hy-acts p.pet)
      =.  aut  |(aut aut.q.pet)
      ?-  -.ham.q.pet
        $back  ~|(%unsecured-back ?>(aut (hy-acme %back kos [q r]:ham.q.pet)))
        $carp  ~|(%meta-carp !!)
        $fore  (hy-acme %fore +.ham.q.pet) 
        $bond  ~|  %bogus-assembly
               ?>  &(aut =([kos liq] p.ham.q.pet))
               (hy-bond [q r]:ham.q.pet)
      ==
    ::
    ++  hy-cong  (hy-conk (~(get by exc) liq))          ::  duplicate ack
    ++  hy-conk                                         ::  ack current
      |=(cop/coop (hy-acme %back kos dam cop))
    ++  hy-cone                                         ::  record ack
      |=  cop/coop
      =>  ?~(cop . .(exc (~(put by exc) liq u.cop)))
      (hy-conk cop)
    ::                                                  ::
    ++  hy-golf
      |=  duv/dove                                ::  assemble fragments
      =+  [nix=0 rax=*(list @)]
      |-  ^-  @
      ?:  =(p.duv nix)
        (can 13 (turn (flop rax) |=(a/@ [1 a])))
      $(nix +(nix), rax [(need (~(get by q.duv) nix)) rax])
    --
  --
--
::                                                      ::
::::  outbound cores                                    ::::
  ::                                                    ::
::
::::  packet pump
  ::
|%
++  pump                                                ::  packet pump
  =>  |%                                                ::
      ++  gift                                          ::  effect
        $%  {$good p/flap q/part r/@dr s/coop}          ::  logical ack
            {$send p/flap q/part r/rock}                ::  release packet
        ==                                              ::
      ++  kiss                                          ::  event
        $%  {$back p/flap q/coop r/@dr}                 ::  raw ack
            {$cull p/tick}                              ::  cancel message
            {$pack p/(list clue)}                       ::  submit packets
            {$wake $~}                                  ::  random wakeup
        ==                                              ::
      --
  |%
  ++  yawn                                              ::
    |=  myn/mini                                        ::
    ^+  zu
    ~(. zu ~ myn)                                       ::
  ::
  ++  zu                                                ::  state machine
    |_  $:  fex/(list gift)                             ::  effects
            mini                                        ::  state
        ==
    ::                                                  ::
    ++  abba                                            ::  a older than b
      |=  {a/part b/part}
      |((lth q.a q.b) &(=(q.a q.b) (lth p.a p.b)))
    ::                                                  ::
    ++  abet                                            ::  resolve
      ^-  {(list gift:pump) mini}
      ::  =.  .  aver
      [(flop fex) +<+]
    ::                                                  ::
    ++  aver                                            ::  verify
      ?>  (lte cur.saw max.saw)
      ?>  !=(0 max.saw)
      ?.  =(cur.saw (lent (~(tap to liv))))
        ~&  [%aver-cur cur.saw (lent (~(tap to liv)))]
        !!
      ?>  =(rey.saw (lent (~(tap to lop))))
      ?>  =+  |=  {a/coal b/coal}
              &((lth out.a out.b) (lth lod.a lod.b))
          |-  ?|  ?=($~ liv)
                  ?&  ?|  ?=($~ r.liv) 
                          ?&  (+< n.r.liv n.liv)
                              $(liv r.liv)
                      ==  ==
                      ?|  ?=($~ l.liv)
                          ?&  (+< n.liv n.l.liv)
                              $(liv l.liv)
                      ==  ==
                  ==
              ==
      ?>  =+  |=  {a/part b/part}
              |((lth q.a q.b) &(=(q.a q.b) (lth p.a p.b)))
          |-  ?|  ?=($~ lop)
                  ?&  ?|  ?=($~ r.lop) 
                          ?&  (+< tel.n.r.lop tel.n.lop)
                              $(lop r.lop)
                      ==  ==
                      ?|  ?=($~ l.lop) 
                          ?&  (+< tel.n.lop tel.n.l.lop)
                              $(lop l.lop)
                      ==  == 
                  ==
              ==
      .
    ::                                                  ::
    ++  back                                            ::  process raw ack
      |=  {now/@da dam/flap cop/coop lag/@dr}
      ^+  +>
      =-  =/  rtt  ?~(ack ~s0 (sub now out.u.ack))
          =.  rtt  ?:((gth rtt lag) (sub rtt lag) rtt)
          (done:(lose(liv lov) ded) ack dam cop rtt)
      |-  ^-  $:  ack/(unit coal) 
                  ded/(list coal) 
                  lov/(qeu coal)
              ==
      ?~  liv  [~ ~ ~]
      =+  ryt=$(liv r.liv)
      ?^  ack.ryt
        ::
        ::  found in front, no need to search back.
        ::
        [ack.ryt ded.ryt [n.liv l.liv lov.ryt]]
      ::
      ::  lose unacked packets sent before an acked virgin.
      ::
      =+  ^-  $:  top/?
                  ack/(unit coal)
                  ded/(list coal)
                  lov/(qeu coal)
              ==
          ?:  =(dam fap.clu.n.liv)
            [| `n.liv ~ l.liv]
          [& $(liv l.liv)]
      ?~  ack  [~ ~ liv]
      =.  ded  ?:(top [n.liv ded] ded)
      =.  ded  ?:(vig.clu.u.ack (~(tap to r.liv) ded) ded)
      =.  lov  ?:(top [n.liv lov ~] lov)
      [ack ded lov]
    ::                                                  ::
    ++  clap                                            ::  ordered enqueue
      ::
      ::  the `lop` queue isn't really a queue in case of 
      ::  resent packets; packets from older messages
      ::  need to be sent first.  unfortunately hoon.hoon 
      ::  lacks a general sorted/balanced heap right now.
      ::  so we implement a balanced queue insert by hand.
      ::
      |=  clu/clue
      %_    +>
          lop
        |-  ^+  lop
        ?~  lop  [clu ~ ~]
        ?:  ?|  (abba tel.clu tel.n.lop)
                ?&  =(tel.clu tel.n.lop)
                    (lth fap.clu fap.n.lop)
            ==  ==
          [n.lop l.lop $(lop r.lop)]
        [n.lop $(lop l.lop) r.lop]
      ==
    ::                                                  ::
    ++  cull                                            ::  clear message
      |=  tiq/tick
      %_    +>
          liv
        |-  ^+  liv
        ?~  liv  ~
        =+  vil=[n.liv $(liv l.liv) $(liv r.liv)]
        ?.  =(tiq q.tel.clu.n.liv)  vil 
        ~(nip to `(qeu coal)`vil)
      ::
          lop
        |-  ^+  lop
        ?~  lop  ~
        =+  pol=[n.lop $(lop l.lop) $(lop r.lop)]
        ?:  =(tiq q.tel.n.lop)  pol
        ~(nip to `(qeu clue)`pol)
      ==
    ::                                                  ::
    ++  done                                            ::  process cooked ack
      |=  {lyd/(unit coal) dam/flap cop/coop rtt/@dr}
      ^+  +>
      ?~  lyd  +>
      %_  +>
        cur.saw  (dec cur.saw)
        fex      [[%good dam tel.clu.u.lyd rtt cop] fex]
      ==
    ::                                                  ::
    ++  fire                                            ::  send a packet
      |=  {now/@da clu/clue}
      ^+  +>
      ?>  (lth cur.saw max.saw)
      =+  out=?:((lte now las.saw) +(las.saw) now)
      =+  lod=(add now (mul 2 rtt.saw))
      =.  lod  ?:((gth lod lad.saw) lod +(lad.saw))
      ::  ~&  [%fire (flam fap.clu) `@da`out `@da`lod]
      %=  +>.$
        fex      [[%send fap.clu tel.clu dat.clu] fex]
        las.saw  out
        lad.saw  lod
        cur.saw  +(cur.saw)
        liv      (~(put to liv) [out lod clu])
      ==
    ::                                                  ::
    ++  flay                                            ::  time out packets
      |=  now/@da
      ^+  +>
      =-  (lose(liv q.ole) p.ole)
      ^=  ole
      =|  ded/(list coal)
      |-  ^+  [p=ded q=liv]
      ?~  liv  [ded ~]
      ?:  (gte now lod.n.liv)
        ::
        ::  everything in front of a dead packet is dead
        ::
        $(liv l.liv, ded (~(tap to r.liv) [n.liv ded]))
      =+  ryt=$(liv r.liv)
      [p.ryt [n.liv l.liv q.ryt]]
    ::                                                  ::
    ++  lose                                            ::  abandon packets
      |=  cud/(list coal)
      ^+  +>
      ?~  cud  +>
      =.  +>  (clap clu.i.cud)
      %=    $
        cud      t.cud
        cur.saw  (dec cur.saw)
        rey.saw  +(rey.saw)
      ==
    ::                                                  ::
    ++  ship                                            ::  send packets
      |=  {now/@da cly/(list clue)}
      ^+  +>
      ?:  (gte cur.saw max.saw)  +>
      ?:  =(0 rey.saw)
        ?~  cly  +>
        $(cly t.cly, +> (fire now i.cly))
      =^  clu  lop  ~(get to lop)
      $(+> (fire(rey.saw (dec rey.saw)) now clu))
    ::                                                  ::
    ++  wait                                            ::  next wakeup
      ^-  (unit @da)  
      =+  tup=`(unit coal)`~(top to liv)
      ?~(tup ~ `lod.u.tup)
    ::                                                  ::
    ++  want                                            ::  window space
      ^-  @ud
      ?:  (gte cur.saw max.saw)  0
      =+  gap=(sub max.saw cur.saw)
      ?:  (gte rey.saw gap)  0
      (sub gap rey.saw)
    ::
    ++  work                                            ::
      |=  {now/@da job/kiss}                            ::  perform 
      ^+  +>
      ?-  -.job
        $back  (back now [p q r]:job)
        $cull  (cull p.job)
        $pack  (ship now p.job)
        $wake  (flay now)
      ==
    --
  --
::                                                      ::
::::  knit                                              ::::  message encoder
  ::                                                    ::
++  knit
  =>  |%
      ++  gift                                          ::  side effect
        $%  {$link key/code}                            ::  set symmetric key
        ==                                              ::
      --
  |=  {our/@p her/@p det/pipe}
  |=  {now/@da eny/@ ham/meal}
  =+  hom=(jam ham)
  ^-  (pair (list gift) (list rock))
  =<  weft
  |%
  ++  wain                                              ::  message identity
    ^-  flea
    ?+  -.ham  [0 0]
      $bond  p.ham
      $carp  [kos liq]:p.ham
    == 
  ::
  ++  wasp  ^-({p/skin q/@} [%none hom])                ::  null security
  ++  weft                                              ::  fragment message
    ^-  (pair (list gift) (list rock))
    =+  gum=wisp
    :-  p.gum
    =+  wit=(met 13 q.q.gum)
    ?:  =(1 wit)
      ::  message fits in one packet, don't fragment
      [(spit [our her] p.q.gum q.q.gum) ~]
    =+  ruv=(rip 13 q.q.gum)
    =+  inx=0
    |-  ^-  (list rock)
    ?~  ruv  ~
    :_  $(ruv t.ruv, inx +(inx))
    %+  spit
      [our her]
    wasp(ham [%carp [wain (ksin p.q.gum) wit] inx i.ruv])
  ::
  ++  wisp                                              ::  generate message
    ^-  (pair (list gift) (pair skin @))
    ?:  =(%carp -.ham)
      [~ wasp]
    ?^  out.det
      :-  ~
      :-  %fast
      %^  cat  7
        p.u.out.det
      (en:crub q.u.out.det hom)
    =+  cry=(nol:nu:crub (~(got by war.det) p.cur.det))
    ?~  q.cur.det
      :-  ~
      :-  %open
      %^    jam
          [~ p.cur.det]
        gyt.det
      (sign:as:cry *code hom)
    =+  key=(shaz :(mix (mug ham) now eny))
    :-  [%link key]~
    :-  %full
    %^    jam
        [u.q.cur.det p.cur.det]
      gyt.det
    (seal:as:cry (~(got by pub.det) u.q.cur.det) key hom)
  --


::                                                      ::
::::  rail                                              ::::  message manager
  ::                                                    ::
++  rail                                                ::  message rail
  =>  |%                                                ::
      ++  gift                                          ::
        $%  {$hear p/chan q/coop}                       ::  message ack
            {$link p/code}                              ::  sent key
            {$send p/flap q/rock}                       ::  release packet
        ==                                              ::
      ++  kiss                                          ::
        $%  {$back p/flap q/coop r/@dr}                 ::  raw ack
            {$tell p/chan q/*}                          ::  send message
            {$wake $~}                                  ::  random wakeup
        ==                                              ::
      --                                                ::
  =|  $:  $:  $:  our/ship
                  her/ship
                  det/pipe
              ==
              $:  now/@da
                  eny/@
              ==
              kos/bole
              mup/_(yawn:pump)
              fex/(list gift)
          ==
          colt
      ==
  =*  cot  ->
  |%                                                    ::
  ++  abet  [(flop fex) `colt`cot]                      ::  resolve 
  ++  view                                              ::  inspect
    |%                                                  ::
    ++  bulk                                            ::  queue count
      ^-  @ud
      |-(?~(cob 0 :(add 1 $(cob l.cob) $(cob r.cob))))
    ::                                                  ::
    ++  wait                                            ::  next wakeup
      ^-  (unit @da)
      wait:mup
    --
  ::                                                    ::
  ++  wish                                              ::  operate list
    |=  {now/@da day/(list kiss)}
    ^+  +>
    ?~(day +> $(day t.day, +> (work now i.day)))
  ::
  ++  work                                              ::
    |=  {now/@da job/kiss}                              ::  compute
    ^+  +>
    =<  +>:wy-abet
    |%                                                  ::
    ++  wy-abet  +:wy-able                              ::  resolve
    ++  wy-able  wy-tire:wy-ably:wy-feed:wy-ably        ::  converge
    ++  wy-ably                                         ::  drain
      ^+  .
      =^  fix  myn  abet:mup
      =.  mup  (yawn:pump myn)
      |-  ^+  +>.$
      ?~  fix  +>.$
      $(fix t.fix, +>.$ (wy-abut i.fix))
    ::                                                  ::
    ++  wy-abut                                         ::  pump effect
      |=  fic/gift:pump
      ^+  +>
      ?-    -.fic
          $good
        ::  ~&  [%ok her `@p`(mug p.fic) r.fic]
        (wy-good q.fic s.fic)
      ::
          $send
        ::  ~&  [%go her `@p`(mug p.fic) q.fic]
        +>(fex [[%send p.fic r.fic] fex])
      ==                                                
    ::                                                  ::
    ++  wy-back                                         ::  hear an ack
      |=  {dam/flap cop/coop lag/@dr}
      ::  ~&  [%wy-back (flam dam) cop lag]
      +>(mup (work:mup now %back dam cop lag))
    ::                                                  ::
    ++  wy-feed                                         ::  feed pump
      ^+  .
      =^  cly  .  (wy-find want.mup)
      ::  ~&  [%wy-feed want.mup (lent cly)]
      +(mup (work:mup now %pack cly))
    ::                                                  ::
    ++  wy-find                                         ::  collect packets
      |=  may/@ud
      ^-  {(list clue) _+>}
      =-  [(flop -<) ->]
      =+  [inx=lac hav=*(list clue)]
      |-  ^-  {(list clue) _+>.^$}
      ?:  |(=(0 may) =(inx seq))  [hav +>.^$]
      =^  hey  +>.^$  (wy-flow inx may hav)
      $(inx +(inx), may p.hey, hav q.hey)
    ::                                                  ::
    ++  wy-flow                                         ::  collect by message
      |=  {tiq/tick may/@ud hav/(list clue)}
      =+  mob=(~(got by cob) tiq)
      |-  ^-  {(pair @ud (list clue)) _+>.^$}
      ?:  |(=(0 may) ?=($~ cly.mob))
        [[may hav] +>.^$(cob (~(put by cob) tiq mob))]
      %=  $
        may      (dec may)
        hav      [i.cly.mob hav]
        cly.mob  t.cly.mob
      ==
    ::                                                  ::
    ++  wy-good                                         ::  message ack
      |=  {paz/part cop/coop}
      ^+  +>
      =+  bum=(~(get by cob) q.paz)
      ?:  |(?=($~ bum) =(~ cly.u.bum))  
        ::  ~&  [%wy-good-ignore paz ?=($~ cop)]
        +>.$
      ?^  cop
        ::
        ::  a failure; save this nack, clear the message
        ::
        ~&  [%wy-good-fail q.paz]
        %_  +>.$
          mup  (work:mup now %cull q.paz)
          cob  (~(put by cob) q.paz u.bum(cly ~, cup `cop))
        ==
      ?>  (lth ack.u.bum num.u.bum)
      =.  ack.u.bum  +(ack.u.bum)
      =.  cup.u.bum  ?.(=(ack.u.bum num.u.bum) ~ [~ ~])
      +>.$(cob (~(put by cob) q.paz u.bum))
    ::                                                  ::
    ++  wy-tire                                         ::  report results
      |-  ^+  +
      =+  zup=(~(get by cob) lac)
      ?~  zup  +.$
      ?~  cup.u.zup  +.$
      ::  ~&  [?:(=(0 (end 0 1 kos)) %ta %ba) her kos lac]
      %=  $
        lac  +(lac)
        cob  (~(del by cob) lac)
        fex  :_(fex [%hear [cha u.cup]:u.zup])
      ==
    ::                                                  ::
    ++  wy-wake                                         ::  timeout
      ^+  .
      .(mup (work:mup now %wake ~))
    ::                                                  ::
    ++  wy-tell                                         ::  send
      |=  {cha/chan val/*}
      ^+  +>
      =+  yex=((knit our her det) now eny [%bond [(mix kos 1) seq] cha val])
      =.  fex  (weld (flop p.yex) fex)
      ::  ~&  [?:(=(0 (end 0 1 kos)) %tx %bx) her kos seq cha (lent pex)]
      %_    +>.$ 
          seq  +(seq)
          cob
        %+  ~(put by cob)
          seq
        ^-  comb
        :*  ~
            cha
            (lent q.yex)
            0
            =+  inx=0
            |-  ?~  q.yex  ~
                :_  $(q.yex +.q.yex, inx +(inx))
                [& [inx seq] (shaf %flap i.q.yex) i.q.yex]
        ==
      ==
    --                                                  
  ::                                                    ::
  ++  zeal                                              ::  default state
    |=  {kos/bole tuc/(unit colt)}
    ^+  +>
    ?^  tuc   
      +>(cot u.tuc, mup (yawn:pump myn.u.tuc))
    %_    +>
        cot
      ^-  colt
      :*  0     ::  seq/tick
          0     ::  lac/tick
          ~     ::  cob/(map tick comb)
          ^-  mini
          :*  ^-  stat
              :*  :*  0               :: cur/@ud
                      2               :: max/@ud
                      0               :: rey/@ud
                  ==
                  :*  ~s5             :: rtt/@dr
                      ~2010.1.1       :: las/@da
                      ~2010.1.1       :: lad/@da
              ==  ==
            ~
            ~
      ==  ==
    ==
  --
--
  .  ==
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aH, protocol vane            ::
  ::
  =|  $:  fox/fort                                      ::  kernel state
      ==                                                ::
  |=  {now/@da eny/@ ski/sley}                          ::  current invocation
  |%                                                  ::  vane interface
  ++  call                                            ::  handle request
    |=  $:  hen/duct
            hic/(hypo (hobo kiss-ames))
        ==
    ^-  {p/(list move) q/_..^$}
    !!
  ::
  ++  doze
    |=  {now/@da hen/duct}
    ^-  (unit @da)
    !!
  ::
  ++  load
    |=  old/fort
    ^+  ..^$
    !!
  ::
  ++  scry
    |=  {fur/(unit (set monk)) ren/@tas who/ship syd/desk lot/coin tyl/path}
    ^-  (unit (unit cage))
    !!
  ::
  ++  stay  
    !!
  ++  take                                            ::  accept response
    |=  {tea/wire hen/duct hin/(hypo sign-arvo)}
    ^-  {p/(list move) q/_..^$}
    !!
  --
