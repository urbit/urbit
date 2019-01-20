::                                                      ::  ::
::::  /hoon/ames/arvo                                   ::::::  vane prelude
  !:                                                    ::  ::
|=  pit/vase                                            ::  kernel vase
=>  =~                                                  ::
=,  xmas
::                                                      ::  ::
::::                                                    ::::::  ames structures
  ::                                                    ::  ::
::
=*  pipe  channel:able:jael                             ::  secure channel
=*  gree  farm:pki:jael                                 ::  pki information
|%                                                      ::
++  bait  {p/skin q/@ud r/dove}                         ::  fmt nrecvd spec
++  bath                                                ::  per friend
          $:  det/pipe                                  ::  secure channel
              lun/(unit lane)                           ::  latest route
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
++  coal                                                ::  live packet state
          $:  out/@da                                   ::  sent date
              lod/@da                                   ::  lost-by deadline
              clu/clue                                  ::  packet to send
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
++  dove  {p/@ud q/(map @ud @)}                         ::  count 13-blocks
++  flap  @uvH                                          ::  network packet id
++  flea  (pair bole tick)                              ::  message id
++  frag  @ud                                           ::  fragment number
++  hand  @uvH                                          ::  128-bit hash
++  lock                                                ::  inbound sequencer
          $:  laq/tick                                  ::  acknowledged until
              nys/(map tick bait)                       ::  inbound partials
              laz/(unit (trel tick flap lane))          ::  awaiting app
              exc/(map tick ares)                       ::  negative acks
          ==                                            ::
++  meal                                                ::  payload
          $%  {$back p/bone q/flap r/coop s/@dr}        ::  acknowledgment
              {$bond p/flea q/chan r/*}                 ::  message
              {$carp p/moan q/(pair @ud @)}             ::  fragment
              {$fore p/ship q/(unit lane) r/@}          ::  forwarded packet
          ==                                            ::
++  mini                                                ::  pump data
          $:  saw/stat                                  ::  statistics
              liv/(qeu coal)                            ::  live packets
              lop/(qeu clue)                            ::  lost packets
          ==                                            ::
++  moan                                                ::  message invariant
          $:  {kos/bole liq/tick}                       ::  flow identity
              syn/@                                     ::  skin number
              cnt/@                                     ::  number of packets
          ==                                            ::
++  mute                                                ::  awaiting channel
          $:  inn/(list (pair lane rock))               ::  inbound packets
              out/(list (trel duct chan *))             ::  outbound messages
          ==                                            ::
++  part  (pair frag tick)                              ::  fragment of packet
++  rock  @uvO                                          ::  packet
++  silo                                                ::  global state
          $:  lyf/life                                  ::  current version
              wyr/(map life ring)                       ::  private keys
              ech/(map ship mute)                       ::  waiting partners
              pol/(map ship bath)                       ::  open partners
          ==                                            ::
++  skin  ?($none $open $fast $full)                    ::  encoding stem
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
++  tick  @ud                                           ::  message sequence no
--                                                      ::
::                                                      ::
::::                                                    ::::  arvo structures
  ::                                                    ::
|%                                                      ::
++  flam  |=(a/flap `@p`(mug a))                        ::  debug flap
++  msec  |=(a/@dr `@ud`(div a (div ~s1 1.000)))        ::  debug @dr
++  move  %+  pair                                      ::  local move
            duct                                        ::
          (wind note:able:xmas gift:able:xmas)          ::
::                                                      ::
::::  loft                                              ::::  main transceiver
  ::                                                    ::
++  loft                                                ::
  =>  |%                                                ::
      ++  gift                                          ::  output
        $%  {$east p/duct q/ship r/chan s/*}            ::  network response
            {$home p/lane q/@}                          ::  resend to self
            {$line p/ship q/@da r/code}                 ::  add outbound key
            {$link p/ship q/@da r/code}                 ::  add inbound key
            {$meet p/gree}                              ::  add public key(s)
            {$rest p/duct q/coop}                       ::  message result
            {$send p/lane q/@}                          ::  transmit packet
            {$veil p/ship}                              ::  cache channel
            {$west p/ship q/bole r/chan s/*}            ::  outbound message
        ==                                              ::
      ++  task                                          ::  input
        $%  {$clue p/ship q/pipe}                       ::  update channel
            {$done p/ship q/bole r/coop}                ::  completion
            {$hear p/lane q/@}                          ::  incoming packet
            {$mess p/ship q/duct r/chan s/*}            ::  forward message
            {$rend p/ship q/bole r/chan s/*}            ::  backward message
            {$wake $~}                                  ::  wakeup
        ==
      --
  =|  $:  $:  now/@da
              eny/@
          ==
          silo
          fex/(list gift)
      ==
  =*  syl  ->-
  |%                                                    ::
  ++  abet  [(flop fex) syl]                            ::  resolve
  ++  apex                                              ::  compute
    |=  job/task
    ^+  +>
    ?-    -.job
        $clue  (dear p.job q.job)
        $done  abet:(done:(etre p.job) q.job r.job)
        $hear
      =+  kec=(bite q.job)
      ?>  =(our q.p.kec)
      =+  buh=(~(get by pol) p.p.kec)
      ?~  buh
        ~&  [%ames-from p.p.kec]
        =+  nut=(fall (~(get by ech) p.p.kec) *mute)
        %_  +>.$
          fex  [[%veil p.p.kec] fex]
          ech  (~(put by ech) p.p.kec nut(inn [+.job inn.nut]))
        ==
      abet:(~(hear et p.p.kec u.buh) p.job (shaf %flap q.job) q.kec r.kec)
    ::
        $mess
      =+  buh=(~(get by pol) p.job)
      ?~  buh
        ~&  [%ames-unto p.job]
        =+  nut=(fall (~(get by ech) p.job) *mute)
        %_  +>.$
          fex  [[%veil p.job] fex]
          ech  (~(put by ech) p.job nut(out [+>.job out.nut]))
        ==
      =/  etc  ~(. et p.job u.buh)
      =^  kos  etc  (blow:etc q.job)
      abet:(mess:etc kos r.job s.job)
    ::
        $rend
      abet:(mess:(etre p.job) q.job r.job s.job)
    ::
        $wake
      |-  ^+  +>.^$
      ?~  pol  +>.^$
      =+  lef=$(pol l.pol)
      =+  ryt=$(pol r.pol, fex fex.lef)
      =+  top=~(to-wake et(fex fex.ryt) n.pol)
      +>.^$(fex fex.top, pol [+<.top pol.lef pol.ryt])
    ==
  ::                                                    ::
  ++  dear                                              ::  neighbor update
    |=  {who/@p det/pipe}
    ^+  +>
    =+  noz=(~(get by ech) who)
    ?~  noz
      ::
      ::  we're not waiting for this ship; we must have it
      ::
      =+  bah=(~(got by pol) who)
      +>.$(pol (~(put by pol) who bah(det det)))
    ::
    ::  new neighbor; run all waiting i/o
    ::
    =.  pol  (~(put by pol) who [det ~ [2 ~ ~] ~ ~])
    =+  [inn out]=[(flop inn.u.noz) (flop out.u.noz)]
    =.  +>.$
      |-  ^+  +>.^$
      ?~  inn  +>.^$
      $(inn t.inn, +>.^$ (apex `task`[%hear i.inn]))
    |-  ^+  +>.^$
    ?~  out  +>.^$
    $(out t.out, +>.^$ (apex `task`[%mess who i.out]))
  ::
  ++  doze                                              ::  sleep until
    |-  ^-  (unit @da)
    ?~  pol  ~
    ;:  (cury hunt lth)
      $(pol l.pol)
      $(pol r.pol)
      ~(to-wait et p.n.pol q.n.pol)
    ==
  ::                                                    ::
  ++  etre                                              ::  old neighbor
    |=  who/@p
    ~(. et who (~(got by pol) who))
  ::                                                    ::
  ++  et                                                ::  per neighbor
    |_  $:  who/ship
            bah/bath
        ==
    ++  abet  +>(pol (~(put by pol) who bah))           ::  resolve
    ++  acme  |=(fic/gift +>(fex [fic fex]))            ::  effect
    ++  blow                                            ::  register duct
      |=  hen/duct
      ^-  {bole _+>}
      =+  kus=(~(get by q.zam.bah) hen)
      ?^  kus  [u.kus +>.$]
      :-  p.zam.bah
      %=  +>.$
        p.zam.bah  (add 2 p.zam.bah)
        q.zam.bah  (~(put by q.zam.bah) hen p.zam.bah)
        r.zam.bah  (~(put by r.zam.bah) p.zam.bah hen)
      ==
    ::
    ++  done
      |=  {kos/bole cop/coop}
      ^+  +>
      (in-task %done +<)
    ::                                                  ::
    ++  have                                            ::  receive message
      |=  {kos/bole cha/chan val/*}
      ^+  +>
      ?:  =(0 (end 0 1 kos))
        =+  hen=(~(got by r.zam.bah) kos)
        ::
        ::  if the bole is even, this is a backward flow,
        ::  like a subscription update; ack automatically.
        ::
        (acme:(in-task %done kos ~) %east hen who cha val)
      ::
      ::  if the bole is odd, it's a forward flow.  we
      ::  need to wait for the target to actively ack it.
      ::
      (acme %west who kos cha val)
    ::
    ++  hear                                            ::
      |=  {lyn/lane dam/flap syn/skin msg/@}            ::  hear packet
      ^+  +>
      (in-task %hear +<)
    ::                                                  ::
    ++  mess                                            ::  send message
      |=  {kos/bole cha/chan val/*}
      ^+  +>
      (to-task kos %mess cha val)
    ::                                                  ::
    ++  sack                                            ::  send acknowledgment
      |=  {kos/bole dam/flap cop/coop}
      =+  ^=  yex
          ((knit who lyf wyr det.bah) now eny [%back (mix kos 1) dam cop ~s0])
      =.  +>.$  (to-gifs p.yex)
      |-  ^+  +>.^$
      ?~  q.yex  +>.^$
      $(q.yex t.q.yex, +>.^$ (send ~ i.q.yex))
    ::                                                  ::
    ++  send                                            ::  send packet
      |=  {urg/(unit lane) pac/rock}
      ^+  +>
      ?:  =(our who)  (acme [%send *lane pac])
      =+  zaw=sax.det.bah
      |-  ^+  +>.^$
      ?~  zaw  +>.^$
      =+  ^=  lun  ^-  (unit lane)
          ?:  (lth i.zaw 256)
            ::
            ::  galaxies are mapped into reserved IP space,
            ::  which the interpreter maps into a DNS request.
            ::
            [~ %if ~2000.1.1 31.337 (mix i.zaw .0.0.1.0)]
          ?:  =(who i.zaw)  lun.bah
          =+  hab=(~(get by pol) i.zaw)
          ?~(hab ~ lun.u.hab)
      ?~  lun
        $(zaw t.zaw)
      =.  pac  ?:  &(=(i.zaw who) =(~ urg))
                 pac
               ::
               ::  forwarded packets are not signed/encrypted,
               ::  because (a) we don't need to; (b) we don't
               ::  want to turn one packet into two.  the wrapped
               ::  packet may exceed 8192 bits, but it's unlikely
               ::  to blow the MTU (IP MTU == 1500).
               ::
               (spit [our i.zaw] %none (jam `meal`[%fore who urg pac]))
      =.  +>.^$  (acme %send u.lun pac)
      ::
      ::  stop if we have an %if (direct) address;
      ::  continue if we only have %ix (forwarded).
      ::
      ?:(?=($if -.u.lun) +>.^$ $(zaw t.zaw))
    ::
    ++  in-gift
      |=  hox/gift:hose
      ^+  +>
      ?-    -.hox
          $fore
        ?:  =(our her.hox)
          (acme %home org.hox pac.hox)
        (send(who her.hox) [~ org.hox] pac.hox)
      ::
          $have  (have +.hox)
          $link  (acme %link who exp.hox key.hox)
          $meet  (acme hox)
          $rack  (to-task kos.hox %back dam.hox cop.hox ~s0)
          $rout  +>(lun.bah `lyn.hox)
          $sack  (sack +.hox)
      ==
    ::
    ++  in-gifs
      |=  hoz/(list gift:hose)
      ?~  hoz  +>
      $(hoz t.hoz, +> (in-gift i.hoz))
    ::
    ++  to-gift
      |=  rax/gift:rail
      ?-  -.rax
        $line  (acme %line who ~2018.1.1 q.rax)
        $mack  (acme %rest (~(got by r.zam.bah) p.rax) q.rax)
        $send  (send ~ q.rax)
      ==
    ::
    ++  to-gifs
      |=  raz/(list gift:rail)
      ?~  raz  +>
      $(raz t.raz, +> (to-gift i.raz))
    ::
    ++  in-task
      |=  kyz/task:hose
      ^+  +>
      =^  hoz  fon.bah  abet:(~(apex hose [who wyr det.bah] ~ fon.bah) kyz)
      (in-gifs hoz)
    ::
    ++  to-task
      |=  {kos/bole kyz/task:rail}
      ^+  +>
      =+  cot=((bond |.(zeal:rail)) (~(get by sal.bah) kos))
      =^  raz  cot  abet:(work:(to-rail kos cot) kyz)
      (to-gifs raz)
    ::
    ++  to-rail
      |=  {kos/bole cot/colt}
      ~(. rail [[who lyf wyr det.bah] [now eny] kos (yawn:pump myn.cot) ~] cot)
    ::
    ++  to-wait
      |-  ^-  (unit @da)
      ?~  sal.bah  ~
      ;:  (cury hunt lth)
        $(sal.bah l.sal.bah)
        $(sal.bah r.sal.bah)
        wait:(to-rail p.n.sal.bah q.n.sal.bah)
      ==
    ::
    ++  to-wake
      |-  ^+  +.$
      ?~  sal.bah  +.$
      =+  lef=$(sal.bah l.sal.bah)
      =+  ryt=$(sal.bah r.sal.bah, fex fex.lef)
      =+  top=(work:(to-rail(fex fex.ryt) p.n.sal.bah q.n.sal.bah) %wake ~)
      +.$(fex fex.ryt, sal.bah [[kos cot]:top sal.bah.lef sal.bah.ryt])
    --
  --
  ::
  ::::  inbound cores
    ::
::                                                      ::
::::  bite                                              ::::  packet format
  ::                                                    ::
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
::
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
::                                                      ::
::::  nose                                              ::::  packet decoder
  ::                                                    ::
++  nose  !:
  =>  |%
      ++  gift                                          ::  side effect
        $%  {$link exp/@da key/code}                    ::  learn symmetric key
            {$meet doy/gree}                            ::  learn public key(s)
        ==                                              ::
      --
  |=  $:  him/@p
          wyr/(map life ring)
          det/pipe
      ==
  |=  {syn/skin msg/@}
  ^-  (pair (list gift) {aut/? ham/meal})
  |^  ?-    syn
          $none  [~ | (maul msg)]
          $fast
        =+  [mag=`hand`(end 7 1 msg) bod=(rsh 7 1 msg)]
        =+  key=q:(~(got by inn.det) mag)
        =+  clr=(need (de:crub:crypto key bod))
        [~ & (maul clr)]
      ::
          $full
        =+  mex=((hard {p/{p/life q/life} q/gree r/@}) (cue msg))
        =+  rig=(~(got by wyr) p.p.mex)
        =+  pas=(whom q.p.mex q.mex)
        =+  mes=(need (tear:as:(nol:nu:crub:crypto rig) pas r.mex))
        =+  [key out]=((hard (pair @ux @ux)) (cue mes))
        :-  :~  [%link ~2018.1.1 key]
                [%meet q.mex]
            ==
        [& (maul out)]
      ::
          $open
        =+  mex=((hard {p/{$~ q/life} q/gree r/@}) (cue msg))
        =+  pas=(whom q.p.mex q.mex)
        =+  out=(need (sure:as:(com:nu:crub:crypto pas) r.mex))
        [[%meet q.mex]~ & (maul r.mex)]
      ==
  ++  maul  |=(@ `meal`((hard meal) (cue +<)))          ::  unpack message
  ++  whom                                              ::  select public key
    |=  {lyf/life gyr/gree}
    ^-  pass
    ::
    ::  if we have the public key for this life, use it.
    ::  otherwise, use the key the sender sent, without
    ::  without checking its validity.  invalid public-key
    ::  data will crash the packet when we install it.
    ::
    %-  (bond |.(pub.dat:(~(got by (~(got by gyr) lyf)) him)))
    (bind (~(get by pub.det) lyf) |=(cert:pki:jael pub.dat))
  --
::                                                      ::
::::  hose                                              ::
  ::                                                    ::
++  hose                                                ::  input decoder
  =>  |%                                                ::
      ++  gift                                          ::  action
        $%  {$fore her/ship org/lane pac/rock}          ::  send forward
            {$have kos/bole cha/chan val/*}             ::  report message
            {$link exp/@da key/code}                    ::  learn symmetric key
            {$meet doy/gree}                            ::  learn public key
            {$rack kos/bole dam/flap cop/coop}          ::  report ack
            {$rout lyn/lane}                            ::  learn route
            {$sack kos/bole dam/flap cop/coop}          ::  send ack
        ==                                              ::
      ++  task                                          ::  event
        $%  {$done kos/bole cop/coop}                   ::  commit message
            {$hear lyn/lane dam/flap syn/skin msg/@}    ::  raw packet
        ==                                              ::
      --                                                ::
  =|  $:  $:  him/ship                                  ::
              wyr/(map life ring)                       ::
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
    |=  job/task
    ^+  +>
    ?-    -.job
        $done
      =+  loc=(~(got by fon) kos.job)
      ?>  ?=(^ laz.loc)
      =<  hy-abet
      (~(hy-done hy [kos.job p.u.laz.loc] [& [q r]:u.laz.loc] loc) cop.job)
    ::
        $hear
      =+  pet=((nose him wyr det) syn.job msg.job)
      =.  +>.$  (acts p.pet)
      ::  if packet is authenticated, use its routing info
      =.  +>.$  ?.(aut.q.pet +>.$ (acme %rout lyn.job))
      ?-    -.ham.q.pet
          $back
        ~|  %unsecured-back
        ?>(aut.q.pet (acme %rack [p q r]:ham.q.pet))
      ::
          $bond
        =+  loc=((bond |.(*lock)) (~(get by fon) p.p.ham.q.pet))
        =<  hy-abet
        %.  [q r]:ham.q.pet
        ~(hy-bond hy p.ham.q.pet [aut.q.pet [dam lyn]:job] loc)
      ::
          $carp
        =+  loc=((bond |.(*lock)) (~(get by fon) kos.p.ham.q.pet))
        =<  hy-abet
        %.  [(kins syn.p.ham.q.pet) cnt.p.ham.q.pet q.ham.q.pet]
        ~(hy-carp hy [kos liq]:p.ham.q.pet [aut.q.pet [dam lyn]:job] loc)
      ::
          $fore
        (acme %fore p.ham.q.pet (born lyn.job q.ham.q.pet) r.ham.q.pet)
      ==
    ==
  ::                                                    ::
  ++  born                                              ::  set forward origin
    |=  {lyn/lane urg/(unit lane)}
    ^-  lane
    ::  a forwarded packet contains its origin address,
    ::  but only after the first hop.  if the address
    ::  field is empty, we fill it in with the address
    ::  we received the packet from.  but we replace
    ::  %if with %ix, to show that the ultimate receiver
    ::  may not be able to send back to the origin
    ::  (due to non-full-cone NAT).
    ?~  urg  lyn
    ?.  ?=($if -.u.urg)
      u.urg
    [%ix +.u.urg]
  ::
  ++  hy                                                ::  message assembler
    =|  $:  $:  kos/bole                                ::  sender
                liq/tick                                ::  message number
            ==
            $:  aut/?                                   ::  authenticated
                dam/flap                                ::  critical flap
                lyn/lane                                ::  origin address
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
        laz  `[liq dam lyn]
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
      =+  pet=((nose him wyr det) syn (hy-golf r.neb))
      ::  record decoder effects
      =.  +>.$  (hy-acts p.pet)
      =.  aut  |(aut aut.q.pet)
      ?-  -.ham.q.pet
        $back  ~|(%unsecured-back ?>(aut (hy-acme %rack kos [q r]:ham.q.pet)))
        $carp  ~|(%meta-carp !!)
        $fore  (hy-acme %fore p.ham.q.pet (born lyn q.ham.q.pet) r.ham.q.pet)
        $bond  ~|  %bogus-assembly
               ?>  &(aut =([kos liq] p.ham.q.pet))
               (hy-bond [q r]:ham.q.pet)
      ==
    ::
    ++  hy-cong  (hy-conk (~(get by exc) liq))          ::  duplicate ack
    ++  hy-conk                                         ::  ack current
      |=(cop/coop (hy-acme %sack kos dam cop))
    ++  hy-cone                                         ::  record ack
      |=  cop/coop
      =>  ?~(cop . .(exc (~(put by exc) liq u.cop)))
      (hy-conk cop)
    ::                                                  ::
    ++  hy-golf                                         ::  assemble fragments
      |=  duv/dove
      =+  [nix=0 rax=*(list @)]
      |-  ^-  @
      ?:  =(p.duv nix)
        (can 13 (turn (flop rax) |=(a/@ [1 a])))
      $(nix +(nix), rax [(need (~(get by q.duv) nix)) rax])
    --
  --
::                                                      ::
::::  outbound cores                                    ::::
  ::                                                    ::
::
::::  packet pump
  ::
++  pump                                                ::  packet pump
  =>  |%                                                ::
      ++  gift                                          ::  effect
        $%  {$good p/flap q/part r/@dr s/coop}          ::  logical ack
            {$send p/flap q/part r/rock}                ::  release packet
        ==                                              ::
      ++  task                                          ::  event
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
      ?.  =(cur.saw (lent ~(tap to liv)))
        ~&  [%aver-cur cur.saw (lent ~(tap to liv))]
        !!
      ?>  =(rey.saw (lent ~(tap to lop)))
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
      =?  ded  vig.clu.u.ack  (weld ~(tap to r.liv) ded)
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
        $(liv l.liv, ded (welp ~(tap to r.liv) [n.liv ded]))
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
      |=  {now/@da job/task}                            ::  perform
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
        $%  {$line exp/@da key/code}                    ::  set symmetric key
        ==                                              ::
      --
  |=  {her/@p lyf/life wyr/(map life ring) det/pipe}
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
      (en:crub:crypto q.q.u.out.det hom)
    =+  cry=(nol:nu:crub:crypto (~(got by wyr) lyf))
    ?~  cur.det
      :-  ~
      :-  %open
      %^    jam
          [~ lyf]
        `gree`!!
      (sign:as:cry hom)
    =+  key=(shaz :(mix (mug ham) now eny))
    :-  [%line ~2018.1.1 key]~
    :-  %full
    %^    jam
        [u.cur.det lyf]
      `gree`!!
    (seal:as:cry pub.dat:(~(got by pub.det) u.cur.det) (jam key hom))
  --
::                                                      ::
::::  rail                                              ::::  message manager
  ::                                                    ::
++  rail                                                ::
  =>  |%                                                ::
      ++  gift                                          ::
        $%  {$line p/@da q/code}                        ::  sent key
            {$mack p/bole q/coop}                       ::  message ack
            {$send p/flap q/rock}                       ::  release packet
        ==                                              ::
      ++  task                                          ::
        $%  {$back p/flap q/coop r/@dr}                 ::  raw ack
            {$mess p/chan q/*}                          ::  send message
            {$wake $~}                                  ::  random wakeup
        ==                                              ::
      --                                                ::
  =|  $:  $:  $:  her/ship
                  lyf/life
                  wyr/(map life ring)
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
  ::
  ++  work                                              ::
    |=  job/task                                        ::  compute
    ^+  +>
    =<  +>:wy-abet:wy-work
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
        ~&  [%ok her `@p`(mug p.fic) r.fic]
        (wy-good q.fic s.fic)
      ::
          $send
        ~&  [%go her `@p`(mug p.fic) q.fic]
        +>(fex [[%send p.fic r.fic] fex])
      ==
    ::                                                  ::
    ++  wy-back                                         ::  hear an ack
      |=  {dam/flap cop/coop lag/@dr}
      ~&  [%wy-back (flam dam) cop lag]
      +>(mup (work:mup now %back dam cop lag))
    ::                                                  ::
    ++  wy-feed                                         ::  feed pump
      ^+  .
      =^  cly  .  (wy-find want.mup)
      ~&  [%wy-feed want.mup (lent cly)]
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
        ~&  [%wy-good-ignore paz ?=($~ cop)]
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
    ++  wy-mess                                         ::  send
      |=  {cha/chan val/*}
      ^+  +>
      =+  yex=((knit her lyf wyr det) now eny [%bond [(mix kos 1) seq] cha val])
      =.  fex  (weld (flop p.yex) fex)
      ~&  [?:(=(0 (end 0 1 kos)) %tx %bx) her kos seq cha (lent fex)]
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
    ::                                                  ::
    ++  wy-tire                                         ::  report results
      |-  ^+  +
      =+  zup=(~(get by cob) lac)
      ?~  zup  +.$
      ?~  cup.u.zup  +.$
      ~&  [?:(=(0 (end 0 1 kos)) %ta %ba) her kos lac]
      %=  $
        lac  +(lac)
        cob  (~(del by cob) lac)
        fex  :_(fex [%mack kos `coop`u.cup.u.zup])
      ==
    ::                                                  ::
    ++  wy-wake                                         ::  timeout
      ^+  .
      .(mup (work:mup now %wake ~))
    ::
    ++  wy-work
      ^+  .
      ?-  -.job
        $back  (wy-back +.job)
        $mess  (wy-mess +.job)
        $wake  wy-wake
      ==
    --
  ::                                                    ::
  ++  zeal                                              ::  default state
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
  --
--
  .  ==
::                                                      ::
::::                                                    ::::  kernel interface
  ::                                                    ::
=|  $:  syl/silo                                        ::  kernel state
    ==                                                  ::
|=  {now/@da eny/@ ski/sley}                            ::  current invocation
=>  |%
    ++  love  ~(. loft [now eny] syl ~)                 ::  create loft
    ++  lung                                            ::  gift to move
      |=  gax/gift:loft
      ^-  move
      ?-    -.gax
          $east  [p.gax %give [%east s.gax]]
          $home  [~ %give gax]
          $link  [~ %pass ~ %j gax]
          $line  [~ %pass ~ %j gax]
          $meet  [~ %pass ~ %j gax]
          $rest  [p.gax %give %rest q.gax]
          $send  [~ %give gax]
          $veil  [~ %pass /det/(scot %p p.gax) %j gax]
          $west
        =+  pax=/msg/(scot %p p.gax)/(scot %ud q.gax)
        =+  cad=[%west p.gax +.r.gax s.gax]
        =+  dat=?+(-.r.gax !! $c [%c cad], $e [%e cad], $g [%g cad])
        [~ %pass pax dat]
      ==
    ::
    ++  work
      |=  job/task:loft
      ^-  {(list move) q/_..^$}
      =^  fex  syl  abet:(apex:love job)
      [(turn fex lung) ..^$]
    --
|%                                                    ::  vane interface
++  neon
  |=  our/ship
  ^-  (vane task:able gift:able sign:able note:able silo silo)
  =|  syl/silo
  |%
  ++  load  |=(silo +>)
  ++  stay  syl
  ++  plow
    =|  $:  now/@da
            eny/@e
            sky/roof
        ==
    |%
    ++  doze  ~
    ++  peek
      |=  $:  lyc/(unit (set ship))
              car/term
              bem/beam
          ==
      ^-  (unit (unit (cask vase)))
      ~
    ::
    ++  spin
      =|  $:  hen/duct
              moz/(list move)
          ==
      |%
      ++  call
        |=  tac/task:able
        ^+  +>
        =*  job  ^-  task:loft
            ?-  -.tac
              $hear  tac
              $mess  [%mess p.tac hen q.tac r.tac]
              $wake  tac
            ==
        =^  fex  syl  abet:(apex:love job)
        +>.$(moz (weld (turn fex lung) moz))
      ::
      ++  take
        |=  {tea/wire hin/sign:able}
        =*  job  ^-  task:loft
          ?+    -.tea  !!
              $msg
            ?>  ?=({@ @ $~} +.tea)
            =+  [who kos]=[(slav %p i.t.tea) (slav %ud i.t.t.tea)]
            ?-  +<.hin
                $rend
              [%rend who kos p.+.hin q.+.hin]
                $mack
              [%done who kos ?~(p.+.hin ~ `coop`[~ `[%fail u.p.+.hin]])]
            ==
          ==
        =^  fex  syl  abet:(apex:love job)
        +>.$(moz (weld (turn fex lung) moz))
      --
    --
  --
++  call                                              ::  handle request
  |=  $:  hen/duct
          hic/(hypo task:able:xmas)
      ==
  ^-  {p/(list move) q/_..^$}
  %-  work
  ^-  task:loft
  ?-  -.q.hic
    $hear  q.hic
    $mess  [%mess p.q.hic hen q.q.hic r.q.hic]
    $wake  q.hic
  ==
::
++  doze
  |=  {now/@da hen/duct}
  ^-  (unit @da)
  doze:love
::
++  load
  |=  old/silo
  ^+  ..^$
  ..^$(syl old)
::
++  scry
  |=  {fur/(unit (set monk)) ren/@tas why/shop syd/desk lot/coin tyl/path}
  ^-  (unit (unit cage))
  ~
::
++  stay  syl
++  take                                            ::  accept response
  |=  {tea/wire hen/duct hin/(hypo sign-arvo)}
  ^-  {p/(list move) q/_..^$}
  %-  work
  ?+    -.tea  !!
      $msg
    ?>  ?=({@ @ $~} +.tea)
    =+  [who kos]=[(slav %p i.t.tea) (slav %ud i.t.t.tea)]
    ?>  ?=(?($rend $mack) +<.q.hin)
    ?-  +<.q.hin
      $rend  [%rend who kos p.+.q.hin q.+.q.hin]
      $mack  [%done who kos ?~(p.+.q.hin ~ `coop`[~ `[%fail u.p.+.q.hin]])]
    ==
  ::
      $det
    ?>  ?=({@ $~} +.tea)
    =+  who=(slav %p i.t.tea)
    ?>  ?=($veil +<.q.hin)
    [%clue who p.+.q.hin]
  ==
--
