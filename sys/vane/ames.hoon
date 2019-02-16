!:  ::  ames (4a), networking
::
  |=  pit=vase
  =>  =~
::  structures
=,  ames
::  this number needs to be below 8
::
=+  protocol-version=1
|%
+=  move  [p=duct q=(wind note:able gift:able)]         ::  local move
::  |pact: internal packet structures
::
++  pact
  |%
  +$  full  [lyf=[to=life from=life] law=(unit deed) txt=@]
  +$  open  [lyf=[to=~ from=life] law=(unit deed) txt=@]
  --
--
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aA, identity logic           ::
  ::              removed in favor of jael/ethereum     ::
  ::
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aB, packet format            ::
  ::
  |%
  ++  bite                                              ::  packet to cake
    |=  pac=rock  ^-  cake
    =+  [mag=(end 5 1 pac) bod=(rsh 5 1 pac)]
    =+  :*  vez=(end 0 3 mag)                           ::  protocol version
            chk=(cut 0 [3 20] mag)                      ::  checksum
            wix=(bex +((cut 0 [23 2] mag)))             ::  width of receiver
            vix=(bex +((cut 0 [25 2] mag)))             ::  width of sender
            tay=(cut 0 [27 5] mag)                      ::  message type
        ==
    ?>  =(protocol-version vez)
    ?>  =(chk (end 0 20 (mug bod)))
    :+  [(end 3 wix bod) (cut 3 [wix vix] bod)]
      (kins tay)
    (rsh 3 (add wix vix) bod)
  ::
  ++  kins  |=(tay=@ (snag tay `(list skin)`[%none %open %fast %full ~]))
  ++  ksin  |=(sin=skin `@`?-(sin %none 0, %open 1, %fast 2, %full 3))
  ++  spit                                              ::  cake to packet
    |=  kec=cake  ^-  @
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
      :~  [3 protocol-version]
          [20 (mug bod)]
          [2 yax]
          [2 qax]
          [5 tay]
      ==
    (lsh 5 1 bod)
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aC, PKI engine               ::
  ::
  |%
  ++  go                                                ::    go
    |_  ton=town                                        ::  ames state
    ++  as                                              ::    as:go
      |_  our=ship                                      ::  per server
      ++  lax                                           ::    lax:as:go
        |_  [her=ship dur=dore]                         ::  per client
        ++  cluy                                        ::    cluy:lax:as:go
          ^-  [lyf=life cub=acru]                       ::  client crypto
          ?>  ?=(^ lew.wod.dur)
          [life.u.lew.wod.dur (com:nu:crub:crypto pass.u.lew.wod.dur)]
        ::
        ++  clon
          ^-  life
          ::  if we don't have a +deed for :her, we guess 1
          ::
          ?~(lew.wod.dur 1 life.u.lew.wod.dur)
        ::
        ++  griz                                        ::    griz:lax:as:go
          |=  now=@da                                   ::  generate key for
          ^-  [p=code q=_+>]
          =+  key=(shas %enty (mix now any.ton))
          :-  key
          %=  +>.$
            any.ton      (shax (mix now any.ton))
            heg.caq.dur  (~(put by heg.caq.dur) (shaf %hand key) key)
          ==
        ::
        ++  pode                                        ::    pode:lax:as:go
          |=  now=@da                                   ::  timeout route
          ^+  +>
          ?:  (lth her 256)  +>(lun.wod.dur [~ %if ~2000.1.1 0 (mix her .0.0.1.0)])
          +>(lun.wod.dur ~)
        ::
        ++  kuch                                        ::    kuch:lax:as:go
          |=  had=hand                                  ::  hear key tag
          ^-  (unit [code _+>])
          =+  wey=(~(get by heg.caq.dur) had)
          ?^  wey
            =+  key=u.wey
            :+  ~  key
            %=    ..kuch
                yed.caq.dur  [~ had u.wey]
                heg.caq.dur  (~(del by heg.caq.dur) had)
                qim.caq.dur  (~(put by qim.caq.dur) had key)
            ==
          =+  dyv=(~(get by qim.caq.dur) had)
          ?~  dyv  ~
          [~ u.dyv ..kuch]
        ::
        ++  wasc                                        ::    wasc:lax:as:go
          |=  key=code                                  ::  hear foreign code
          ^+  +>
          =+  had=(shaf %hand key)
          %_  ..wasc
            yed.caq.dur  [~ had key]
            qim.caq.dur  (~(put by qim.caq.dur) had key)
          ==
        ::
        ++  wast                                        ::    wast:lax:as:go
          |=  ryn=lane                                  ::  set route
          ^+  +>
          %=    +>
              lun.wod.dur
            ?:  ?=([%ix *] ryn)
              ?:  ?|  ?=(~ lun.wod.dur)
                      ?=([%ix *] u.lun.wod.dur)
                      ?&  ?=([%if *] u.lun.wod.dur)
                          (gth p.ryn (add ~s10 p.u.lun.wod.dur))
                      ==
                  ==
                [~ ryn]
              lun.wod.dur
            [~ ryn]
          ==
        ::
        ++  wist                                        ::    wist:lax:as:go
          |=  $:  now=@da                                ::  route via
                  waz=(list @p)
                  ryn=(unit lane)
                  pac=rock
              ==
          ^-  (list boon)
          ?:  =(our her)  [[%ouzo *lane pac] ~]
          ?~  waz  ~
          =+  dyr=?:(=(her i.waz) dur (gur i.waz))
          ?.  ?&  !=(our i.waz)
                  ?=(^ lun.wod.dyr)
              ==
            $(waz t.waz)
          :_  ?:  ?=(%ix -.u.lun.wod.dyr)
                $(waz t.waz)
              ~
          :+  %ouzo  u.lun.wod.dyr
          ?:  &(=(i.waz her) =(~ ryn))  pac
          =+  mal=(jam `meal`[%fore her ryn pac])
          %-  spit
          ^-  cake
          :*  [our i.waz]
              ?~  yed.caq.dyr  [%none mal]
              :-  %fast
              %^  cat  7
                p.u.yed.caq.dyr
              (en:crub:crypto q.u.yed.caq.dyr mal)
          ==
        ::
        ++  zuul                                        ::    zuul:lax:as:go
          |=  [now=@da seg=ship ham=meal]               ::  encode message
          ^-  [p=(list rock) q=_+>]
          =<  weft
          |%
          ++  wasp                                      ::  null security
            ^-([p=skin q=@] [%none (jam ham)])
          ::
          ++  weft                                      ::  fragment message
            ^-  [p=(list rock) q=_+>.$]
            =^  gim  ..weft  wisp
            :_  +>.$
            ^-  (list rock)
            =+  wit=(met 13 q.gim)
            ?<  =(0 wit)
            ?:  =(1 wit)
              =+  yup=(spit [our her] p.gim q.gim)
              [yup ~]
            =+  ruv=(rip 13 q.gim)
            =+  gom=(shaf %thug q.gim)
            =+  inx=0
            |-  ^-  (list rock)
            ?~  ruv  ~
            =+  ^=  vie
                %+  spit
                  [our her]
                wasp(ham [%carp (ksin p.gim) inx wit gom i.ruv])
            :-  vie
            $(ruv t.ruv, inx +(inx))
          ::
          ++  wisp                                      ::  generate message
            ^-  [[p=skin q=@] q=_..wisp]
            ?:  =(%carp -.ham)
              [wasp ..wisp]
            ?:  !=(~ yed.caq.dur)
              ?>  ?=(^ yed.caq.dur)
              :_  ..wisp
              :-  %fast
              %^  cat  7
                p.u.yed.caq.dur
              (en:cub:cluy q.u.yed.caq.dur (jam ham))
            ?:  &(=(~ lew.wod.dur) =(%back -.ham))
              [wasp ..wisp]
            ::  we include our deed in asymmetric skins (%open and %full)
            ::  if we're a comet or moon, or if we're sponsoring her
            ::
            =/  bil=(unit deed)
              =/  rac  (clan:title our)
              ?.  ?|  ?=(?(%earl %pawn) rac)
                      &(!?=(%czar rac) =(our seg))
                  ==
                ~
              `law.ton
            =/  yig  sen
            =/  hom  (jam ham)
            ?:  =(~ lew.wod.dur)
              :_  ..wisp
              :-  %open
              %-  jam
              ^-  open:pact
              :+  [~ lyf.yig]
                bil
              (sign:as:cub.yig hom)
            =/  cay  cluy
            :: :tuy: symmetric key proposal
            ::
            =^  tuy  +>.$  (griz now)
            :_  ..wisp
            :-  %full
            %-  jam
            ^-  full:pact
            :+  [lyf.cay lyf.yig]
              bil
            (seal:as:cub.yig pub:ex:cub.cay (jam tuy hom))
          --                                            ::  --zuul:lax:as:go
        --                                              ::  --lax:as:go
      ::
      ++  gur                                           ::  default dore
        |=  her=ship
        ^-  dore
        =+  def=?.((lth her 256) ~ [~ %if ~2000.1.1 0 (mix her .0.0.1.0)])
        [[~2100.1.1 def ~] *clot]
      ::
      ++  myx                                           ::  dore by ship
        |=  her=ship
        ^+  lax
        =/  fod=dore
          (fall (~(get by hoc.ton) her) (gur her))
        ~(. lax [her fod])
      ::
      ++  nux                                           ::  install dore
        |=  new=_lax
        ^+  +>
        +>(hoc.ton (~(put by hoc.ton) her.new dur.new))
      ::
      ++  sen                                           ::  current crypto
        ^-  [lyf=life cub=acru]
        ?~(val.ton !! [p.i.val.ton r.i.val.ton])
      ::
      ++  sev                                           ::  crypto by life
        |=  mar=life
        ^-  [p=? q=acru]
        ?~  val.ton  !!
        ?:  =(mar p.i.val.ton)
          [& r.i.val.ton]
        ?>  (lth mar p.i.val.ton)
        :-  |
        |-  ^-  acru
        ?>  ?=(^ t.val.ton)
        ?:  =(mar p.i.t.val.ton)
          r.i.t.val.ton
        $(t.val.ton t.t.val.ton)
      --                                                ::  --as:go
    ::
    ++  su                                              ::  install safe
      |=(new=_as `town`ton.new)
    ::
    ++  ti                                              ::  expire by time
      |=  now=@da
      ^-  town
      !!
    ::
    ++  us                                              ::  produce safe
      |=(our=ship `_as`~(. as our))
    --                                                ::  --go
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aF, packet pump             ::
  |%
  ++  pu                                                ::  packet pump
    |_  shed
    ++  abet  +<
    ++  ahoy                                            ::    ahoy:pu
      ^+  .                                             ::  initialize
      %_    .
          rtt  ~s1
          rto  ~s4
          rtn  ~
          rue  ~
          nus  0
          nif  0
          nep  0
          caw  2
          cag  64
          diq  ~
          pyz  ~
          puq  ~
      ==
    ::
    ++  bick                                            ::    bick:pu
      |=  [now=@da fap=flap]                            ::  ack by hash
      ^-  [[p=(unit soup) q=(list rock)] _+>]
      =+  sun=(~(get by diq) fap)
      ?~  sun
        ::  ~&  [%bick-none `@p`(mug fap)]              ::  not a real error
        [[~ ~] +>.$]
      ::  ~&  [%bick-good `@p`(mug fap) u.sun]
      =.  diq  (~(del by diq) fap)
      =^  gub  +>.$  (bock now u.sun)
      =^  yop  +>.$  (harv now)
      [[gub yop] +>.$]
    ::
    ++  bilk                                            ::    bilk:pu
      |=  now=@da                                       ::  inbound packet
      ^+  +>
      =+  trt=(mul 2 rtt)
      %=  +>.$
        rue  [~ now]
        rto  trt
        rtn  ?~(puq ~ [~ (add now trt)])
      ==
    ::
    ++  boom                                            ::    boom:pu
      |=  now=@da  ^-  ?                                ::  address timeout
      |(?=(~ rue) (gte (sub now u.rue) ~m1))
    ::
    ++  bust                                            ::    bust:pu
      ^-  ?                                             ::  not responding
      &(?=(^ rtn) (gte rto ~s16))
    ::
    ++  bike                                            ::    bike:pu
      ^+  .                                             ::  check stats
      ?>  .=  nif
          |-  ^-  @
          ?~  puq  0
          :(add !liv.q.n.puq $(puq l.puq) $(puq r.puq))
      .
    ::
    ++  beet                                            ::    beet:pu
      ^+  .                                             ::  advance unacked
      =-  +(nep ?~(foh nus u.foh))
      ^=  foh
      |-  ^-  (unit @ud)
      ?~  puq  ~
      ?:  (lte p.n.puq nep)  $(puq l.puq)
      =+  rig=$(puq r.puq)
      ?^(rig rig [~ p.n.puq])
    ::
    ++  bine                                            ::    bine:pu
      |=  [now=@da num=@ud]                             ::  apply ack
      ^-  [(unit soup) _+>]
      ?~  puq  !!
      ?.  =(num p.n.puq)
        ?:  (gth num p.n.puq)
          =+  lef=$(puq l.puq)
          [-.lef +.lef(puq [n.puq puq.lef r.puq])]
        =+  rig=$(puq r.puq)
        [-.rig +.rig(puq [n.puq l.puq puq.rig])]
      =:  rtt  ?.  &(liv.q.n.puq =(1 nux.q.n.puq))  rtt
               =+  gap=(sub now lys.q.n.puq)
               ::  ~&  [%bock-trip num (div gap (div ~s1 1.000))]
               (div (add (mul 2 rtt) gap) 3)
          nif  (sub nif !liv.q.n.puq)
        ==
      =+  lez=(dec (need (~(get by pyz) gom.q.n.puq)))
      =^  gub  pyz
          ?:  =(0 lez)
            [[~ gom.q.n.puq] (~(del by pyz) gom.q.n.puq)]
          [~ (~(put by pyz) gom.q.n.puq lez)]
      :-  gub
      +>.$(puq ~(nap to puq))
    ::
    ++  bock                                            ::    bock:pu
      |=  [now=@da num=@ud]                             ::  ack by sequence
      ^-  [(unit soup) _+>]
      =^  gym  +>  (bine now num)
      :-  gym
      ?:  (gth num nep)
        =+  cam=(max 2 (div caw 2))
        ::  ~&  [%bock-hole num nep cam]
        beet:(wept(nep num, cag cam, caw cam) nep num)
      =.  caw  ?:  (lth caw cag)  +(caw)
               (add caw !=(0 (mod (mug now) caw)))
      ?:  =(num nep)
        ::  ~&  [%bock-fine num nif caw cag]
        beet
      ::  ~&  [%bock-fill num nif caw cag]
      +>.$
    ::
    ++  harv                                            ::    harv:pu
      |=  now=@da                                       ::  harvest queue
      ^-  [(list rock) _+>]
      ?:  =(~ puq)  [~ +>(rtn ~)]
      ?.  (gth caw nif)  [~ +>]
      =+  wid=(sub caw nif)
      =|  rub=(list rock)
      =<  abet  =<  apse
      |%
      ++  abet
        ?~  rub  [~ +>.$]
        [(flop rub) +>.$(rtn [~ (add rto now)])]
      ::
      ++  apse
        ^+  .
        ?~  puq  .
        ?:  =(0 wid)  .
        =>  rigt  =<  left
        ?>  ?=(^ puq)
        ?:  =(0 wid)  .
        ?.  =(| liv.q.n.puq)  .
        ::  ~&  [%harv nux.q.n.puq p.n.puq]
        %_    .
          wid          (dec wid)
          rub          [pac.q.n.puq rub]
          nif          +(nif)
          liv.q.n.puq  &
          nux.q.n.puq  +(nux.q.n.puq)
          lys.q.n.puq  now
        ==
      ::
      ++  left
        ?>  ?=(^ puq)
        ^+(. =+(lef=apse(puq l.puq) lef(puq [n.puq puq.lef r.puq])))
      ++  rigt
        ?>  ?=(^ puq)
        ^+(. =+(rig=apse(puq r.puq) rig(puq [n.puq l.puq puq.rig])))
      --
    ::
    ++  wack                                            ::    wack:pu
      |=  now=@da                                       ::  wakeup (timeout)
      ^-  [(list rock) _+>]
      ?.  &(!=(~ rtn) ?>(?=(^ rtn) (gte now u.rtn)))  [~ +>]
      ::  ~&  [%slow (div rto (div ~s1 1.000))]
      =.  +>  (wept 0 nus)
      ?>  =(0 nif)
      =:  caw  2
          rto  ;:  min
                 (mul 2 rto)
                 ~m2
                 (mul ~s16 ?~(rue 1 +((div (sub now u.rue) ~d1))))
               ==
        ==
      (harv now)
    ::
    ++  wept                                            ::    wept:pu
      |=  [fip=@ud lap=@ud]                             ::  fip thru lap-1
      =<  abet  =<  apse
      |%
      ++  abet  +>.$
      ++  apse
        ^+  .
        ?~  puq  .
        ?:  (lth p.n.puq fip)  ?~(l.puq . left)
        ?:  (gte p.n.puq lap)  ?~(r.puq . rigt)
        =>  rigt  =<  left
        ?>  ?=(^ puq)
        ?.(liv.q.n.puq . .(nif (dec nif), liv.q.n.puq |))
      ::
      ++  left
        ?>  ?=(^ puq)
        ^+(. =+(lef=apse(puq l.puq) lef(puq [n.puq puq.lef r.puq])))
      ++  rigt
        ?>  ?=(^ puq)
        ^+(. =+(rig=apse(puq r.puq) rig(puq [n.puq l.puq puq.rig])))
      --
    ::
    ++  whap                                            ::    whap:pu
      |=  [now=@da gom=soup wyv=(list rock)]            ::  send a message
      ^-  [(list rock) _+>]
      =.  pyz  (~(put by pyz) gom (lent wyv))
      =.  +>
        |-  ^+  +>.^$
        ?~  wyv  +>.^$
        %=  $
          wyv  t.wyv
          nus  +(nus)
          diq  (~(put by diq) (shaf %flap i.wyv) nus)
          puq  (~(put to puq) [nus `soul`[gom 0 | ~2000.1.1 i.wyv]])
        ==
      (harv now)
    --
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aG, protocol engine          ::
  ::
  |%
  ++  am                                                ::    am
    ~%  %ames-am  ..is  ~
    |_  [our=ship now=@da fox=fort ski=sley]            ::  protocol engine
    ::  +deed-scry: for a +deed at a +life
    ::
    ++  deed-scry
      ~/  %deed-scry
      |=  [who=ship lyf=life]
      ^-  (unit deed)
      =;  ded
        ?~(ded ~ u.ded)
      ;;  (unit (unit deed))
      %-  (sloy-light ski)
      =/  pur=spur
        /(scot %ud lyf)/(scot %p who)
      [[151 %noun] %j our %deed da+now pur]
    ::  +life-scry: for a +life
    ::
    ++  life-scry
      ~/  %life-scry
      |=  who=ship
      ^-  (unit life)
      =;  lyf
        ?~(lyf ~ u.lyf)
      ;;  (unit (unit life))
      %-  (sloy-light ski)
      =/  pur=spur
        /(scot %p who)
      [[151 %noun] %j our %life da+now pur]
    ::  +sein-scry: for sponsor
    ::
    ++  sein-scry
      ~/  %sein
      |=  who=ship
      ;;  ship
      %-  need  %-  need
      %-  (sloy-light ski)
      [[151 %noun] %j our %sein da+now /(scot %p who)]
    ::  +saxo-scry: for sponsorship chain
    ::
    ++  saxo-scry
      ~/  %saxo
      |=  who=ship
      ;;  (list ship)
      %-  need  %-  need
      %-  (sloy-light ski)
      [[151 %noun] %j our %saxo da+now /(scot %p who)]
    ::
    ++  vein                                            ::    vein:am
      ~/  %vein
      |=  [=life vein=(map life ring)]                  ::  new private keys
      ^-  fort
      ::
      ?.  ?&  (~(has by vein) life)
              =(life (roll ~(tap in ~(key by vein)) max))
          ==
        ~|  [%vein-mismatch +<]  !!
      %=  fox
          hoc.ton
        ::  reset connections
        ::
        (~(run by hoc.ton.fox) |=(=dore dore(caq *clot)))
      ::
          seh.ton
        ::  reset symmetric key cache
        ::
        ~
      ::
      ::  save our secrets, ready for action
          law.ton
        ::  save our deed (for comet/moon communication)
        ::
        (need (deed-scry our life))
      ::
          val.ton
        ::  save our secrets, ready for action
        ::
        ^-  wund
        %+  turn
          %+  sort
            ~(tap by vein)
          |=  [a=[=^life =ring] b=[=^life =ring]]
          (gth life.a life.b)
        |=  [=^life =ring]
        [life ring (nol:nu:crub:crypto ring)]
      ==
    ::
    ++  gnaw                                            ::    gnaw:am
      ~/  %gnaw
      |=  [kay=cape ryn=lane pac=rock]                  ::  process packet
      ^-  [p=(list boon) q=fort]
      ?.  =(protocol-version (end 0 3 pac))  [~ fox]
      =+  kec=(bite pac)
      ?:  (goop p.p.kec)
        [~ fox]
      ?.  =(our q.p.kec)
        [~ fox]
      =;  zap=[p=(list boon) q=fort]
        [(weld p.zap next) q.zap]
      =<  zork
      =<  zank
      ::  ~&  [%hear p.p.kec ryn `@p`(mug (shaf %flap pac))]
      %-  ~(chew la:(ho:um p.p.kec) kay ryn %none (shaf %flap pac))
      [q.kec r.kec]
    ::
    ++  goop                                            ::  blacklist
      |=  him=ship
      |
    ::
    ++  kick                                            ::    kick:am
      |=  hen=duct                                      ::  refresh net
      ^-  [p=(list boon) q=fort]
      zork:(kick:um hen)
    ::
    ++  next
      ^-  (list boon)
      =/  doz=(unit @da)  [~ (add now ~s32)]
      =.  doz
        |-  ^+  doz
        ?~  wab.zac.fox  doz
        =.  doz  $(wab.zac.fox l.wab.zac.fox)
        =.  doz  $(wab.zac.fox r.wab.zac.fox)
        =+  bah=q.n.wab.zac.fox
        (hunt lth doz rtn.sop.bah)
      =/  nex  (hunt lth doz tim.fox)
      ?:  =(tim.fox nex)
        ~
      [%pito (need nex)]~
    ::
    ++  rack                                            ::    rack:am
      ~/  %rack
      |=  [her=ship cha=path cop=coop]                  ::  e2e ack
      =/  oh  (ho:um her)
      =^  gud  oh  (cook:oh cop cha ~)
      ?.  gud  oh
      (cans:oh cha)
    ::
    ++  wake                                            ::    wake:am
      ~/  %wake
      |=  hen=duct                                      ::  harvest packets
      ^-  [p=(list boon) q=fort]
      =.  tim.fox  ~
      =/  neb=(list ship)  ~(tap in ~(key by wab.zac.fox))
      =|  bin=(list boon)
      |-  ^-  [p=(list boon) q=fort]
      ?~  neb
        =^  ban  fox  (kick hen)
        [:(weld bin p.ban next) fox]
      =/  fro=(list ship)  (saxo-scry our)
      =/  hoz  (ho:um i.neb)
      =^  bun  fox  zork:zank:(thaw:hoz fro)
      $(neb t.neb, bin (weld p.bun bin))
    ::
    ++  wise                                            ::    wise:am
      |=  [hen=duct her=ship cha=path val=*]            ::  send a statement
      ^-  [p=(list boon) q=fort]
      =^  ban  fox  zork:zank:(wool:(ho:um her) hen cha val)
      [(weld p.ban next) fox]
    ::
    ++  um                                              ::  per server
      =/  gus   (~(us go ton.fox) our)
      =/  weg=corn  zac.fox
      =|  bin=(list boon)
      |%
      ++  ho                                            ::    ho:um:am
        |=  her=ship                                    ::  per friend
        =+  diz=(myx:gus her)
        =+  bah=(~(get by wab.weg) her)
        =+  puz=?~(bah ahoy:pu %*(. pu +< sop.u.bah))
        =>  .(bah `bath`?~(bah [abet:puz ~ ~] u.bah))
        |%
        ++  busk                                        ::    busk:ho:um:am
          |=  [waz=(list ship) pax=(list rock)]         ::  send packets
          %_    +>
              bin
            |-  ^+  bin
            ?~  pax  bin
            $(pax t.pax, bin (weld (flop (wist:diz now waz ~ i.pax)) bin))
          ==
        ::
        ++  cans                                        ::    cans:ho:um:am
          |=  cha=path
          =+  rum=(need (~(get by raz.bah) cha))
          =.  rum
            %=  rum
              did  +(did.rum)
              mis  (~(del by mis.rum) did.rum)
            ==
          (coat cha rum)
        ::
        ++  coat                                        ::    coat:ho:um:am
          |=  [cha=path rum=race]                       ::  update input race
          ^+  +>
          =+  cun=(~(get by mis.rum) did.rum)
          ?:  |(!dod.rum ?=(~ cun))
            ::
            ::  if we have not yet received the current message,
            ::  or if we are not idle, just wait.
            ::
            +>.$(raz.bah (~(put by raz.bah) cha rum))
          ?.  =(%good p.u.cun)
            ::
            ::  if we are recording a failed message, acknowledge
            ::  it now, since it obviously won't be processed.
            ::
            ~&  [%fail-ack did.rum]
            =^  gud  +>.$
              (cook ``[%dead-message ~] cha `[q.u.cun r.u.cun])
            ?.  gud  +>.$
            %=    +>.$
                raz.bah
              %+  ~(put by raz.bah)  cha
              %=  rum
                did  +(did.rum)
                mis  (~(del by mis.rum) did.rum)
              ==
            ==
          ::
          ::  the message is good.  send it to be processed.
          ::
          ?>  ?=(^ s.u.cun)
          %=    +>.$
              raz.bah  (~(put by raz.bah) cha rum(dod |))
              bin
            :_  bin
            :^    %milk
                her
              `soap`[[lyf:sen:gus clon:diz] cha did.rum]
            u.s.u.cun
          ==
        ::
        ++  cook                                        ::    cook:ho:um:am
          |=  [cop=coop cha=path ram=(unit [ryn=lane dam=flap])]
          ^-  [gud=? con=_+>]                        ::  acknowledgment
          ::  ~&  [%ames-cook cop cha ram]
          =+  rum=(need (~(get by raz.bah) cha))
          =+  lat=(~(get by mis.rum) did.rum)
          ?:  &(?=(~ lat) ?=(~ ram))
            ~&  %ack-late-or-redundant
            [%| +>.$]
          :-  %&
          =+  ^-  [ryn=lane dam=flap]
              ?^  ram  [ryn.u.ram dam.u.ram]
              ?<  ?=(~ lat)
              [q r]:u.lat
          =.  raz.bah
            ?^  ram  raz.bah
            %+  ~(put by raz.bah)  cha
            rum(dod &, bum ?~(cop bum.rum (~(put by bum.rum) did.rum u.cop)))
          =/  seg  (sein-scry her)
          =^  roc  diz  (zuul:diz now seg [%back cop dam ~s0])
          =/  fro=(list ship)  (saxo-scry our)
          (busk(diz (wast:diz ryn)) (xong fro) roc)
        ::  XX move this logic into %zuse, namespaced under %jael?
        ::
        ++  deng                                        ::    deng:ho:um:am
          |=  law=(unit deed)                           ::  accept inline deed
          ^+  diz
          ?:  |(=(~ law) =(lew.wod.dur.diz law))
            diz
          ~|  [%deng-fail her]
          ?>  ?=(^ law)
          =*  wed  u.law
          ?>  ^-  ?
              ?-    (clan:title her)
                  %earl
                ::  signed by parent
                ::
                =/  seg  (^sein:title her)
                =/  yig
                  ?:  =(our seg)
                    sen:gus
                  cluy:(myx:gus seg)
                ?&  =(lyf.yig life.wed)
                    ?=(^ oath.wed)
                    .=  (need (sure:as:cub.yig u.oath.wed))
                    (shaf %earl (sham [her life.wed pass.wed]))
                ==
              ::
                  %pawn
                ::  self-signed, life 1, address is fingerprint
                ::
                =/  cub=acru  (com:nu:crub:crypto pass.wed)
                ?&  =(`@`fig:ex:cub her)
                    ?=(%1 life.wed)
                    ?=(^ oath.wed)
                    ::  XX do we care about this signature at all?
                    ::
                    .=  (need (sure:as:cub u.oath.wed))
                    (shaf %self (sham [her life.wed pass.wed]))
                ==
              ::
                  *
                ::  our sponsor
                ::
                ?&  !?=(%czar (clan:title our))
                    =(her (sein-scry our))
                ==
              ==
          diz(lew.wod.dur law)
        ::
        ++  done                                        ::    done:ho:um:am
          |=  [cha=path num=@ud]                        ::  complete outgoing
          ^-  [(unit duct) _+>]
          ::  ~&  [%ames-done cha num]
          =+  rol=(need (~(get by ryl.bah) cha))
          =+  rix=(~(get by san.rol) num)
          ?~  rix  [~ +>.$]
          :-  rix
          %_    +>.$
              ryl.bah
            (~(put by ryl.bah) cha rol(san (~(del by san.rol) num)))
          ==
        ::
        ++  la                                          ::    la:ho:um:am
          |_  [kay=cape ryn=lane aut=skin dam=flap]     ::  per packet
          ::
          ++  chew                                      ::    chew:la:ho:um:am
            |=  [sin=skin msg=@]                        ::  receive
            ^+  +>
            =<  abed
            |%
            ::  +abed: check that we have the keys to communicate with :her
            ::
            ++  abed
              ^+  +>.$
              ::  if we don't have a deed, subscribe for public key updates
              ::
              ::    XX update state so we only ask once?
              ::
              =?  +>.$  ?=(~ lew.wod.dur.diz)
                (emit %beer her)
              ::  if we don't have a deed, scry for it
              ::  (to avoid dropping the packet, if possible).
              ::
              =?  lew.wod.dur.diz  ?=(~ lew.wod.dur.diz)
                =/  life  (life-scry her)
                ?~(life ~ (deed-scry her u.life))
              ::  if we have a deed, proceed
              ::
              ?^  lew.wod.dur.diz
                apse
              ::  if :her is our initial sponsor, proceed (TOFU)
              ::
              ::    XX TOFU is unnecessary if we include keys
              ::    for the full sponsorship chain in the boot event
              ::
              ?:  =(her (sein-scry our))
                apse
              ::  if :her is a comet, or a moon of a known ship, proceed
              ::
              =/  =rank:title  (clan:title her)
              ?:  ?|  ?=(%pawn rank)
                      ?&  ?=(%earl rank)
                          !=(~ lew.wod.dur:(myx:gus (sein-scry her)))
                  ==  ==
                apse
              ::  otherwise, drop the packet
              ::
              +>.$
            ::  +apse: process the packet, notify if :her status changed
            ::
            ++  apse
              ^+  +>.$
              =/  oub  bust:puz
              =/  neg  =(~ yed.caq.dur.diz)
              =.  +>.$  east
              =/  eng  =(~ yed.caq.dur.diz)
              =/  bou  bust:puz
              =?  +>.$  &(oub !bou)
                (emit [%wine her " is ok"])
              ::  the presence of a symmetric key indicates neighboring
              ::  XX use deed instead?
              ::
              =?  +>.$  &(neg !eng)
                %-  emir  :~
                  [%wine her " is your neighbor"]
                  ?>  ?=(^ lew.wod.dur.diz)
                  [%raki her [life pass]:u.lew.wod.dur.diz]
                ==
              +>.$
            ::
            ++  east
              ^+  +>.$
              ?-    sin
                  %none
                ::  ~&  %chew-none
                =.  puz  (bilk:puz now)
                (chow ((hard meal) (cue msg)))
              ::
                  %fast
                ::  ~&  %chew-fast
                =+  [mag=`hand`(end 7 1 msg) bod=(rsh 7 1 msg)]
                =/  dey  (kuch:diz mag)
                ::  ignore unknown key
                ::
                ?~  dey  +>.$
                =.  puz  (bilk:puz now)
                =^  key  diz  u.dey
                (chow(aut sin) ((hard meal) (cue (dy:cub:sen:gus key bod))))
              ::
                  %full
                ::  ~&  %chew-full
                =/  mex  ((hard full:pact) (cue msg))
                =.  diz  (deng law.mex)
                =/  wug  cluy:diz
                ?>  =(lyf.wug from.lyf.mex)
                =/  gey  (sev:gus to.lyf.mex)
                =/  sem  (need (tear:as:q.gey pub:ex:cub.wug txt.mex))
                =/  mes  ((hard (pair @ @)) (cue sem))
                =.  diz  (wasc:diz p.mes)
                =.  puz  (bilk:puz now)
                (west(msg q.mes))
              ::
                  %open
                ::  ~&  %chew-open
                =/  mex  ((hard open:pact) (cue msg))
                =.  diz  (deng law.mex)
                =/  wug  cluy:diz
                ?>  =(lyf.wug from.lyf.mex)
                =/  mes  (need (sure:as:cub.wug txt.mex))
                =.  puz  (bilk:puz now)
                (west(msg mes))
              ==
            ++  west
              |=  ~
              =+  vib=(cue msg)
              =+  mal=(meal vib)
              ?.  =(mal vib)
                ~&  [%bad-meal her]
                +>.^$
              (chow(aut sin) mal)
            --
          ::
          ++  chow                                      ::    chow:la:ho:um:am
            |=  fud=meal                                ::  interpret meal
            ^+  +>
            =.  diz  ?:(=(%none aut) diz (wast:diz ryn))
            (dine fud)
          ::
          ++  cock                                      ::    cock:la:ho:um:am
            ^+  .                                       ::  send new ack
            ::  ~&  [%back kay dam]
            =*  cop  `coop`?:(=(%good kay) ~ ``[%dead-packet ~])
            =/  seg  (sein-scry her)
            =^  pax  diz  (zuul:diz now seg [%back cop dam ~s0])
            =/  fro=(list ship)  (saxo-scry our)
            ..cock(+> (busk(diz (wast:diz ryn)) (xong fro) pax))
          ::
          ++  deer                                      ::    deer:la:ho:um:am
            |=  [cha=path num=@ud dut=(unit)]           ::  interpret message
            ^+  +>
            =+  rum=(fall (~(get by raz.bah) cha) *race)
            ::  ~&  [%rx kay cha num [dod.rum did.rum] ?=(~ dut)]
            =*  bad  (~(has in bad.fox) her)
            =.  kay  ?.((~(has in bad.fox) her) kay ~&(%blocked %dead))
            %=    +>.$
                +>
              ?:  (lth num did.rum)
                ::
                ::  this message already acknowledged; repeat old ack,
                ::  or negative ack if this ship is blocked
                ::
                =*  cop  ^-  coop
                  %+  fall
                    (~(get by bum.rum) num)
                  ?:(bad ~ ``[%blocked ~])
                con:(cook (~(get by bum.rum) num) cha `[ryn dam])
              ::
              ::  insert this message in unprocessed set
              ::
              =.  mis.rum  (~(put by mis.rum) num [kay ryn dam dut])
              ::
              ::  if ship is blocked, advance pointer to latest message
              ::
              =.  did.rum  ?.(bad did.rum num)
              ::
              ::  process update
              ::
              (coat cha rum)
            ==
          ::
          ++  dine                                      ::    dine:la:ho:um:am
            |=  fud=meal                                ::  interpret meal
            ^+  +>
            ?-    -.fud
                %back
              =.  +>  ?.(=(%full aut) +> cock)          ::  finish handshake
              +>(..la (tock p.fud q.fud r.fud))
            ::
                %bond
              ::  ~&  [%bond p.fud q.fud]
              (deer p.fud q.fud ?-(kay %dead ~, %good [~ r.fud]))
            ::
                %carp
              ::  =+  zol=(~(get by olz.weg) s.fud)
              ::  ?^  zol  cock(kay u.zol)
              =^  neb  nys.weg
                  =+  neb=(~(get by nys.weg) s.fud)
                  ?^  neb  [u.neb nys.weg]
                  =+  neb=`bait`[(kins p.fud) 0 r.fud ~]
                  [neb (~(put by nys.weg) s.fud neb)]
              ?>  (lth q.fud p.r.neb)
              ?>  =((kins p.fud) p.neb)
              ?>  =(r.fud p.r.neb)
              =+  doy=`(unit @)`(~(get by q.r.neb) q.fud)
              ?^  doy  cock
              =>  ^+  .   %=  .
                    q.r.neb  (~(put by q.r.neb) q.fud t.fud)
                    q.neb    +(q.neb)
                  ==
              ::  ~&  [%carp q.fud s.fud q.neb p.r.neb]
              ?:  =(q.neb p.r.neb)
                =:  nys.weg  (~(del by nys.weg) s.fud)
                ::  olz.weg  (~(put by olz.weg) s.fud kay)
                  ==
                (golf p.neb r.neb)
              =.  +>.$  cock
              +>.$(nys.weg (~(put by nys.weg) s.fud neb))
            ::
                %fore
              =+  ^=  lyn  ^-  lane
                  ?~  q.fud  ryn
                  ?.  ?=(%if -.u.q.fud)  u.q.fud
                  [%ix +.u.q.fud]
                  ::  u.q.fud
              ?:  =(our p.fud)
                (emit %mead lyn r.fud)
              =/  zid  (myx:gus p.fud)
              =/  zon  %*(xong ..xong her p.fud)
              =/  fro=(list ship)  (saxo-scry our)
              (emir (wist:zid now (zon fro) [~ lyn] r.fud))
            ==
          ::
          ++  emir                                      ::    emir:la:ho:um:am
            |=  ben=(list boon)                         ::  emit boons
            ^+  +>
            ?~(ben +> $(ben t.ben, bin [i.ben bin]))
          ::
          ++  emit                                      ::    emit:la:ho:um:am
            |=  bun=boon                                ::  emit a boon
            +>(bin [bun bin])
          ::
          ++  golf                                      ::    golf:la:ho:um:am
            |=  [sin=skin duv=dove]                     ::  assemble fragments
            ^+  +>
            %+  chew  sin
            =+  [nix=0 rax=*(list @)]
            |-  ^-  @
            ?:  =(p.duv nix)
              (can 13 (turn (flop rax) |=(a=@ [1 a])))
            $(nix +(nix), rax [(need (~(get by q.duv) nix)) rax])
          --                                            ::  --la:ho:um:am
        ::
        ++  pong                                        ::    pong:ho:um:am
          |=  hen=duct                                  ::  test connection
          ^+  +>
          ?.  ?&  =(~ puq.puz)
                  ?|  bust:puz
                      ?=(~ rue.puz)
                      (gth now (add ~s32 u.rue.puz))
                      (lth u.rue.puz hop.fox)
                  ==
              ==
            +>.$
          (wool [/a/ping hen] /a/pi ~ |)
        ::
        ++  thaw                                        ::    thaw:ho:um:am
          |=  fro=(list ship)
          ^+  +>                                        ::  wakeup bomb
          =+  oub=bust:puz
          =^  yem  puz  (wack:puz now)
          =+  bou=bust:puz
          =.  bin
              ?.  &(bou !oub)  bin
              :_(bin [%wine her " not responding still trying"])
          =.  diz  ?:((boom:puz now) (pode:diz now) diz)
          (busk (xong fro) yem)
        ::
        ++  tock                                        ::    tock:ho:um:am
          |=  [cop=coop fap=flap cot=@dr]               ::  e2e ack by hash
          ^+  +>
          =^  yoh  puz  (bick:puz now fap)
          =.  +>.$
            ?~  p.yoh  +>.$
            =^  hud  +>.$
              (done p.u.p.yoh q.u.p.yoh)
            ?~  hud  +>.$
            %=    +>.$
                bin
              :_  bin
              `boon`[%cake her [[lyf:sen:gus clon:diz] u.p.yoh] cop u.hud]
            ==
          =/  fro=(list ship)  (saxo-scry our)
          (busk (xong fro) q.yoh)
        ::
        ++  wind                                        ::    wind:ho:um:am
          |=  [gom=soup ham=meal]
          ::  ~&  [%wind her gom]
          ^+  +>
          =/  seg  (sein-scry her)
          =^  wyv  diz  (zuul:diz now seg ham)
          =^  feh  puz  (whap:puz now gom wyv)
          =/  fro=(list ship)  (saxo-scry our)
          (busk (xong fro) feh)
        ::
        ++  wool                                        ::    wool:ho:um:am
          |=  [hen=duct cha=path val=*]                 ::  send a statement
          ^+  +>
          =/  rol=rill  (fall (~(get by ryl.bah) cha) *rill)
          =/  sex=@ud  sed.rol
          ::  ~&  [%tx [our her] cha sex]
          ::  if we don't have a public key for :her,
          ::  subscribe to %jael for keys and proceed
          ::
          ::    XX update state so we only ask once?
          ::
          =?  bin  =(~ lew.wod.dur.diz)  :_(bin [%beer her])
          ::  if we don't have a public key for :her,
          ::  scry into %jael for them.
          ::  (skin will only be %open if the scry is ~)
          ::
          =?  lew.wod.dur.diz  =(~ lew.wod.dur.diz)
              =/  life  (life-scry her)
              ?~(life ~ (deed-scry her u.life))
          =.  ryl.bah
              %+  ~(put by ryl.bah)  cha
              %=  rol
                sed  +(sed.rol)
                san  (~(put by san.rol) sex hen)
              ==
          %+  wind  [cha sex]
          [%bond cha sex val]
        ::
        ++  zest                                        ::    zest:ho:um:am
          :~  :~  :*  [%rtt rtt.sop.bah]
                      [%rto rto.sop.bah]
                      [%rtn rtn.sop.bah]
                      [%rue rue.sop.bah]
                  ==
                  :*  [%nus nus.sop.bah]
                      [%nif nif.sop.bah]
                      [%nep nep.sop.bah]
                      [%caw caw.sop.bah]
                      [%cag cag.sop.bah]
                  ==
                  =+  qup=~(tap to puq.sop.bah)
                  :-  %qup
                  %+  turn  qup
                  |=  [a=@ud b=soul]
                  :*  a
                      nux.b
                      liv.b
                      lys.b
                      `@p`(mug (shaf %flap pac.b))
                      gom.b
                  ==
              ==
          ::
              :-  %raz
              =+  zar=~(tap by raz.bah)
              %+  turn  zar
              |=  [a=path b=race]
              :+  a
                did.b
              =+  ciy=~(tap by mis.b)
              %+  turn  ciy
              |=  [c=@ud d=[p=cape q=lane r=flap s=(unit)]]
              [c p.d r.d]
          ::
              [%ryl ~(tap to ryl.bah)]
              [%lun lun.wod.dur.diz]
              [%caq caq.dur.diz]
              [%lew lew.wod.dur.diz]
          ==
        ::
        ++  zank                                        ::    zank:ho:um:am
          %=  +>.$                                      ::  resolve
            gus      (nux:gus diz)
            wab.weg  (~(put by wab.weg) her bah(sop abet:puz))
          ==
        ::
        ++  xong                                        ::    xong:ho:um:am
          |=  fro=(list ship)
          ^-  (list ship)                               ::  route unto
          =/  too  (saxo-scry her)
          =+  ^=  oot  ^-  (list ship)
              =|  oot=(list ship)
              |-  ^+  oot
              ?~  too  ~
              ?:  (lien fro |=(a=ship =(a i.too)))  ~
              [i.too $(too t.too)]
          ::  ~&  [%xong-to [our her] (weld oot ?>(?=(^ fro) t.fro))]
          (weld oot ?>(?=(^ fro) t.fro))
        --                                              ::  --ho:um:am
      ::
      ++  kick                                          ::    kick:um:am
        |=  hen=duct                                    ::  test connection
        ^+  +>
        =/  hoy  (tail (saxo-scry our))
        |-  ^+  +>.^$
        ?~  hoy
          +>.^$
        $(hoy t.hoy, +>.^$ (pong i.hoy hen))
      ::
      ++  pals                                          ::    pals:um:am
        ^-  (list @p)                                   ::  active neighbors
        %+  turn
          %+  skim  ~(tap by wab.weg)
          |=  [a=ship b=bath]
          !(~(boom pu sop.b) now)
        |=([a=ship b=bath] a)
      ::
      ++  pong                                          ::    pong:um:am
        |=  [her=ship hen=duct]                         ::  test neighbor
        ^+  +>
        zank:(pong:(ho her) hen)
      ::
      ++  zork                                          ::    zork:um:am
        ^-  [p=(list boon) q=fort]                      ::  resolve
        :-  (flop bin)
        %_  fox
          ton  (~(su go ton.fox) gus)
          zac  weg
        ==
      --                                                ::  --um:am
    --                                                  ::  --am
  --
  .  ==
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aH, protocol vane            ::
  ::
  =|  $:  fox=fort                                      ::  kernel state
      ==                                                ::
  |=  [our=ship now=@da eny=@uvJ ski=sley]              ::  current invocation
  ^?                                                    ::  opaque core
  =<
    ~%  %ames-protocol  ..is  ~
    |%                                                  ::  vane interface
    ++  call                                            ::  handle request
      ~/  %call
      |=  $:  hen=duct
              type=*
              wrapped-task=(hobo task:able)
          ==
      ^-  [(list move) _..^$]
      =/  task=task:able
        ?.  ?=(%soft -.wrapped-task)
          wrapped-task
        ((hard task:able) p.wrapped-task)
      =.  any.ton.fox  eny
      =^  duy  ..knob  (knob hen task)
      [duy ..^$]
    ::
    ++  load
      |=  old=fort
      ..^$(fox old)
    ::
    ++  scry
      |=  [fur=(unit (set monk)) ren=@tas why=shop syd=desk lot=coin tyl=path]
      ^-  (unit (unit cage))
      ?.  ?=(%& -.why)  [~ ~]
      ?~  tyl  [~ ~]
      =+  hun=(slaw %p i.tyl)
      ?~  hun  [~ ~]
      ?.  =(`@`0 ren)  ~
      ?.  ?=([$$ %da @] lot)
        ~
      ?.  =(now q.p.lot)  ~
      (temp u.hun [syd t.tyl])
    ::
    ++  stay  fox
    ++  take                                            ::  accept response
      ~/  %take
      |=  [tea=wire hen=duct hin=(hypo sign:able)]
      ^-  [(list move) _..^$]
      =.  any.ton.fox  eny
      =^  duy  ..knap
        (knap tea hen q.hin)
      [duy ..^$]
    --
  ~%  %ames-impl  ..is  ~
  |%
  ++  clop
    ~/  %clop
    |=  [now=@da hen=duct bon=boon]
    ^-  [(list move) fort]
    ?-    -.bon
        %beer
      =/  =wire  /pubs/(scot %p p.bon)
      :_  fox  [hen [%pass wire %j %pubs p.bon]]~
    ::
        %bock
      :_  fox  [hen %give %turf tuf.fox]~
    ::
        %brew
      :_  fox  [hen [%pass / %j %turf ~]]~
    ::
        %cake
      ::  ~?  ?=(^ r.bon)  [%cake-woot-bad hen r.bon]
      :_  fox
      :~  [s.bon %give %woot p.bon r.bon]
      ==
    ::
        %mead
      =^  moz  +>.$  (knob hen [%hear p.bon q.bon])
      [moz fox]
    ::
        %milk
      ::  ~&  [%milk p.bon q.bon]
      ?>  ?=([@ @ *] q.q.bon)
      ?>  ?=(?(%a %c %e %g %j) i.q.q.bon)
      =/  =wire  [(scot %p our) (scot %p p.bon) q.q.bon]
      :_  fox  [hen %pass wire i.q.q.bon %west p.bon t.q.q.bon r.bon]~
    ::
        %ouzo
      ::  ~&  [%send now p.bon `@p`(mug (shaf %flap q.bon))]
      ~|  [%ames-bad-duct duct=gad.fox lane=p.bon]
      ?>  ?=(^ gad.fox)
      :_  fox
      [[gad.fox [%give %send p.bon q.bon]] ~]
    ::
        %pito
      :_  fox(tim `p.bon)
      %-  flop
      ^-  (list move)
      :-  [gad.fox %pass /ames %b %wait p.bon]
      ?~  tim.fox  ~
      [gad.fox %pass /ames %b %rest u.tim.fox]~
    ::
        %raki
      =*  her  p.bon
      =/  moz=(list move)
        [hen [%pass / %j %meet her life=q.bon pass=r.bon]]~
      ::  poke :dns with an indirect binding if her is a planet we're spnsoring
      ::
      =?  moz  ?&  ?=(%duke (clan:title her))
                   ?=(%king (clan:title our))
                   =(our (~(sein-scry am [our now fox ski]) her))
               ==
        =/  cmd  [%meet her]
        =/  pok  [%dns %poke `cage`[%dns-command !>(cmd)]]
        :_  moz  [hen [%pass / %g %deal [our our] pok]]
      [moz fox]
    ::
        %sake
      =/  =wire  /our/(scot %p our)
      :_  fox  [hen [%pass wire %j %vein ~]]~
    ::
        %wine
      :_  fox
      =+  fom=~(rend co %$ %p p.bon)
      :~  :-  hen
          :+  %slip  %d
          :+  %flog  %text
          ;:  weld
            "; "
            fom
            q.bon
          ==
      ==
    ==
  ::
  ++  knap
    ~/  %knap
    |=  [tea=wire hen=duct sih=sign:able]
    ^-  [(list move) _+>]
    ?-  +<.sih
        %crud  [[[hen [%slip %d %flog +.sih]] ~] +>]
    ::
        %mack  ?~  +>.sih  $(sih [%g %nice ~])          ::  XX using old code
               $(sih [%g %mean `[%mack +>+.sih]])
    ::
        %turf
      =.  tuf.fox  turf.sih
      [~ +>.$]
    ::
        %pubs
      ?.  ?=([%pubs @ ~] tea)
        ~&  [%strange-pubs tea]
        [~ +>]
      =/  her=ship  (slav %p i.t.tea)
      =/  gus  (~(us go ton.fox) our)
      =/  diz  (myx:gus her)
      ?:  =(0 life.sih)
          ::  this should clear lew.wod.dur.diz because it means
          ::  we no longer trust that their public key came to
          ::  us honestly (becuse of a %jael snapshot restore).
          ::  in practice, that crashes in ++cluy:las:as:go, so
          ::  we ignore for now.
          ~&  [%ames-hear-empty-pub her]
          [~ +>.$]
      =/  ded=deed
        [life.sih (~(got by pubs.sih) life.sih) oath=~]
      =.  lew.wod.dur.diz  `ded
      =.  ton.fox  (~(su go ton.fox) (nux:gus diz))
      [~ +>.$]
    ::
        %unto  [~ +>]
    ::
        %vein
      ?.  ?=([%our @ ~] tea)
        ~&  [%strange-vein tea]
        [~ +>]
      =.  fox  (~(vein am [our now fox ski]) life.sih vein.sih)
      [~ +>.$]
    ::
        %woot  [~ +>]
    ::
        *
      =+  ^=  fuy
        ^-  [p=(list boon) q=fort]
        ?-  +<.sih
        ::
            %wake
          (~(wake am [our now fox ski]) hen)
        ::
            ?(%mean %nice)                              ::  XX obsolete
          ?:  ?=([%ye ~] tea)
            [~ fox]
          ?>  ?=([@ @ @ *] tea)
          =/  her  (slav %p i.t.tea)
          =*  pax  t.t.tea
          =<  zork  =<  zank
          %^  ~(rack am [our now fox ski])  her  pax
          ::  ~&  [%knap-ack ?-(+<.sih %mean `p.+.sih, %nice ~)]
          ?-(+<.sih %mean `p.+.sih, %nice ~)
        ==
      =>  %_(. fox q.fuy)
      =|  out=(list move)
      |-  ^-  [p=(list move) q=_+>.^$]
      ?~  p.fuy
        [(flop out) +>.^$]
      =^  toe  fox  (clop now hen i.p.fuy)
      $(p.fuy t.p.fuy, out (weld (flop toe) out))
    ==
  ::
  ++  knob
    ~/  %knob
    |=  [hen=duct kyz=task:able]
    ^-  [(list move) _+>]
    ?:  ?=(%crud -.kyz)
      [[[hen [%slip %d %flog kyz]] ~] +>]
    ?:  ?=(%west -.kyz)
      ?>  ?=([%pi ~] q.kyz)
      :_  +>.$
      [[hen %give %mack ~] ~]
    ?:  ?=(%wegh -.kyz)
      ~&  %ames-weighing
      [[hen %give %mass wegh]~ +>]
    =+  ^=  fuy
        ^-  [p=(list boon) q=fort]
        ?-    -.kyz
            %barn
          :_  fox(gad hen)
          [%bock ~]~
        ::
            %bonk
          :_  fox
          ?~  tim.fox
            ~&  %ames-bonk-e
            ~
          [%pito u.tim.fox]~
        ::
            %hear
          (~(gnaw am [our now fox ski]) %good p.kyz q.kyz)
        ::
            %halo
          (~(gnaw am [our now fox ski]) %dead p.kyz q.kyz)
        ::
            %hole
          (~(gnaw am [our now fox ski]) %dead p.kyz q.kyz)
        ::
            %init
          :_  fox  [[%sake ~] [%brew ~] ~]
        ::
            %kick
          =^  ban  fox  (~(kick am [our now fox(hop p.kyz) ski]) hen)
          ::  +next:am called here because +wake calls +kick in a loop
          ::
          [(weld p.ban ~(next am [our now fox ski])) fox]
        ::
            %nuke
          :-  ~
          ?:  (~(has in bad.fox) p.kyz)
            ~&  [%unblock p.kyz]
            fox(bad (~(del in bad.fox) p.kyz))
          ~&  [%block p.kyz]
          fox(bad (~(put in bad.fox) p.kyz))
        ::
            %sunk
          =*  who  p.kyz
          =*  lyf  q.kyz
          ?:  =(our who)
            ?:  (lth lyf p:(head val.ton.fox))
              ::  replaying our old sinkage, ignore
              ::  XX review
              ::
              [~ fox]
            ::  XX include some helpful instructions here
            ::
            :_  fox
            [%wine who ", you have sunk"]~
          ::
          =:  hoc.ton.fox  (~(del by hoc.ton.fox) who)
              wab.zac.fox  (~(del by wab.zac.fox) who)
            ==
          [[%wine who " has sunk"]~ fox]
        ::
            %vega
          ::  re-initialize our cryptosuite B cores
          ::
          =/  =wund
            %+  turn
              val.ton.fox
            |=  [=life =ring *]
            [life ring (nol:nu:crub:crypto ring)]
          [~ fox(val.ton wund)]
        ::
            %wake
          (~(wake am [our now fox ski]) hen)
        ::
            %want
          (~(wise am [our now fox ski]) hen p.kyz q.kyz r.kyz)
        ==
    =>  %_(. fox q.fuy)
    =|  out=(list move)
    |-  ^-  [p=(list move) q=_+>.^$]
    ?~  p.fuy
      [(flop out) +>.^$]
    =^  toe  fox  (clop now hen i.p.fuy)
    $(p.fuy t.p.fuy, out (weld (flop toe) out))
  ::
  ++  temp
    ~/  %temp
    |=  [his=ship tyl=path]
    ^-  (unit (unit cage))
    ?:  ?=([?(%show %tell) *] tyl)
      ?^  t.tyl  [~ ~]
      =+  zet=zest:(ho:~(um am [our now fox ski]) his)
      ``[%noun ?:(=(%show i.tyl) !>(>zet<) !>(zet))]
    ?:  ?=([%pals ~] tyl)
      ?.  =(our his)
        ~
      ``[%noun !>(pals:~(um am [our now fox ski]))]
    ?:  ?=([%time ~] tyl)
      ?.  =(our his)
        ~
      ``[%noun !>(tim.fox)]
    ~
  ::
  ++  wegh
    ^-  mass
    :+  %ames  %|
    :~  :+  %town  %|
        =>  ton.fox
        :~  wund+&+val
            deed+&+law
            fast+&+seh
            them+&+hoc
        ==
        :+  %corn  %|
        =>  zac.fox
        :~  incoming+&+nys
            complete+&+olz
            neighbor+&+wab
        ==
        dot+&+fox
    ==
  --
