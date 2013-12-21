!:
::  /=main=/bin/tach/hoon
::
=>  %=    .
        +
      =>  +
      =>  ^/===/lib/pony
      =>  ^/===/lib/chat
      =>  |%
          ++  chat
            %+  cook  |=(a=^chat a)
            ;~  pose
              (cold [%how ~] wut)
              (cold [%who ~] tis)
              (cold [%out ~] zap)
              ;~(pfix pam (stag %all (stag %& mess)))
              ;~(pfix bar (stag %all (stag %| mess)))
              (stag %say ;~(plug ;~(pfix sig fed:ag) ;~(pfix ace mess)))
              (stag %def mess)
            ==
          ::
          ++  expn
            %-  sear
            :_  text
            |=  a=@t
            ^-  (unit ,[p=@t q=tank])
            =+  hun=(rush a wide:vast)
            ?~(a ~ [~ a (sell (slap seed u.hun))])
          ::
          ++  mess
            %+  cook  |=(a=^mess a)
            ;~  pose
              (stag %do ;~(pfix pat text))
              (stag %ex ;~(pfix hax expn))
              (stag %qu text)
            ==
          ++  text  (boss 256 (star ;~(pose (shim 32 126) (shim 128 255))))
          --
      |%
      ++  rend
        |=  [chr=tape nym=tape dum=^mess]
        ^-  tank
        ?-  -.dum
          %do  =+  msg=?:(=(0 p.dum) "remains quietly present" (trip p.dum))
               [%leaf "{chr}{nym} {(trip p.dum)}"]
          %ex  [%rose [" " "" ""] [%leaf "{chr}{nym} {(trip p.dum)}"] q.dum ~]
          %qu  [%leaf "{chr}{nym}: {(trip p.dum)}"]
        ==
      --
    ==
=>  %=    .
        -
      :-  :*  bud=(sein `@p`-<)                         ::  chat server
              oot=_@ud                                  ::  outstanding, server
              tod=*(map ,@p ,@ud)                       ::  outstanding, friend
              giz=*(list gift)                          ::  stuff to send
              sad=`sand`[%& &]                          ::  default state
              wak=_@da                                  ::  next wakeup
              dun=|                                     ::  done
          ==
      [who=`@p`-< how=`path`->]
    ==
|=  [est=time *]
|=  ~
=.  wak  est
|-  ^-  bowl
=<  abet:init
|%
++  abet  `bowl`[(flop giz) ?:(dun ~ [~ hope vent(giz ~)])]
++  hope                                                ::  wait for events
  =<  apex
  |%  ++  apex  ^-  (list slip)
        ;:  weld
          buds
          pals
          regs
        ==
      ::
      ++  buds  ^-  (list slip)
        ?:  =(0 oot)  ~
        [[/re [%ow ~]] ~]
      ::
      ++  pals  ^-  (list slip)
        =|  alx=(list slip)
        |-  ^+  alx
        ?~  tod  alx
        %=  $
          tod  r.tod
          alx  %=  $
                 tod  l.tod
                 alx  :_(alx [[%ra (scot %p p.n.tod) ~] [%ow ~]])
                ==
        ==
      ::
      ++  regs  ^-  (list slip)
        :~  [/oy [%lq %oy]]
            [/wa [%wa wak]]
            [/ya [%lq %ya]]
            ^-  slip
            :-  /up
            :+  %up  %text
            :_  ""
            =+  wyt=?:(?=(& -.sad) !=(0 oot) (~(has by tod) p.sad))
            %+  weld
              ?:(?=(& -.sad) ?:(p.sad "&" "|") (scow %p p.sad))
            ?:(wyt "... " " ")
        ==
  --
::
++  init  (joke:(joke ~ [%who ~]) ~ [%ego est])         ::  initial actions
++  joke                                                ::  send message
  |=  [hur=(unit ,@p) msg=*]
  ^+  +>
  ?~  hur
    +>(oot +(oot), giz :_(giz [%sq bud %yo /re msg]))
  %=    +>
      giz  :_(giz [%sq u.hur %ya [%ra (scot %p u.hur) ~] msg])
      tod  =+  dut=(~(get by tod) u.hur)
           (~(put by tod) u.hur ?~(dut 1 +(u.dut)))
  ==
::
++  nice                                                ::  got response
  |=  [hur=(unit ,@p) kay=cape]
  ^+  +>
  =.  +>
    ?~  hur
      +>(oot (dec oot))
    =+  dyt=(need (~(get by tod) u.hur))
    %_    +>.$
        tod
      ?:  =(1 dyt)
        (~(del by tod) u.hur)
      (~(put by tod) u.hur (dec dyt))
    ==
  ?-  kay
    %good  +>
    %dead  (show %leaf "server {(scow %p ?~(hur bud u.hur))} choked")
  ==
::
++  priv                                                ::  private message
  |=  [her=@p mes=^mess]
  ^+  +>
  (show (rend "" (trip (numb her est)) mes))
::
++  said                                                ::  server message
  |=  [her=@p duz=(list zong)]
  ^+  +>
  ?~  duz  +>
  %=    $
      duz  t.duz
      +>
    %-  show
    ?-  -.i.duz
      %all  (rend ?:(=(%white p.i.duz) "& " "| ") (trip q.q.i.duz) r.i.duz)
      %new  [%leaf "{(trip q.p.i.duz)} is in the building"]
      %out  [%leaf "{(trip q.p.i.duz)} has left the building"]
      %who  [%rose [", " "" ""] (turn p.i.duz |=(a=user [%leaf (trip q.a)]))]
    ==
  ==
::
++  shew  |=(tax=(list tank) +>(giz [[%lo tax] giz]))   ::  print to screen
++  show  |=(tan=tank +>(giz [[%la tan] giz]))          ::  print to screen
++  take                                                ::  alarm event
  |-  ^+  +
  =.  wak  (add ~m1 (max wak est))
  ?.(=(0 oot) + (joke ~ `zing`[%ego est]))
::
++  toke                                                ::  user action
  |=  txt=@t
  ^+  +>
  ?:  =(0 txt)  +>
  =+  rey=(rush txt chat)
  ?~  rey
    (show %leaf "invalid input")
  ?-  -.u.rey
    %all  (joke(sad [%& p.u.rey]) ~ `zing`u.rey)
    %def  %-  joke
          ?:  ?=(& -.sad)
            [~ `zing`[%all p.sad p.u.rey]]
          [[~ p.sad] `^mess`p.u.rey]
    %how  (shew (turn (lore ^:@/===doc%/help/txt) |=(a=@t [%leaf (trip a)])))
    %out  (show(dun &) %leaf "thanks for chatting yo")
    %say  (joke(sad [%| p.u.rey]) [~ p.u.rey] `^mess`q.u.rey)
    %who  (joke ~ `zing`u.rey)
  ==
::
++  vent                                                ::  handle event
  |=  [now=@da pax=path nut=note]
  ^-  bowl
  ::  ~&  [%vent now pax nut]
  =.  est  now
  =<  abet
  ?+  -.pax  +>
    %oy  ?>(?=(%lq -.nut) (said p.nut ((hard (list zong)) r.nut)))
    %re  ?>(?=(%ow -.nut) (nice ~ p.nut))
    %ra  ?>(?=(%ow -.nut) (nice [~ (need (slaw %p i.t.pax))] p.nut))
    %up  ?>(?=(%up -.nut) (toke p.nut))
    %wa  ?>(?=(%wa -.nut) take)
    %ya  ?>(?=(%lq -.nut) (priv p.nut ((hard ^mess) r.nut)))
  ==
--
