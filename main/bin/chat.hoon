!:
::  /=main=/bin/chat/hoon
::
=>  %=    .
        +
      =>  +
      =>  ^/===/lib/pony
      =>  ^/===/lib/chat
      =+  flag=?(%elvis %quiet %time [%haus p=@p])
      =+  flags=*(list flag)
      =>  |%
          ++  chk-flag  |=(f=@tas (lien flags |=(flag =(f +<))))
          ++  chat
            %+  cook  |=(a=^chat a)
            ;~  pose
              (cold [%how ~] wut)
              (cold [%out ~] zap)
              %+  stag  %who  %+  stag  %tcc  (teklist ^room tis cen room)
              (cold [%who %ttt ~] ;~(plug tis tis tis))
              (cold [%who %tts ~] ;~(plug tis tis))
              (cold [%who %tis ~] tis)
              %+  stag  %lus  (teklist ^room lus cen room)
              %+  stag  %hep  (teklist ^room hep cen room)
              %+  stag  %kil  (teklist ,@p hep sig fed:ag)
              %+  stag  %res  (teklist ,@p lus sig fed:ag)
              ;~(pfix pam (stag %all (stag %$ (stag %& mess))))
              ;~(pfix bar (stag %all (stag %$ (stag %| mess))))
              %+  stag  %all
              ;~(plug ;~(pfix cen room) (stag %& ;~(pfix ace mess)))
              (stag %say ;~(plug ;~(pfix sig fed:ag) ;~(pfix ace mess)))
              (stag %def mess)
            ==
          ::
          ++  teklist
            |*  [t=_,* pep=_rule pef=_rule sef=_rule]
            ;~(pfix pep (cook (list t) (plus (ifix [pef (star ace)] sef))))
          ::
          ++  expn
            %-  sear
            :_  text
            |=  a=@t
            ^-  (unit ,[p=@t q=tank])
            =+  hun=(rush a wide:vast)
            ?~  hun  ~
            ?~(a ~ [~ a (sell (slap seed u.hun))])
          ::
          ++  room
            %+  cook  |=(a=(list ,@t) `^room`(crip a))
            (plus ;~(pose low nud hep))
          ::
          ++  mess
            %+  cook  |=(a=^mess a)
            ;~  pose
              (stag %do ;~(pfix pat text))
              (stag %ex ;~(pfix hax expn))
              (stag %do (full (easy '')))
              (stag %qu text)
            ==
          ++  text  (boss 256 (star ;~(pose (shim 32 126) (shim 128 255))))
          --
      |%
      ++  rend
        |=  [sen=@da roo=@tas chr=tape nym=tape dum=^mess] ::  roo=^room
        ^-  tank
        =+  da=(yell sen)
        ?-  -.dum
          %do  =+  msg=?:(=(0 p.dum) "remains quietly present" (trip p.dum))
               [%leaf "%{(trip roo)} {chr}{nym} {msg}"]
          %ex  :~  %rose
                   [" " "" ""]
                   [%leaf "%{(trip roo)} {chr}{nym} {(trip p.dum)}"]
                   q.dum
               ==
          %qu
            :-  %leaf
            %+  welp
              ?.  (chk-flag %time)  ~
              "{(scow %ud h.da)}:{(scow %ud m.da)} "
            "%{(trip roo)} {chr}{nym}: {(trip p.dum)}"
        ==
      ::
      ++  read-wlist
        |=  pax=path
        %-  (unit (list))
        =+  fil=((hard arch) .^(%cy pax))
        ?~  q.fil  ~
        `(cue p:((hard ,[%dtzy %uw p=@]) (ream ((hard ,@) .^(%cx pax)))))
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
              kills=*(list ,@p)
              rooms=*(list ^room)
          ==
      [who=`@p`-< how=`path`->]
    ==
|=  [est=time *]
|=  args=(list flag)
=.  flags  `(list flag)`args
?:  &((chk-flag %elvis) (chk-flag %quiet))
  ~|  %invalid-parameters  !!
=+  sta=est  ::  move up to declaration of state
=.  wak  est
=.  bud    ::  future maintainers:  don't add more cell types with changing this
  ?:  (lien args |=(a=flag ?=(^ a)))
    (roll args |=([p=flag q=@p] ?:(?=(^ p) p.p q)))
  bud
=.  kills  %-  (list ,@p)
  (fall (read-wlist /[(scot %p who)]/conf=/chat/killfile/wlist) ~)
=.  rooms  %-  (list ^room)
  (fall (read-wlist /[(scot %p who)]/conf=/chat/[(scot %p bud)]/wlist) ~[coci])
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
        :~  [/ob [%lq %ob]]
            [/wa [%wa wak]]
            [/ya [%lq %ya]]
            ^-  slip
            :-  /up
            :+  %up  %text
            :_  ""
            =+  wyt=?:(?=(& -.sad) !=(0 oot) (~(has by tod) p.sad))
            %+  weld
              ?.  ?=(& -.sad)
                (scow %p p.sad)
               ?~  rooms
                 "deep space "
               :(weld "%" (trip i.rooms) ?:(p.sad " &" " |"))
            ?:(wyt "... " " ")
        ==
  --
::
++  init  (joke:(joke ~ [%who ~]) ~ [%ego est])         ::  initial actions
++  joke                                                ::  send message
  |=  [hur=(unit ,@p) msg=*]
  ^+  +>
  ?~  hur
    +>(oot +(oot), giz :_(giz [%sq bud %bo /re msg]))
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
  ?:  (dead her)  +>
  (show (rend est '(private)' "" (trip (numb her est)) mes))
::
++  said                                                ::  server message
  |=  [her=@p duz=(list zong)]
  ^+  +>
  ?~  duz  +>
  %=    $
      duz  t.duz
      +>
    %-  shew
    ^-  (list tank)
    ?-    -.i.duz
        %all
      ?:  (dead p.s.i.duz)
        ~
      :_  ~
      %-  rend
      :*  p.i.duz
          q.i.duz
          ?:(=(%white r.i.duz) "& " "| ")
          (trip q.s.i.duz)
          t.i.duz
      ==
        %who
      %+  ~(rep by q.i.duz)  *(list tank)
      |=  [p=[r=^room u=(list user)] q=(list tank)]
      :*  [%leaf "%{(trip r.p)}:"]
          :+  %rose  [", " " " ""]
          %+  turn
            %+  weld
              (skim u.p |=(a=user =(p.a who)))
              (skip u.p |=(a=user =(p.a who)))
          |=(a=user [%leaf (trip q.a)])
          q
      ==
        ?(%new %out)
      ?:  ?|  (dead p.r.i.duz)
              (chk-flag %quiet)
              &(!(chk-flag %elvis) (gth sta p.i.duz))
          ==
        ~
      ?-  -.i.duz
        %new  ~[[%leaf "{(trip q.r.i.duz)} saunters into %{(trip q.i.duz)}"]]
        %out  ~[[%leaf "{(trip q.r.i.duz)} ducks out of %{(trip q.i.duz)}"]]
  ==  ==
    ==
::
++  dead
  |=  her=@p
  (lien kills |=(@p =(her +<)))
::
++  kill
  |=  her=(list ,@p)
  %=    +>
      kills  (weld her (skip kills |=(@p =(her +<))))
      giz
    =+  j=(jam (weld her (skip kills |=(@p =(her +<)))))
    =+  encoded=(cat 3 (scot %uw j) `@t`10)             ::  Base-64 encoding
    [[%ok (foal /[(scot %p who)]/conf=/chat/killfile/wlist encoded)] giz]
  ==
::
++  resurrect
  |=  her=(list ,@p)
  %=    +>
      kills  (skip kills |=(a=@p (lien her |=(a=@p =(a ^a)))))
      giz
    =+  j=(jam (skip kills |=(a=@p (lien her |=(a=@p =(a ^a))))))
    =+  encoded=(cat 3 (scot %uw j) `@t`10)             ::  Base-64 encoding
    [[%ok (foal /[(scot %p who)]/conf=/chat/killfile/wlist encoded)] giz]
  ==
::
++  add-room
  |=  roo=(list ^room)
  =+  rs=(weld roo (skip rooms |=(a=^room (lien roo |=(a=^room =(a ^a))))))
  %+  %=  joke
          sad     [%& %&]
          rooms   rs
          giz
        =+  encoded=(cat 3 (scot %uw (jam rs)) `@t`10)  ::  Base-64 encoding
        :_  giz
        [%ok (foal /[(scot %p who)]/conf=/chat/[(scot %p bud)]/wlist encoded)]
      ==
  ~
  `zing`[%lus roo]
::
++  remove-room
  |=  roo=(list ^room)
  =+  rs=(skip rooms |=(a=^room (lien roo |=(a=^room =(a ^a)))))
  %+  %=  joke
          sad     [%& %&]
          rooms   rs
          giz
        =+  j=(jam rs)
        =+  encoded=(cat 3 (scot %uw j) `@t`10)         ::  Base-64 encoding
        :_  giz
        [%ok (foal /[(scot %p who)]/conf=/chat/[(scot %p bud)]/wlist encoded)]
      ==
  ~
  `zing`[%hep roo]
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
    %all  ?~  p.u.rey
            ?~  rooms
              (show [%leaf "in space, no one can hear you scream..."])
            (joke(sad [%& q.u.rey]) ~ `zing`[%all i.rooms q.u.rey r.u.rey])
          (joke(sad [%& q.u.rey]) ~ `zing`u.rey)
    %def  ?~  rooms
            (show [%leaf "in space, no one can hear you scream..."])
          %-  joke
          ?:  ?=(& -.sad)
            [~ `zing`[%all i.rooms p.sad p.u.rey]]
          [[~ p.sad] `^mess`p.u.rey]
    %how  (shew (turn (lore ^:@/===doc%/help/txt) |=(a=@t [%leaf (trip a)])))
    %out  (show(dun &) %leaf "see you space cowboy...")
    %say  (joke(sad [%| p.u.rey]) [~ p.u.rey] `^mess`q.u.rey)
    %who  ?-  p.u.rey
            %tis  ?~  rooms
                    %+  show  %leaf
                    "you are alone.  try again on a habitable world."
                  %+  joke  ~  ^-  zing  :-  %who  `~[i.rooms]
            %tts  %+  joke  ~  ^-  zing  :-  %who  `rooms
            %ttt  %+  joke  ~  ^-  zing  :-  %who  ~
            %tcc  %+  joke  ~  ^-  zing  :-  %who  `q.u.rey
          ==

    %lus  (add-room p.u.rey)
    %hep  ?~  rooms
            %+  show  %leaf
            "you are at the outer rim, alone.  try entering a habitable world."
          (remove-room p.u.rey)
    %kil  (kill p.u.rey)
    %res  (resurrect p.u.rey)
  ==
::
++  vent                                                ::  handle event
  |=  [now=@da pax=path nut=note]
  ^-  bowl
  =.  est  now
  =<  abet
  ?+  -.pax  +>
    %ob  ?>(?=(%lq -.nut) (said p.nut ((hard (list zong)) r.nut)))
    %re  ?>(?=(%ow -.nut) (nice ~ p.nut))
    %ra  ?>  &(?=(%ow -.nut) ?=(^ t.pax))
         (nice [~ (need (slaw %p i.t.pax))] p.nut)
    %up  ?>(?=(%up -.nut) (toke p.nut))
    %wa  ?>(?=(%wa -.nut) take)
    %ya  ?>(?=(%lq -.nut) (priv p.nut ((hard ^mess) r.nut)))
  ==
--
