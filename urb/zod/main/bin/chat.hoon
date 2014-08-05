!:
::  /=main=/bin/chat/hoon
::
=>  %=    .
        +
      =>  +
      =*  sed  .
      =>  ^/===/lib/pony
      =+  ^=  flag
          $?  %leet
              %monitor
              %noob
              %quiet
              %time
              [%tower p=@p]
              [%s p=path]
          ==
      =+  flags=*(list flag)
      =>  |%
          ++  chat                                      ::  user action
            $%  [%all p=mess]                           ::  say
                [%back p=?(%da %dr %ud) q=@]            ::  backlog
                [%def p=mess]                           ::  use current prompt
                [%how ~]                                ::  help
                [%priv p=@p q=mess]                     ::  private
                [%who ~]                                ::  who
            ==                                          ::
          ++  idad  ,[p=@p q=@t]                        ::  identity
          ++  mess                                      ::  message
            $%  [%do p=@t]                              ::  act
                [%exp p=@t q=tank]                      ::  code
                [%say p=@t]                             ::  say
            ==                                          ::
          ++  prom                                      ::  prompt type
            $%  [%pub ~]                                ::  public message
                [%pri p=ship]                           ::  private message
            ==                                          ::
          ++  user                                      ::  amigos
            $%  [%in p=idad]                            ::  coming on the air
                [%out p=idad]                           ::  signing off
            ==                                          ::
          ++  station  path                             ::
          ++  zung                                      ::  client to server
            $%  [%backlog p=path q=?(%da %dr %ud) r=@]  ::
                [%hola p=station]                       ::
                [%mess p=station q=mess]                ::
                [%tint p=ship]                          ::
            ==                                          ::
          ++  zong                                      ::  server to client
            $%  [%mess p=@da q=ship r=mess]             ::
            ==                                          ::
          --
      =>  |%
          ++  chat
            %+  cook  |=(a=^chat a)
            ;~  pose
              (cold [%how ~] wut)
              (cold [%who ~] tis)
              (stag %back dat)
              (stag %priv ;~(plug ;~(pfix sig fed:ag) ;~(pfix ace mess)))
              (stag %all ;~(pfix pam mess))
              (stag %def mess)
            ==
          ::
          ++  dat
            %+  cook
              |=  p=coin
              ?.  ?=(~ -.p)  [%ud 5]
              ?+  p.p.p  [%ud 5]
                %da  [%da q.p.p]
                %dr  [%dr q.p.p]
                %ud  [%ud q.p.p]
              ==
            ;~(pfix (jest '\\\\ ') nuck:so)
          ::
          ++  expn
            %-  sear
            :_  text
            |=  a=@t
            ^-  (unit ,[p=@t q=tank])
            =+  hun=(rush a wide:vast)
            ?~  hun  ~
            ?~(a ~ [~ a (sell (slap !>(sed) u.hun))])
          ::
          ++  mess
            %+  cook  |=(a=^mess a)
            ;~  pose
              (stag %do ;~(pfix pat text))
              (stag %exp ;~(pfix hax expn))
              (stag %do (full (easy '')))
              (stag %say text)
            ==
          ::
          ++  text  (boss 256 (star prn))
          --
      |%
      ++  idt
        |=  from=idad
          ?:  |(!(lien flags |=(a=flag ?=(%noob a))) =("" q.from))
            (scow %p p.from)
          %-  trip
          %^  cat  3  %^  cat  3  (scot %p p.from)  ' '  q.from
      ++  rend
        |=  [from=idad msg=^mess pre=tape tim=@da] ::  roo=^room
        =+  tst=(lien flags |=(a=flag ?=(%time a)))
        ^-  tank
        ?-  -.msg
            %do
          =+  mes=?:(=(0 p.msg) "remains quietly present" (trip p.msg))
          :-  %leaf
          %+  weld
            ?.  tst  ""  (timestamp tim)
          "{pre}{(idt from)} {mes}"
            %exp
          :~  %rose
              [" " "" ""]
              :-  %leaf
              %+  weld
                ?.  tst  ""  (timestamp tim)
              "{pre}{(idt from)} {(trip p.msg)}"
              q.msg
          ==
            %say
          :-  %leaf
          %+  weld
            ?.  tst  ""  (timestamp tim)
          "{pre}{(idt from)}: {(trip p.msg)}"
        ==
      ++  timestamp
        |=  t=@da
        =+  da=(yell t)
        "{?:((gth 10 h.da) "0" "")}{(scow %ud h.da)}:".
        "{?:((gth 10 m.da) "0" "")}{(scow %ud m.da)} "
      --
      ::
    ==
=>  %=    .
        -
      :-  :*  ami=*(map ,@p ,@t)                        ::
              bud=(sein `@p`-<)                         ::  chat server
              dun=|                                     ::  done
              giz=*(list gift)                          ::  stuff to send
              mon=*?                                    ::  leet mode
              nub=*?                                    ::  monitor mode
              pro=`prom`[%pub ~]                        ::  prompt state
              sta=*station                              ::  station
              sub=*(list path)                          ::  subscriptions
              tod=*(map ,@p ,@ud)                       ::  outstanding, friend
              tst=|                                     ::  timestamp mode
              wak=_@da                                  ::  next heartbeat
          ==
      [who=`@p`-< how=`path`->]
    ==
|=  [est=time *]
|=  args=(list flag)
=.  flags  args
=.  wak  est
=.  bud
  ?:  (lien args |=(a=flag &(?=(^ a) ?=(%tower -.a))))
    (roll args |=([p=flag q=@p] ?:(&(?=(^ p) ?=(%tower -.p)) p.p q)))
  bud
=.  nub  (lien args |=(a=flag ?=(%noob a)))
=.  mon  (lien args |=(a=flag ?=(%monitor a)))
=.  sta
  ?:  (lien args |=(a=flag &(?=(^ a) ?=(%s -.a))))
    (roll args |=([p=flag q=station] ?:(&(?=(^ p) ?=(%s -.p)) p.p q)))
  sta
=.  tst  (lien args |=(a=flag ?=(%time a)))
|-  ^-  bowl
=<  abet:init
|%
++  abet  `bowl`[(flop giz) ?:(dun ~ [~ hope vent(giz ~)])]
++  hope
  :^    [/wa %wa wak]
      [/ya %lq %ya]
    :^  /up  %up  %text
    :_  ""
    ?-  -.pro
      %pub  "& "
      %pri  (weld (scow %p p.pro) " ")
    ==
  %+  welp
    (turn sub |=(pax=path [[%gr pax] [%gr ~]]))
  %+  turn  (~(tap by tod))
  |=  [p=@p q=@ud]
  [[%ra (scot %p p) ~] [%ow ~]]
::
++  iden
  |=  her=@p
  (fall (~(get by ami) her) *@t)
::
++  init
  (joke:(subs:(subs:tint (welp sta /amigos)) (welp sta /mensajes)) %hola sta)
::
++  jake
  |=  [her=@p msg=^mess]
  ^+  +>
  %=  +>.$
    giz  :_(giz [%sq her %ya [%ra (scot %p her) ~] msg])
    pro  [%pri her]
    tod  (~(put by tod) her +((fall (~(get by tod) her) 0)))
  ==
::
++  joke                                                ::  send message
  |=  msg=zung
  ^+  +>
  +>(giz :_(giz [%xz [who %chat] who %zung msg]))
::
++  join
  |=  you=user
  ^+  +>
  ?-  -.you
      %in
    =.  ami  (~(put by ami) p.you)
    ?.  mon  +>.$
    (show %leaf "{(idt p.you)} comes on the air")
      %out
    =.  ami  (~(del by ami) p.p.you)
    ?.  mon  +>.$
    (show %leaf "{(idt p.you)} signs off")
  ==
++  joyn
  |=  yall=(list idad)
  ^+  +>
  =.  ami  (~(gas by ami) yall)
  ?.  mon  +>.$
  (shew (turn yall |=(you=idad [%leaf "{(idt you)} is on the air"])))
::
++  nice                                                ::  got response
  |=  [her=@p kay=cape]
  ^+  +>
  =.  +>
    =+  dyt=(need (~(get by tod) her))
    %_    +>.$
        tod
      ?:  =(1 dyt)
        (~(del by tod) her)
      (~(put by tod) her (dec dyt))
    ==
  ?-  kay
    %good  +>
    %dead  (show %leaf "server {(scow %p her)} choked")
  ==
::
++  priv
  |=  [now=@da her=@p mes=^mess]
  ^+  +>
  (show (rend [her (iden her)] mes "(private) " now))
::
++  said                                                ::  server message
  |=  duz=(list zong)
  ^+  +>
  ?~  duz  +>
  %=    $
      duz  t.duz
      +>
    ?-    -.i.duz
        %mess
      (show (rend [q.i.duz (iden q.i.duz)] r.i.duz "" p.i.duz))
==  ==
::
++  shew  |=(tax=(list tank) +>(giz [[%lo tax] giz]))   ::  print to screen
++  show  |=(tan=tank +>(giz [[%la tan] giz]))          ::  print to screen
::
++  subs
  |=  pax=path
  ^+  +>
  +>(sub [pax sub], giz :_(giz [%zz /g [%gr pax] %show [who %chat] who pax]))
::
++  take  (joke(wak (add ~m1 (max wak est))) %hola sta) ::  beat heart
++  tint  (joke %tint bud)                              ::  init local chat
++  toke                                                ::  user action
  |=  [now=@da txt=@t]
  ^+  +>
  ?:  =(0 txt)  +>
  =+  rey=(rush txt chat)
  ?~  rey
    (show %leaf "invalid input")
  |-
  ?-  -.u.rey
    %all   (joke(pro [%pub ~]) %mess sta p.u.rey)
    %back  (joke %backlog sta p.u.rey q.u.rey)
    %def   $(u.rey ?-(-.pro %pub [%all p.u.rey], %pri [%priv p.pro p.u.rey]))
    %how   (shew (turn (lore ^:@/===pub/src/doc/chat/help/txt) |=(a=@t [%leaf (trip a)])))
    %priv  (jake p.u.rey q.u.rey)
    %who
      %^  show  %rose  [", " "" ""]
      %+  turn  (~(tap by ami))
      |=  p=idad
      :-  %leaf 
      %-  trip
      %^  cat  3  %^  cat  3  (scot %p p.p)  ' '  q.p
  ==
::
++  vent                                                ::  handle event
  |=  [now=@da pax=path nut=note]
  ^-  bowl
  =.  est  now
  =<  abet
  ?+  -.pax  ~&  [%chat-vent-unknown -.nut]  +>.$
    %gr  ?>  ?=(%gr -.nut)
         ?+  p.nut  ~&  %chat-vent-logo-fail  +>.$
           %user   (join ((hard user) q.nut))
           %users  (joyn ((hard (list idad)) q.nut))
           %zong   =+(zog=((hard zong) q.nut) (said ?:(=(who q.zog) ~ [zog ~])))
           %zongs  (said (flop ((hard (list zong)) q.nut)))
         ==
    %up  ?>(?=(%up -.nut) (toke now p.nut))
    %ra  ?>  &(?=(%ow -.nut) ?=(^ t.pax))
         (nice (need (slaw %p i.t.pax)) p.nut)
    %wa  ?>(?=(%wa -.nut) take)
    %ya  ?>  ?=(%lq -.nut)
         =+  n=((soft ^mess) r.nut)
         ?~  n
           ~&  %chat-mess-fail  +>+
         (priv now p.nut u.n)
  ==
--
