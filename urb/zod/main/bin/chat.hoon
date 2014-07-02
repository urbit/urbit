!:
::  /=main=/bin/chat/hoon
::
=>  %=    .
        +
      =>  +
      =>  ^/===/lib/pony
      =+  ^=  flag
          $?  [%haus p=@p]
              [%s p=path]
          ==
      =+  flags=*(list flag)
      =>  |%
          ++  chat                                      ::  user action
            $%  [%all p=mess]                           ::  say
                [%back p=@da]                           ::  backlog
                [%how ~]                                ::  help
                [%who ~]                                ::  who
            ==                                          ::
          ++  mess                                      ::  message
            $%  [%do p=@t]                              ::  act
                [%exp p=@t q=tank]                      ::  code
                [%say p=@t]                             ::  say
            ==                                          ::
          ++  station  path                             ::
          ++  zing                                      ::  client to server
            $%  [%backlog p=path q=@da]                 ::
                [%hola p=station]                       ::
                [%mess p=station q=mess]                ::
            ==                                          ::
          ++  zong                                      ::  server to client
            $%  [%mess p=@da q=ship r=mess]             ::
            ==                                          ::
          --
      =>  |%
          ++  chat
            |=  now=@da
            %+  cook  |=(a=^chat a)
            ;~  pose
              (cold [%how ~] wut)
              (cold [%who ~] tis)
              (stag %back (dat now))
              (stag %all mess)
            ==
          ::
          ++  dat
            |=  now=@da
            %+  cook
              |=  [p=@tas q=@]
              ?+  p  `@da`0
                %da  `@da`q
                %dr  `@da`(sub now q)
              ==
            ;~(pfix (jest '\\\\ ~') crub:so)
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
      ++  rend
        |=  [from=@p msg=^mess] ::  roo=^room
        ^-  tank
        ?-  -.msg
          %do   =+  mes=?:(=(0 p.msg) "remains quietly present" (trip p.msg))
                :-  %leaf
                "{<from>} {mes}"
          %exp  :~  %rose
                    [" " "" ""]
                    :-  %leaf
                    "{<from>} {(trip p.msg)}"
                    q.msg
                ==
          %say  [%leaf "{<from>}: {(trip p.msg)}"]
        ==
      --
      ::
    ==
=>  %=    .
        -
      :-  :*  ami=*(set ship)
              bud=(sein `@p`-<)                         ::  chat server
              dun=|                                     ::  done
              giz=*(list gift)                          ::  stuff to send
              sta=*station                              ::  station
          ==
      [who=`@p`-< how=`path`->]
    ==
|=  [est=time *]
|=  args=(list flag)
=.  flags  args
=.  bud
  ?:  (lien args |=(a=flag ?=(%haus -.a)))
    (roll args |=([p=flag q=@p] ?:(?=(%haus -.p) p.p q)))
  bud
=.  sta
  ?:  (lien args |=(a=flag ?=(%s -.a)))
    (roll args |=([p=flag q=station] ?:(?=(%s -.p) p.p q)))
  sta
|-  ^-  bowl
=<  abet:init
|%
++  abet  `bowl`[(flop giz) ?:(dun ~ [~ hope vent(giz ~)])]
++  hope
  :~
      [/gr [%gr ~]]
      ^-  slip
      :-  /up
      :+  %up  %text
      ["& " ""]
  ==
::
++  init  (subs:(subs:(joke %hola sta) (welp sta /amigos)) (welp sta /mensajes))
++  joke                                                ::  send message
  |=  msg=zing
  ^+  +>
  +>(giz :_(giz [%xz [bud %radio] who %zing msg]))
::
++  join
  |=  you=ship
  ^+  +>
  %+  show(ami (~(put in ami) you))
    %leaf
  "{(scow %p you)} comes on the air"
++  joyn
  |=  yall=(list ship)
  ^+  +>
  %-  shew(ami (~(gas in ami) yall))
  (turn yall |=(you=ship [%leaf "{(scow %p you)} comes on the air"]))
::
++  said                                                ::  server message
  |=  duz=(list zong)
  ^+  +>
  ?~  duz  +>
  %=    $
      duz  t.duz
      +>
    =.  giz
      ?.  ?&  ::  ?=(%mess -.i.duz)
              =+  ^=  r
                  %+  rexp  (scow %p who)
                  (trip =>(r.i.duz ?@(+ p p)))
              &(!=(~ r) !=([~ ~] r) !=([~ ~ ~] r))
          ==
        giz
      [[%xy /d [%blit [%bel ~]~]] giz]
    %-  show
    ^-  tank
    ?-    -.i.duz
      %mess  (rend q.i.duz r.i.duz)
==  ==
::
++  shew  |=(tax=(list tank) +>(giz [[%lo tax] giz]))   ::  print to screen
++  show  |=(tan=tank +>(giz [[%la tan] giz]))          ::  print to screen
::
++  subs
  |=  pax=path
  ^+  +>
  +>(giz :_(giz [%zz /g /gr %show [bud %radio] who pax]))
::
++  toke                                                ::  user action
  |=  [now=@da txt=@t]
  ^+  +>
  ?:  =(0 txt)  +>
  =+  rey=(rush txt (chat now))
  ?~  rey
    (show %leaf "invalid input")
  ?-  -.u.rey
    %all   (joke %mess sta p.u.rey)
    %back  (joke %backlog sta p.u.rey)
    %how   (shew (turn (lore ^:@/===doc%/help/txt) |=(a=@t [%leaf (trip a)])))
    %who   (show %rose [", " "" ""] (turn (~(tap in ami)) |=(p=ship >p<)))
  ==
::
++  vent                                                ::  handle event
  |=  [now=@da pax=path nut=note]
  ^-  bowl
  =.  est  now
  =<  abet
  ?+  -.pax  ~&  [%chat-vent-unknown -.nut]  +>.$
    %gr  ?>  ?=(%gr -.nut)
         ?+  p.nut  ~&  %vent-rush-logo-fail  +>.$
           %user   (join ((hard ship) q.nut))
           %users  (joyn ((hard (list ship)) q.nut))
           %zong   (said [((hard zong) q.nut) ~])
           %zongs  ~&  [%chat-zongs ((hard (list zong)) q.nut)]  (said ((hard (list zong)) q.nut))
         ==
    %up  ?>(?=(%up -.nut) (toke now p.nut))
  ==
--
