!:
::  /=main=/bin/tach/hoon
::
=>  %=    .
        -
      :-  :*  bud=(sein `@p`-<)
              wak=@da
          ==
      [who=`@p`-< how=`path`->]
    ::
        +
      =>  +
      =>  ^/===/lib/pony
      =>  ^/===/lib/chat
      =>  |%
          ++  text  (boss 256 (star ;~(pose (shim 32 126) (shim 128 255))))
          ++  mess
            %+  cook  |=(a=^mess a)
            ;~  pose
              (stag %do ;~(pfix pat text))
              (stag %qu text)
            ==
          ++  chat
            %+  cook  |=(a=^chat a)
            ;~  pose
              (cold [%how ~] wut)
              (cold [%who ~] tis)
              (stag %all (stag %& mess))
              (stag %say ;~(plug ;~(pfix sig fed:ag) ;~(pfix ace mess)))
              (stag %all (stag %& mess))
            ==
          --
      |%
      ++  rend
        |=  [chr=tape nym=tape dum=^mess]
        ^-  tank
        ?-  -.dum
          %do  =+  msg=?:(=(0 p.dum) "remains quietly present" (trip p.dum))
               [%leaf "{chr}{nym} {(trip p.dum)}"]
          %ex  [%rose [" " "" ""] [%leaf "{chr}{(trip p.dum)}"] q.dum ~]
          %qu  [%leaf "{chr}{nym}: {(trip p.dum)}"]
        ==
      --
    ==
|=  [est=time *]
|=  ~
=.  wak  est
|-  ^-  bowl
=<  [init ~ hope vent]
|%
++  hope                                                ::  wait for events
  ^-  (list slip)
  :~  [/oy [%lq %oy]]
      [/re [%ow ~]]
      [/up [%up %text "; " ""]]
      [/wa [%wa wak]]
      [/ya [%lq %ya]]
  ==
::
++  init                                                ::  initial actions
  ^-  (list gift)
  :~  [%sq bud %yo /re `zing`[%who ~]]
  ==
::
++  priv                                                ::  private message
  |=  [her=@p mes=^mess] 
  ^-  [(list gift) _+>]
  :_  +>
  :~  [%la (rend "> " (trip (numb her est)) mes)]
  ==
::
++  said                                                ::  server message
  |=  [her=@p duz=(list zong)]
  ^-  [(list gift) _+>]
  :_  +>
  %+  turn  duz
  |=  dum=zong
  :-  %la
  ^-  tank
  ?-  -.dum
    %all  (rend ?:(=(%white p.dum) "& " "| ") (trip q.q.dum) r.dum)
    %new  [%leaf "{(trip q.p.dum)} is in the building"]
    %out  [%leaf "{(trip q.p.dum)} has left the building"]
    %who  [%palm [" " "" "" ""] (turn p.dum |=(a=user [%leaf (trip q.a)]))]
  ==
::
++  take                                                ::  alarm event
  |-  ^-  [(list gift) _+]
  :_  +(wak (add ~m1 wak))
  :~  [%sq bud %yo /re [%ego est]]
  ==
::
++  toke                                                ::  user prompt
  |=  txt=@t
  ^-  [(list gift) _+>]
  :_  +>
  =+  rey=(rush txt chat)
  :_  ~
  ?~  rey  
    [%la %leaf "invalid command"]
  ?+  -.u.rey  
          [%sq bud %yo /re `zing`u.rey] 
    %how  [%la %leaf "help file goes here :-|"]
    %say  [%sq p.u.rey %ya /re `^mess`q.u.rey]
  ==
::
++  vent                                                ::  handle event
  |=  [now=@da pax=path nut=note]
  ^-  bowl
  =.  est  now
  =^  gaf  +>
    ?+  pax  [~ +>]
      /oy  ?>(?=(%lq -.nut) (said p.nut ((hard (list zong)) r.nut)))
      /re  ?>(?=(%ow -.nut) [~ +>])
      /up  ?>(?=(%up -.nut) (toke p.nut))
      /wa  ?>(?=(%wa -.nut) take)
      /ya  ?>(?=(%lq -.nut) (priv p.nut ((hard ^mess) r.nut)))
    ==
  [gaf ~ hope ..$]
--
