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
          ++  text  (boss 256 (star qit))
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
      [/up [%up %text ": " ""]]
      [/wa [%wa wak]]
      [/ya [%lq %ya]]
  ==
::
++  init                                                ::  initial actions
  ^-  (list gift)
  :~  [%sq bud %yo /re `zing`[%who ~]]
  ==
::
++  said
  ^-  (
::
++  vent
  |=  [now=@da pax=path nut=note]
  ^-  bowl
  =.  est  now
  ?+    pax  !!
      /oy  (said p.nut ((hard (list zong)) r.nut))

    ?>  ?=(%lq -.nut)
    =+  duz=((hard (list zong)) r.nut)
    :_  ~  
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
      /re  ?>(?=(%ow -.nut) [~ ~])
  ::
      /up 
    ?>  ?=(%up -.nut)
    =+  ^=  rey  ^-  (unit ^chat)
        (rush p.nut chat)
    ?~  rey  
      [[[%la %leaf "invalid command"] ~] ~]
    ?:  ?=(%how -.u.rey)
      [[[%la %leaf "help file goes here :-|"] ~] ~]
    :_  ~  :_  ~  
    ^-  gift
    :-  %sq
    ?:  ?=(%say -.u.rey)
      [p.u.rey %ya /re q.u.rey]
    [bud %yo /re ^-(zing u.rey)]
  ::
      /wa  [[`gift`[%sq bud %yo /re `zing`[%ego now]] ~] ~]
  ::
      /ya
    ?>  ?=(%lq -.nut)
    [[[%la (rend "> " (trip (numb p.nut now)) ((hard ^mess) r.nut))] ~] ~]
    
::
^-  bowl
%-  pogo
:_  ?.(=(/wa pax) ^$ ^$(wak (add ~s10 wak)))
^-  bowl
?+    pax  !!
    /oy
  ?>  ?=(%lq -.nut)
  =+  duz=((hard (list zong)) r.nut)
  :_  ~  
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
    /re  ?>(?=(%ow -.nut) [~ ~])
::
    /up 
  ?>  ?=(%up -.nut)
  =+  ^=  rey  ^-  (unit ^chat)
      (rush p.nut chat)
  ?~  rey  
    [[[%la %leaf "invalid command"] ~] ~]
  ?:  ?=(%how -.u.rey)
    [[[%la %leaf "help file goes here :-|"] ~] ~]
  :_  ~  :_  ~  
  ^-  gift
  :-  %sq
  ?:  ?=(%say -.u.rey)
    [p.u.rey %ya /re q.u.rey]
  [bud %yo /re ^-(zing u.rey)]
::
    /wa  [[`gift`[%sq bud %yo /re `zing`[%ego now]] ~] ~]
::
    /ya
  ?>  ?=(%lq -.nut)
  [[[%la (rend "> " (trip (numb p.nut now)) ((hard ^mess) r.nut))] ~] ~]
==
