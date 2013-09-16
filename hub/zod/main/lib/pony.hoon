!:
::  /=main=/lib/pony/hoon
::
|%
++  polo                                                  ::  prompt
  |=  [gim=(list gift) pim=prom pro=tape def=tape use=tape]
  |*  [rul=_rule woo=||([@da *] bowl)]
  ^-  bowl
  :-  gim  :-  ~
  :-  :~  [[%polo ~] [%up pim pro def]]
      ==
  |=  [now=@da pax=path nut=note]
  ^-  bowl
  ?>  &(=([%polo ~] pax) ?=(%up -.nut))
  =+  rey=(rush p.nut rul)
  ?~  rey
    :-  [[%la %leaf ?~(use "invalid response" use)] ~]
    :-  ~
    [[[[%polo ~] [%up pim pro def]] ~] ..$]
  (woo now u.rey)
::
++  pomo  |=([gud=gift bol=bowl] [[gud p.bol] q.bol])
++  pomp  |=([tix=tape bol=bowl] (pomo la/leaf/tix bol))
++  pond                                                  ::  show text block
  |=  [lub=@ bol=bowl]  
  ^-  bowl 
  :_(q.bol (weld (turn (lore lub) |=(a=@t la/leaf/(trip a))) p.bol))
::
++  posh                                                  ::  pause until
  |=  end=@da
  |=  [gim=(list gift) wop=||(@da bowl)]
  ^-  bowl
  :-  gim  :-  ~
  :-  ^-  (list slip)
      :~  [~ %wa end]
      ==
  |=  [now=@da pax=path nut=note]
  (wop now)
::
++  post                                                  ::  request/response
  |=  [him=@p [cho=@ta chu=@ta] msg=*]
  |=  woo=||([@da (unit ,*)] bowl)
  ^-  bowl
  =+  leg="waiting for {(scow %p him)}"
  :-  ~  :-  ~
  :-  ^-  (list slip)
      :~  [/request [%yo him cho msg]]
          [/response [%oy chu]]
          [/prompt [%up %none leg ~]]
      ==
  |=  [now=@da pax=path nut=note] 
  ^-  bowl
  ?+    -.nut  !!
      %oy  (woo now s.nut)
      %yo  
    ?.  =(%good q.nut)  (woo now ~)
    :-  ~  :-  ~
    :_  ..$
    ^-  (list slip)
    :~  [/response [%oy chu]]
        [/prompt [%up %none leg ~]]
    ==
  ==
::
++  pour
  |=  $:  est=@da
          dyz=(list ,[p=@tas q=[p=ship q=disc r=moat]])
          wop=||([@da (map ,@tas ,[p=(unit ,@ud) q=(list frog)])] bowl)
      ==
  ^-  bowl
  ?~  dyz  (wop est ~)
  =+  :*  zyd=(~(gas by *(map ,@tas ,[p=ship q=disc r=moat])) dyz)
          fyx=*(map ,@tas ,[p=(unit ,@ud) q=(list frog)])
      ==
  =<  apex
  |%
  ++  apex
    ^-  bowl
    ?~  zyd  done
    :-  ~  :-  ~
    :-  bite
    |=  [now=@da how=path wat=note]
    ?>  ?=([%es *] wat)
    ?>  ?=([%pull @ ~] how)
    apex:(bice(est now) i.t.how p.wat)
  ::
  ++  bice
    |=  [cyt=@tas rot=riot]
    ^+  +>
    ?~  rot
      ~&  [%bice-done cyt]
      +>(zyd (~(del by zyd) cyt))
    ?>  ?=(%ud -.q.p.u.rot)
    =+  geb=(need (~(get by zyd) cyt))
    =+  saq=(need (~(get by fyx) cyt))
    %=    +>
        zyd
      %+  ~(put by zyd)  cyt
      geb(p.r.q q.p.u.rot)
    ::
        fyx
      %+  ~(put by fyx)  cyt
      :-  ?~(p.saq [~ p.q.p.u.rot] p.saq)
      [((hard frog) r.u.rot) q.saq]
    ==
  ::
  ++  bite
    ^-  (list slip)
    =+  who=?>(?=(^ zyd) p.n.zyd)
    :-  `slip`[/prod [%up %none "waiting for <who> at <est>..." ~]]
    =+  fuh=(~(tap by zyd) ~)
    %+  turn  fuh
    |=  [a=@tas b=[p=ship q=disc r=moat]]
    `slip`[[%pl a ~] %es p.b q.b [%| r.b]]
  ::
  ++  done
    ^-  bowl
    (wop est fyx)
  --
--
