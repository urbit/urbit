!:
::  /=main=/lib/pony/hoon
::
|%
++  pogo                                                  ::  merge bowls
  |=  [top=bowl bot=bowl]
  ^-  bowl
  :-  (weld p.top p.bot)
  ?~  q.top  q.bot
  ?~  q.bot  q.top
  :+  ~
    (weld -.u.q.top -.u.q.bot)
  |=  [now=@da pax=path nut=note]
  ^-  bowl
  ?:  (lien -.u.q.top |=(a=slip =(pax p.a)))
    ^$(top (+.u.q.top +<), p.bot ~)
  ^$(bot (+.u.q.bot +<), p.top ~)
::
++  polo                                                  ::  prompt
  |=  [pim=prom pro=tape def=tape use=tape]
  |*  [rul=_rule woo=$+([@da *] bowl)]
  ^-  bowl
  :-  ~  :-  ~
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
++  pond                                                  ::  text block
  |=(lub=@ :_(~ (turn (lore lub) |=(a=@t la/leaf/(trip a)))))
::
++  posh                                                  ::  pause until
  |=  end=@da
  |=  wop=$+(@da bowl)
  ^-  bowl
  :-  ~  :-  ~
  :-  ^-  (list slip)
      :~  [~ %wa end]
      ==
  |=  [now=@da pax=path nut=note]
  (wop now)
::
++  post                                                  ::  request/response
  |=  [him=@p cav=@tas msg=*]
  |=  woo=$+([@da (unit ,*)] bowl)
  ^-  bowl
  =+  leg="waiting on {(scow %p him)}/{(trip cav)}"
  :-  :~  [%sq him cav /request msg]
      ==
  :-  ~
  :-  ^-  (list slip)
      :~  [/request [%rt ~]]
          [/prompt [%up %none leg ~]]
      ==
  |=  [now=@da pax=path nut=note]
  ^-  bowl
  ?+  -.nut  ~&  [%bad-nut nut]  !!
    %rt  (woo now p.nut)
  ==
--
