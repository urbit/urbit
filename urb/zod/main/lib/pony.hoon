!:
::  /=main=/lib/pony/hoon
::
|%
++  polo                                                  ::  prompt
  |=  [gim=(list gift) pim=prom pro=tape use=tape]
  |*  [rul=_rule woo=||([@da *] bowl)]
  ^-  bowl
  :-  gim  :-  ~
  :-  :~  [[%polo ~] [%up pim pro]]
      ==
  |=  [now=@da pax=path nut=note]
  ^-  bowl
  ?>  &(=([%polo ~] pax) ?=(%up -.nut))
  =+  rey=(rush p.nut rul)
  ?~  rey
    :-  [[%la %leaf ?~(use "invalid response" use)] ~]
    :-  ~
    [[[[%polo ~] [%up pim pro]] ~] ..$]
  (woo now u.rey)
::
++  pomo  |=([gud=gift bol=bowl] [[gud p.bol] q.bol])
++  pomp  |=([tix=tape bol=bowl] (pomo la/leaf/tix bol))
++  pond                                                  ::  show text file
  |=  [lub=@ bol=bowl]  
  :_(bol (turn (lore lub) |=(a=@t la/leaf/(trip a))))
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
          [/prompt [%up %none leg]]
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
        [/prompt [%up %none leg]]
    ==
  ==
--
