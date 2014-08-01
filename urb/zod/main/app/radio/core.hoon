/-  mess,user,users,zing,zong
!:
=>  |%
    ++  axle
      $%  [%0 p=(map path ,[p=(list zong) q=(map ship feel)])]
      ==
    ++  blitz
      $%  [%zong p=zong]
          [%user p=user]
      ==
    ++  feel  ,[liv=? tim=@da]
    ++  iron
      $%  [%zongs p=(list zong)]
          [%users p=users]
      ==
    ++  gift
      $%  [%rush blitz]
          [%rust iron]
          [%mean ares]
          [%nice ~]
      ==
    ++  move  ,[p=bone q=(mold note gift)]
    ++  note  ,~
    --
|_  [hid=hide vat=axle]
++  grab
  |=  sta=path
  (fall (~(get by p.vat) sta) *[p=(list zong) q=(map ship feel)])
::
++  ident
  |=  you=ship
  %-  (hard ,@t)
  .^(%a (scot %p our.hid) %name (scot %da lat.hid) (scot %p you) ~)
::
++  peer
  |=  [ost=bone you=ship pax=path]
  ^-  [(list move) _+>]
  ::  ~&  [%radio-peer ost you pax]
  :_  +>.$
  =+  sta=*path
  |-  ^-  (list move)
  ?:  ?=(~ pax)
    ~
  ?.  ?=(~ +.pax)
    $(sta `path`[-.pax sta], pax `path`+.pax)
  =.  sta  (flop sta)
  =+  ya=(grab sta)
  ?+    -.pax  ~
      %mensajes
    :_  ~
    [ost %give %rust %zongs p.ya]
      %amigos
    :_  ~
    :*  ost  %give  %rust  %users
        %+  turn
          %+  skim  (~(tap by q.ya))
          |=  [ship [p=? @da]]  p
        |=  [p=ship [? @da]]  [p (ident p)]
    ==
  ==
::
++  poke-zing
  |=  [ost=bone you=ship zig=zing]
  ^-  [(list move) _+>]
  ::  ~&  [%poke-zing ost you zig]
  ?-    -.zig
      %hola
    =+  ya=(grab p.zig)
    =^  outs  q.ya
      %+  ~(rib by q.ya)  *(list move)
      |=  [p=[p=ship q=feel] q=(list move)]
      =+  liv=(gth ~m3 (sub lat.hid tim.q.p))
      :_  [p.p liv tim.q.p]
      ?:  |(liv !liv.q.p)  q
      %-  welp  :_  q
      (send (welp p.zig /amigos) %give %rush %user %out p.p (ident p.p))
    =.  p.vat  (~(put by p.vat) p.zig [p.ya (~(put by q.ya) you [& lat.hid])])
    :_  +>.$
    :-  [ost %give %nice ~]
    =+  yel=(~(get by q.ya) you)
    ?.  |(?=(~ yel) !liv.u.yel)  outs
    %+  welp  outs
    (send (welp p.zig /amigos) %give %rush %user %in you (ident you))
      %mess
    =+  ya=(grab p.zig)
    ?.  (~(has by q.ya) you)
      [[ost %give %mean ~ %no-te-conozco ~]~ +>.$]
    =+  zog=`zong`[%mess lat.hid you q.zig]
    =.  p.vat  (~(put by p.vat) p.zig [[zog p.ya] q.ya])
    :_  +>.$
    :-  [ost %give %nice ~]
    (send (welp p.zig /mensajes) %give %rush %zong zog)
  ==
::
++  send
  |=  [pax=path msg=(mold note gift)]
  ^-  (list move)
  %+  turn  (~(tap in (~(get ju pus.hid) pax)))
  |=(ost=bone [ost msg])
++  yend
  |=  [you=ship sta=path msg=(mold note gift)]
  ^-  (list move)
  %+  turn
    %+  skim  (~(tap in (~(get ju pus.hid) sta)))
    |=  b=bone  =(you p:(fall (~(get by sup.hid) b) *(pair ship path)))
  |=  b=bone
  :-  b  msg
--
