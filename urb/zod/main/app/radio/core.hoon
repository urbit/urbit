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
    ++  idad  ,[p=@p q=@t]
    ++  iron
      $%  [%zongs p=(list zong)]
          [%users p=(list idad)]
      ==
    ++  gift
      $%  [%rush blitz]
          [%rust iron]
          [%nice ~]
      ==
    ++  mess                                            ::  message
      $%  [%do p=@t]                                    ::  act
          [%exp p=@t q=tank]                            ::  code
          [%say p=@t]                                   ::  speak
      ==
    ++  move  ,[p=bone q=(mold note gift)]
    ++  note  ,~
    ++  user
      $%  [%in p=idad]
          [%out p=idad]
      ==
    ++  zing
      $%  [%backlog p=path q=?(%da %dr %ud) r=@]
          [%hola p=path]
          [%mess p=path q=mess]
      ==
    ++  zong
      $%  [%mess p=@da q=ship r=mess]
      ==
    --
|_  [hid=hide vat=axle]
++  grab
  |=  sta=path
  (fall (~(get by p.vat) sta) *[p=(list zong) q=(map ship feel)])
::
++  ident
  |=  you=ship
  ((hard ,@t) .^(%a (scot %p our.hid) %name (scot %da lat.hid) (scot %p you) ~))
::
++  since
  |=  [ya=p=(list zong) tim=@da]
  %-  flop
  |-  ^-  (list zong)
  ?:  |(?=(~ p.ya) (lth p.i.p.ya tim))  ~
  [i.p.ya $(p.ya t.p.ya)]
::
++  peer
  |=  [ost=bone you=ship pax=path]
  ^-  [(list move) _+>]
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
    [ost %give %rust %zongs (since p.ya tim:(fall (~(get by q.ya) you) *feel))]
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
  ?-    -.zig
      %backlog
    =+  ya=(grab p.zig)
    :_  +>.$
    :-  [ost give/nice//]
    %^  yend  you  (welp p.zig /mensajes)
    :*  %give  %rust  %zongs 
        ?:  ?=(%ud q.zig)
          %-  flop
          %+  scag  r.zig
          p.ya
        =+  ^=  tim  ?-(q.zig %da r.zig, %dr (sub lat.hid r.zig))
        (since p.ya tim)
    ==
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
    :-  [ost give/nice//]
    ?:  (~(has by q.ya) you)
      outs
    %+  welp  outs
    (send (welp p.zig /amigos) %give %rush %user %in you (ident you))
      %mess
    =+  zog=`zong`[%mess lat.hid you q.zig]
    =+  ya=(grab p.zig)
    =.  p.vat  (~(put by p.vat) p.zig [[zog p.ya] q.ya])
    :_  +>.$
    :-  [ost give/nice//]
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
