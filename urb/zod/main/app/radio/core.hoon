!:
=>  |%
    ++  axle
      $%  [%0 p=(map path ,[p=(list zong) q=(set ship)])]
      ==
    ++  blitz
      $%  [%zong p=zong]
          [%user p=ship]
      ==
    ++  iron
      $%  [%zongs p=(list zong)]
          [%users p=(list ship)]
      ==
    ++  gift
      $%  [%rush blitz]
          [%rust iron]
          [%rasp ~]
      ==
    ++  mess                                            ::  message
      $%  [%do p=@t]                                    ::  act
          [%exp p=@t q=tank]                            ::  code
          [%say p=@t]                                   ::  speak
      ==
    ++  move  ,[p=bone q=(mold note gift)]
    ++  note  ,~
    ++  zing
      $%  [%backlog p=path q=@da]
          [%hola p=path]
          [%mess p=path q=mess]
      ==
    ++  zong
      $%  [%mess p=@da q=ship r=mess]
      ==
    --
|=  *
|_  [hid=hide vat=axle]
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
  ?+    -.pax  ~
      %mensajes
    :_  ~
    :*  ost  %give  %rust  %zongs
        p:(fall (~(get by p.vat) sta) [p=*(list zong) q=*(set ship)])
    ==
      %amigos
    :_  ~
    :*  ost  %give  %rust  %users
        (~(tap in q:(fall (~(get by p.vat) sta) [p=*(list zong) q=*(set ship)])))
    ==
  ==
::
++  poke-zing
  |=  [ost=bone you=ship zig=zing]
  ^-  [(list move) _+>]
  ?-    -.zig
      %backlog
    =+  ya=(fall (~(get by p.vat) p.zig) [p=*(list zong) q=*(set ship)])
    :_  +>.$
    %+  send  (welp p.zig /mensajes)
    :*  %give  %rust  %zongs 
        |-  ^-  (list zong)
        ?:  |(?=(~ p.ya) (lth p.i.p.ya q.zig))  ~
        [i.p.ya $(p.ya t.p.ya)]
    ==
      %hola
    =+  ya=(fall (~(get by p.vat) p.zig) [p=*(list zong) q=*(set ship)])
    ?:  (~(has in q.ya) you)
      [~ +>.$]
    =.  p.vat  (~(put by p.vat) p.zig [p.ya (~(put in q.ya) you)])
    [(send (welp p.zig /amigos) %give %rush %user you) +>.$]
      %mess
    =+  zog=`zong`[%mess lat.hid you q.zig]
    =+  ya=(fall (~(get by p.vat) p.zig) [p=*(list zong) q=*(set ship)])
    =.  p.vat  (~(put by p.vat) p.zig [[zog p.ya] q.ya])
    [(send (welp p.zig /mensajes) %give %rush %zong zog) +>.$]
  ==
::
++  send
  |=  [pax=path msg=(mold note gift)]
  ^-  (list move)
  %-  turn  :_  |=(ost=bone [ost msg])
  ^-  (list bone)
  %+  ~(rep by sup.hid)  *(list bone)
  |=  [p=[p=bone q=[ship path]] q=(list bone)]  ^-  (list bone)
  ?.  =(pax +.q.p)  q
  [p.p q]
--
