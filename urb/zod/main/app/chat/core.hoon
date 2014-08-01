/-  mess,user,users,zing,zong,zung
!:
=>  |%
    ++  axle
      $%  [%0 pad=@p air=(map path station)]
      ==
    ++  blitz
      $%  [%zong p=zong]
          [%user p=user]
      ==
    ++  idad  ,[p=@p q=@t]
    ++  iron
      $%  [%zongs p=(list zong)]
          [%users p=(list idad)]
      ==
    ++  gift
      $%  [%mean ares]
          [%nice ~]
          [%rush blitz]
          [%rust iron]
      ==
    ++  hasp  ,[p=ship q=term]
    ++  move  ,[p=bone q=(mold note gift)]
    ++  note
              $?  $:  %g
              $%  [%mess p=hasp q=ship r=cage]
                  [%nuke p=hasp q=ship]
                  [%show p=hasp q=ship r=path]
              ==  ==  ==
    ++  sign
              $?  $:  %g
              $%  [%mean p=ares]
                  [%nice ~]
                  $:  %rush
                      $=  p
                      $%  [%user q=user]
                          [%zong q=zong]
                  ==  ==
                  $:  %rust
                      $=  p
                      $%  [%users q=users]
                          [%zongs q=(list zong)]
                  ==  ==
              ==  ==  ==
      ++  station
                   $:  msg=(list zong)
                       sub=(unit bone)
                       new=(unit ,[p=@da q=bone])
                       tim=@da
                       ami=(set idad)
                   ==
    --
|_  [hid=hide axle]
++  grab
  |=  sta=path
  (fall (~(get by air) sta) *station)
::
++  since
  |=  [ya=msg=(list zong) tim=@da]
  |-  ^-  (list zong)
  ?:  |(?=(~ msg.ya) (lth p.i.msg.ya tim))  ~
  [i.msg.ya $(msg.ya t.msg.ya)]
::
++  peer
  |=  [ost=bone you=ship pax=path]
  ^-  [(list move) _+>]
  =+  sta=*path
  |-  ^-  [(list move) _+>.^$]
  ?:  ?=(~ pax)
    [~ +>.^$]
  ?.  ?=(~ +.pax)
    $(sta `path`[-.pax sta], pax `path`+.pax)
  =.  sta  (flop sta)
  =+  ya=(grab sta)
  ::  ~&  [%peer ami.ya msg.ya]
  ?+  -.pax  [~ +>.^$]
    %amigos    [[ost %give %rust %users (~(tap in ami.ya))]~ +>.^$]
    %mensajes
      ?~  sub.ya
        [~ +>.^$(air (~(put by air) sta ya(new `[tim.ya ost])))]
      :_  +>.^$
      [ost %give %rust %zongs (since msg.ya tim.ya)]~
  ==
::
++  poke-zung
  |=  [ost=bone you=ship zug=zung]
  ^-  [(list move) _+>]
  ::  ~&  [%poke-zung ost you zug]
  ?.  =(you our.hid)
    [[ost %give %mean ~ %no-sos-mi-amigo ~]~ +>.$]
  ?-    -.zug
      %backlog
    =+  ya=(grab p.zug)
    :_  +>.$
    :-  [ost %give %nice ~]
    %^  yend  you  (welp p.zug /mensajes)
    :*  %give  %rust  %zongs 
        ?:  ?=(%ud q.zug)
          %+  scag  r.zug
          msg.ya
        =+  ^=  tim  ?-(q.zug %da r.zug, %dr (sub lat.hid r.zug))
        (since msg.ya tim)
    ==
  ::
      %hola
    =+  ^=  zag  ^-  move
        :*  ost  %pass  [%mess (scot %ud ost) ~]  %g
            %mess  [pad %radio]  our.hid  %zing  !>(zug)
        ==
    =+  ya=(grab p.zug)
    =.  tim.ya  lat.hid
    =.  air  (~(put by air) p.zug ya)
    ?^  sub.ya  [~[zag] +>.$]
    :_  +>.$(air (~(put by air) p.zug ya(sub `ost)))
    :~  zag
        =+  pax=(welp p.zug /mensajes)
        [ost %pass [%show pax] %g %show [pad %radio] you pax]
        =+  pax=(welp p.zug /amigos)
        [ost %pass [%show pax] %g %show [pad %radio] you pax]
    ==
  ::
      %mess 
    :_  +>.$  :_  ~
    :*  ost  %pass  [%mess (scot %ud ost) ~]  %g
        %mess  [pad %radio]  our.hid  %zing  !>(zug)
    ==
  ::
      %tint
    [[ost %give %nice ~]~ +>.$(pad p.zug, air ?:(=(pad p.zug) air ~))]
  ==
::
++  pour
  |=  [pax=path sih=*]
  ^-  [(list move) _+>]
  =+  sih=((hard sign) sih)
  ::  ~&  [%chat-pour sih]
  ?.  ?=([@ *] pax)  ~&  %chat-pour-strange-path  !!
  ?>  ?=(%g -.sih)
  ?+    i.pax  ~&  %chat-pour-strange-path  !!
      %mess
    ?>  ?=([@ @ ~] pax)
    ?>  ?=(?(%mean %nice) +<.sih)
    [[(slav %ud i.t.pax) %give +.sih]~ +>.$]
  ::
      %show
    =+  rax=(scag (dec (lent t.pax)) t.pax)
    =+  ya=(grab rax)
    ?~  sub.ya  ~&  [%chat-pour-unexpected -.sih +<.sih]  [~ +>.$]
    ::  ~&  [%pour-show lat.hid tim.ya]
    =.  ya
      ?:  ?=(?(%nice %mean) +<.sih)  ya
      ^+  ya
      ?+    `@tas`(snag (dec (lent t.pax)) t.pax)  ~&  %pour-strange-show  ya
          %amigos
        %_    ya
            ami
          ^-  (set idad)
          ?-  +<.sih
            %rust  ?>(?=(%users -.p.sih) (sa q.p.sih))
            %rush
              ?>  ?=(%user -.p.sih)
              ?-  -.q.p.sih
                %in   (~(put in ami.ya) p.q.p.sih)
                %out  (~(del in ami.ya) p.q.p.sih)
              ==
          ==
        ==
          %mensajes
        ^+  ya
        %_   ya
            msg
          ^-  (list zong)
          ?-  +<.sih
            %rush  ?>(?=(%zong -.p.sih) [q.p.sih msg.ya])
            %rust  ?>(?=(%zongs -.p.sih) q.p.sih)
          ==
        ==
      ==
    ?:  &([?=([%rust %zongs] .)]:[+<.sih +>-.sih] .?(new.ya))
      :_  +>.$(air (~(put by air) rax ya(new ~)))
      =+  new=(need new.ya)
      [q.new %give %rust %zongs (since msg.ya p.new)]~
    ?:  (gth ~m2 (sub lat.hid tim.ya))
      :-  (send t.pax %give +.sih)
      +>.$(air (~(put by air) rax ya))
    :-  :-  [u.sub.ya %pass [%show (welp rax /mensajes)] %g %nuke [pad %radio] our.hid]
        :_  (send t.pax %give +.sih)
        [u.sub.ya %pass [%show (welp rax /amigos)] %g %nuke [pad %radio] our.hid]
    +>.$(air (~(put by air) rax ya(sub ~)))
  ==
::
++  send
  |=  [pax=path msg=(mold note gift)]
  ^-  (list move)
  %+  turn  (~(tap in (~(get ju pus.hid) pax)))
  |=(ost=bone [ost msg])
::
++  yend
  |=  [you=ship sta=path msg=(mold note gift)]
  ^-  (list move)
  %+  turn
    %+  skim  (~(tap in (~(get ju pus.hid) sta)))
    |=  b=bone  =(you p:(fall (~(get by sup.hid) b) *(pair ship path)))
  |=  b=bone
  :-  b  msg
--
