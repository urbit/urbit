::  Compile arvo as a pill noun, without compiler changes.
::  usage 
::
::    .urbit/pill +solid
::
::::  /hoon/solid/gen
  ::
/?    310
::
::::
  !:
:-  %say
|=  $:  {now/@da eny/@uvJ bec/beak}
        {arg/$@($~ {top/path $~}) dub/_|}
    ==
?~  arg  $(arg ~[top=`path`/(scot %p p.bec)/[q.bec]/(scot %da now)/sys])
::
:-  %noun
=+  pax=`path`(weld top.arg `path`[%hoon ~])
=+  arp=`path`(weld top.arg `path`[%arvo ~])
~&  %solid-start
=+  txt=.^(@t %cx (weld pax `path`[%hoon ~]))
=+  rax=.^(@t %cx (weld arp `path`[%hoon ~]))
=+  ^=  ken
    =-  ?:(?=($& -.res) p.res (mean (flop p.res)))
    ^=  res  %-  mule  |.
    ~&  %solid-loaded
    =+  gen=(rain pax txt)
    ~&  %solid-parsed
    =+  one=(~(mint ut %noun) %noun gen)
    ~&  %solid-compiled
    ?.  dub
      =+  two=(~(mint ut p.one) %noun (rain arp rax))
      ~&  %solid-arvo
      [7 q.one q.two]
    =/  tri
      '''
      :: XX moveme to, uh arvo probably, this depends on too many names
      |=  [pax=path txt=@t arp=path rax=@t]
      =+  gen=(rain pax txt)
      ~&  %solid-double-parsed
      =+  one=(~(mint ut %noun) %noun gen)
      ~&  %solid-double-compiled
      =+  two=(~(mint ut p.one) %noun (rain arp rax))
      ~&  %solid-arvo
      [7 q.one q.two]
      '''
    =+  all=.*(0 q.one)
    .*  all
    :+  7  =<(+ .*(all [9 2 0+2 1+[p.one tri] 0+7]))
    [9 2 0+2 1+[pax txt arp rax] 0+7]
::
~&  [%solid-kernel `@ux`(mug ken)]
:-  ken
=+  all=.*(0 ken)
=+  ^=  vay  ^-  (list {p/@tas q/path})
    :~  [%$ /zuse]
        [%b /vane/behn]
        [%d /vane/dill]
        [%a /vane/ames]
        [%c /vane/clay]
        [%g /vane/gall]
        [%e /vane/eyre]
        [%f /vane/ford]
        
    ==
|-  ^+  all
?~  vay  all
=+  pax=(weld top.arg q.i.vay)
=+  txt=.^(@ %cx (weld pax `path`[%hoon ~]))
=+  sam=[now `ovum`[[%gold ~] [%veer p.i.vay pax txt]]]
~&  [%solid-veer i.vay]
=+  gat=.*(all .*(all [0 42]))
=+  nex=+:.*([-.gat [sam +>.gat]] -.gat)
$(vay t.vay, all nex)

