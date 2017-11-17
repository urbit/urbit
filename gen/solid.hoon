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
        {$~ $~}
    ==
:-  %noun
=+  top=`path`/(scot %p p.bec)/[q.bec]/(scot %da now)/sys
=+  pax=`path`(weld top `path`[%hoon ~])
=+  arp=`path`(weld top `path`[%ovra ~])
~&  %solid-start
=+  txt=.^(@t %cx (weld pax `path`[%hoon ~]))
=+  rax=.^(@t %cx (weld arp `path`[%hoon ~]))
=+  ^=  ken
    =-  ?>(?=($& -.res) p.res)
    ^=  res  %-  mule  |.
    ~&  %solid-loaded
    =+  gen=(rain pax txt)
    ~&  %solid-parsed
    =+  one=(~(mint ut %noun) %noun gen)
    ~&  %solid-compiled
    =+  two=(~(mint ut p.one) %noun (rain arp rax))
    ~&  %solid-arvo
    [7 q.one q.two]
~&  [%solid-kernel `@ux`(mug ken)]
:-  ken
=+  all=.*(0 ken)
=+  ^=  vay  ^-  (list {p/@tas q/path})
    :~  [%$ /zuse]
        [%f /vane/ford]
        [%c /vane/clay]
        [%g /vane/gall]
        [%a /vane/ames]
        [%b /vane/behn]
        [%d /vane/dill]
        [%e /vane/eyre]
    ==
|-  ^+  all
?~  vay  all
=+  pax=(weld top q.i.vay)
=+  txt=.^(@ %cx (weld pax `path`[%hoon ~]))
=+  sam=[now `ovum`[[%gold ~] [%veer p.i.vay pax txt]]]
~&  [%solid-veer i.vay]
=+  gat=.*(all .*(all [0 42]))
=+  nex=+:.*([-.gat [sam +>.gat]] -.gat)
$(vay t.vay, all nex)

