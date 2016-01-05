::
::::  /hoon+xolid+gen
  ::
/?  314
::
::::
  !:
:-  %say
|=  $:  {now/@da eny/@uvI bec/beak}
        {$~ $~}
    ==
:-  %noun
=+  top=`path`/(scot %p p.bec)/[q.bec]/(scot %da now)/arvo
=+  one=`path`(weld top `path`[%one ~])
=+  two=`path`(weld top `path`[%two ~])
~&  [%xolid-one-start one]
=+  gen=(reck one)
~&  %xolid-one-parsed
=+  ken=q:(~(mint ut %noun) %noun gen)
~&  %xolid-one-compiled
=+  src=.^(%cx (weld two `path`[%hoon ~]))
~&  [%xolid-two-start two]
=+  nek=.*(src ken)
~&  %xolid-two-compiled
:-  nek
=+  all=.*(0 nek)
=+  ^=  vay  ^-  (list {p/@tas q/@tas})
    :~  [%$ %zuse]
        [%g %gall]
        [%f %ford]
        [%a %ames]
        [%b %behn]
        [%c %clay]
        [%d %dill]
        [%e %eyre]
    ==
|-  ^+  all
?~  vay  all
=+  pax=(weld top `path`[q.i.vay ~])
=+  txt=((hard @) .^(%cx (weld pax `path`[%hoon ~])))
=+  sam=[now `ovum`[[%gold ~] [%veer p.i.vay pax txt]]]
~&  [%xolid-veer i.vay]
=+  gat=.*(all .*(all [0 42]))
=+  nex=+:.*([-.gat [sam +>.gat]] -.gat)
$(vay t.vay, all nex)
