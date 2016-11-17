::
::::  /hoon/plastic/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  $:  {now/@da eny/@uvI bec/beak}
        {{who/@p $~} $~}
    ==
:-  %noun
=+  top=`path`/(scot %p p.bec)/[q.bec]/(scot %da now)/arvo
=+  pax=`path`(weld top `path`[%hoon ~])
~&  %plastic-start
=+  gen=(reck pax)
~&  %plastic-parsed
=+  ken=q:(~(mint ut %noun) %noun gen)
~&  %plastic-compiled
:-  ken
=+  all=.*(0 ken)
=+  ^=  vent
    |=  {abr/term nam/term}
    =+  pax=(weld top `path`[nam ~])
    =+  txt=.^(@ %cx (weld pax `path`[%hoon ~]))
    `ovum`[[%vane nam ~] [%veer abr pax txt]]
=+  ^=  evo  
    ^-  (list ovum)
    :~  (vent %$ %zuse)
        [[%name (scot %p who) ~] [%veal who]]
        (vent %c %clay)
        (vent %g %gall)
        (vent %f %ford)
        (vent %a %ames)
        (vent %b %behn)
        (vent %d %dill)
        (vent %e %eyre)
    ==
|-  ^+  all
?~  evo  all
~&  [%plastic-step p.i.evo]
=+  gat=.*(all .*(all [0 42]))
=+  nex=+:.*([-.gat [[now i.evo] +>.gat]] -.gat)
$(evo t.evo, all nex)
