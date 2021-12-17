/-  spider,  hood
/+  strandio, resource, view=group-view
=> 
|%
++  strand  strand:spider
++  hood-poke
  |=  =cage
  (poke-our:strandio %hood cage)
::
++  nuke
  |=  =dude:gall
  (hood-poke kiln-nuke+!>([dude %landscape]))
--
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
:: !!  :: this thread should never be run on a working ship
;<  =bowl:spider  bind:m  get-bowl:strandio
=/  live=(list dude:gall)
  (get-apps-live:hood our.bowl %landscape now.bowl)
~&  live
|-  
=*  loop  $
?~  live  
  ;<  ~  bind:m  (hood-poke kiln-rein+!>([%landscape *rein:hood]))
  (pure:m !>(~))
;<  ~  bind:m  (nuke i.live)
loop(live t.live)
