::  -work: create and mount development desk(s)
::
::    With no arguments, creates and mounts a %work desk.
::    If there are arguments, each one is created and mounted.
::    All desks are begun by merging from our %base desk.
::
/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ desks=(list desk)] arg)
=?  desks  =(~ desks)  [%work]~
|-  ^-  form:m
=*  loop  $
?~  desks  (pure:m !>(ok=&))
::  |merge %work our %base
::
;<  [=ship =desk =case]  bind:m  get-beak:strandio
=/  kiln-merge  [i.desks ship %base case %auto]
;<  ~  bind:m  (poke-our:strandio %hood %kiln-merge !>(kiln-merge))
;<  ~  bind:m  (trace:strandio leaf+"work: merged {<i.desks>}" ~)
::  sleep 10ms to defer to new event
::
::    TODO: This crashes if it's in the same event for some reason.
::
;<  ~  bind:m  (sleep:strandio `@dr`(div ~s1 100))
::  |mount %work
::
=/  pax=path  (en-beam [ship i.desks case] /)
;<  ~  bind:m  (poke-our:strandio %hood %kiln-mount !>([pax i.desks]))
;<  ~  bind:m  (trace:strandio leaf+"work: mounted {<i.desks>}" ~)
::
loop(desks t.desks)
