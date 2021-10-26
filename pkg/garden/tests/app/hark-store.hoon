/-  hark=hark-store
/+  *test, re=hark-unreads
/=  agent  /app/hark-store
|%
++  place
  ^-  place:hark
  [%landscape /graph/~zod/test]
::
++  bin
  ^-  bin:hark
  [/ place]

::
++  body
  |=  run=@
  :*  ~[text/'Title']
      ~[text/(crip "Contents {(scow %ud run)}")]
      `time`(add (mul ~s1 run) *time)
      /
      /test
  ==
::
++  add-note
  |=  run=@
  ^-  action:hark
  [%add-note bin (body run)]
::
++  read-count
  ^-  action:hark
  [%read-count place]
::
+$  state
  $:  %9
      places=(map place:hark stats:hark)
      seen=timebox:hark
      unseen=timebox:hark
      =archive:hark
      half-open=(map bin:hark @da)
  ==
++  bowl
  |=  run=@ud
  ^-  bowl:gall
  :*  [~zod ~zod %hark-store]
      [~ ~]
      [run `@uvJ`(shax run) (add (mul run ~s1) *time) [~zod %garden ud+run]]
  ==
--
|%
::
++  test-half-open
  =|  run=@ud 
  =^  mov1  agent  
     (~(on-poke agent (bowl run)) %hark-action !>((add-note run)))
  =^  mova  agent
     (~(on-poke agent (bowl run)) %noun !>(%sane))
  =.  run  +(run)
  =^  mov2  agent  
     (~(on-poke agent (bowl run)) %hark-action !>(read-count))
  =^  mov3  agent
     (~(on-poke agent (bowl run)) %noun !>(%sane))
  =/  expected-archive=notification:hark
    [(add *time (mul ~s1 0)) bin ~[(body 0)]] 
  =+  !<(=state on-save:agent)
  =/  actual-archive=notification:hark
    (~(got re archive.state) (add *time ~s1) bin)
  (expect-eq !>(expected-archive) !>(actual-archive))
::
++  test-half-open-double
  =|  run=@ud 
  =^  mov1  agent  
     (~(on-poke agent (bowl run)) %hark-action !>((add-note run)))
  =.  run  +(run)
  =^  mov2  agent  
     (~(on-poke agent (bowl run)) %hark-action !>(read-count))
  =.  run  +(run)
  =^  mov3  agent
     (~(on-poke agent (bowl run)) %hark-action !>((add-note run)))
  =.  run  +(run)
  =^  mov4  agent
     (~(on-poke agent (bowl run)) %hark-action !>(read-count))
  =.  run  +(run)
  =^  mov5  agent
     (~(on-poke agent (bowl run)) %noun !>(%sane))
  =/  expected-archive=notification:hark
    [(add *time (mul ~s1 2)) bin ~[(body 2) (body 0)]] 
  =+  !<(=state on-save:agent)
  =/  actual-archive=notification:hark
    (~(got re archive.state) (add *time ~s1) bin)
  (expect-eq !>(expected-archive) !>(actual-archive))
::
++  test-half-open-capped
  =|  run=@ud 
  |-
  ?:  =(run 31)
    =+  !<(=state on-save:agent)
    (expect-eq !>(~) !>(half-open.state))
  =^  movs  agent  
     (~(on-poke agent (bowl run)) %hark-action !>((add-note run)))
  =^  mavs  agent  
     (~(on-poke agent (bowl run)) %hark-action !>(read-count))
  $(run +(run))
::
--
