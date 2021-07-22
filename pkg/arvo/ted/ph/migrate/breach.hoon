/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=+  !<(who=(list @p) arg)
=/  m  (strand ,vase)
;<  ~  bind:m  start-simple
=?  who  ?=(~ who)  ~[~zod ~bus ~web]
|-
=*  loop  $
?~  who
::  ;<  ~  bind:m  (send-hi ~zod ~bus)
::  ;<  ~  bind:m  (send-hi ~zod ~web)
::  ;<  ~  bind:m  (send-hi ~bus ~zod)
::  ;<  ~  bind:m  (send-hi ~bus ~web)
::  ;<  ~  bind:m  (send-hi ~web ~zod)
::  ;<  ~  bind:m  (send-hi ~web ~bus)
  (pure:m *vase)
;<  ~  bind:m  (breach i.who)
;<  ~  bind:m  (init-ship i.who |)
loop(who t.who)
