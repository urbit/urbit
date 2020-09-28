/-  spider
/+  *ph-io
=,  strand=strand:spider
=>
|%
++  import-all
  |=  [who=@p by-app=(list [@tas @])]
  =/  m  (strand:spider ,~)
  ^-  form:m
  =*  loop  $
  ?~  by-app  (pure:m ~)
  =/  [app=@tas data=@]  i.by-app
  ;<  ~  bind:m  (poke-app who app %import data)
  loop(by-app t.by-app)
--
^-  thread:spider
|=  arg=vase
=+  !<([who=@p by-app=(list [@tas @]) ~] arg)
=/  m  (strand ,vase)
;<  az=tid:spider  bind:m  start-azimuth
;<  ~  bind:m  (import-all who by-app)
;<  ~  bind:m  end-azimuth
(pure:m *vase)
