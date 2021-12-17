/-  spider
/+  strandio
=,  strand=strand:spider
|%
++  info
  |=  [=desk =nori:clay]
  =/  m  (strand ,~)
  (send-raw-card:strandio %pass /info %arvo %c %info desk nori)
--
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =path] arg)
=/  bec=^path  (scag 3 path)
?>  ?=([@ @ @ *] path)
;<  =bowl:spider  bind:m  get-bowl:strandio
?>  =((slav %p i.path) our.bowl)
=+  .^(paths=(list ^path) %ct path)
~&  "Deleting {<paths>}"
|-  =*  loop  $
?~  paths
  (pure:m *vase)
;<  ~  bind:m  (info (fray:space:userlib (welp bec i.paths)))
loop(paths t.paths)
