/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
::=;  loch
=+  !<([~ arg=[=dev:loch =wut:loch =cmd:loch =cnt:loch]] arg)
;<  now=@da  bind:m  get-time:strandio
;<  ~  bind:m  (send-raw-card:strandio [%pass /ted/loch/(scot %da now) %arvo %l %read dev.arg wut.arg cmd.arg cnt.arg])
::;<  ~  bind:m  (scry:strandio @ux /l/
(pure:m !>("loch-read suc"))
