/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ arg=path] arg)
;<  ~  bind:m  (send-raw-card:strandio %pass / %arvo %a %keen arg)
;<  [wire sign=sign-arvo]  bind:m  take-sign-arvo:strandio
?>  ?=(%tune +<.sign)
(pure:m !>(data.sign))
