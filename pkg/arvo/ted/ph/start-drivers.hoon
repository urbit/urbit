/-  spider
/+  *ph-io, *ph-util
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  =bowl:spider  bind:m  get-bowl
;<  ~  bind:m  start-simple
::  must be a better way to background threads
|-
=*  loop  $
;<  ~  bind:m  (sleep ~s5)
loop
