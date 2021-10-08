/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
?.  ?=(~ q.arg)
  (strand-fail:strand %has-arg ~)
;<  ~  bind:m  (send-raw-card:strandio %pass /mounts %arvo %c %boat ~)
;<  res=[=wire sign=sign-arvo]  bind:m  take-sign-arvo:strandio
?.  ?=([%mounts ~] wire.res)
  (strand-fail:strand %bad-wire ~)
?.  ?=([%clay %hill *] sign.res)
  (strand-fail:strand %bad-sign ~)
(pure:m !>(`(set @tas)`(silt p.sign.res)))
