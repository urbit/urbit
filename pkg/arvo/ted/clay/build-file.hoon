/-  spider
/+  strand, strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ pax=path] arg)
?~  bem=(de-beam pax)
  (strand-fail:strand %path-not-beam >pax< ~)
;<  vax=(unit vase)  bind:m  (build-file:strandio u.bem)
?^  vax
  (pure:m u.vax)
(strand-fail:strand %build-file >u.bem< ~)
