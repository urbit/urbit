/-  spider
/+  strand, strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ pax=path] arg)
?~  bam=(de-bema pax)
  (strand-fail:strand %path-not-bema >pax< ~)
=/  bem=beam  (bema-to-beam u.bam)
;<  vax=(unit vase)  bind:m  (build-file:strandio bem)
?^  vax
  (pure:m u.vax)
(strand-fail:strand %build-file >bem< ~)
