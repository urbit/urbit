/-  spider
/+  strand, strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ pax=path] arg)
?^  bem=(de-beam pax)
  (build-file:strandio u.bem)
(strand-fail:strand %path-not-beam >pax< ~)
