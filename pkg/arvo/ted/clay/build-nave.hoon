/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ pax=path] arg)
?~  bem=(de-beam pax)
  (strand-fail:strand %path-not-beam >pax< ~)
=/  =mark  (rear s.u.bem)
;<  =vase  bind:m  (build-nave:strandio -.u.bem mark)
(pure:m vase)
