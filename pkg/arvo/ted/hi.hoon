/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ arg=$@(who=ship [who=ship mez=@])] arg)
=/  [who=ship message=@t]
  ?@(arg [who.arg ''] [who.arg mez.arg])
;<  ~  bind:m  (poke:strandio [who %hood] %helm-hi !>(message))
(pure:m !>((crip "hi {<who>} successful")))
