/-  spider, glob
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ hash=@uv] arg)
=/  url  "https://bootstrap.urbit.org/glob-{(scow %uv hash)}.glob"
;<  =cord  bind:m  (fetch-cord:strandio url)
=+  ;;(=glob:glob (cue cord))
(pure:m !>(glob))
