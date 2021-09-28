/-  spider, glob
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ hash=@uv] arg)
=/  url  "https://bootstrap.urbit.org/glob-{(scow %uv hash)}.glob"
;<  =glob:glob  bind:m
  %+  (retry:strandio ,glob:glob)  `5
  =/  n  (strand ,(unit glob:glob))
  ;<  =cord  bind:n  (fetch-cord:strandio url)
  %-  pure:n
  %-  mole
  |.
  ;;(=glob:glob (cue cord))
(pure:m !>(glob))
