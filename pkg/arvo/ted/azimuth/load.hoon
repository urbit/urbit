/-  spider, *dice
/+  strand, strandio, naive, dice
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=/  url=tape
  ?~  lur=!<((unit tape) arg)
    "https://bootstrap.urbit.org/mainnet.azimuth-snapshot"
  u.lur
;<  =cord  bind:m  (fetch-cord:strandio url)
=+  ;;(snap=snap-state (cue cord))
;<  ~      bind:m  (poke-our:strandio %azimuth %azimuth-poke !>([%load snap]))
(pure:m !>(~))
