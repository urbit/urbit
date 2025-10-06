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
=+  ;;(snap=versioned-snap (cue cord))
=?  snap  ?=(%0 -.snap)
  snap(- %1, nas (load:naive nas.snap))
?>  ?=(%1 -.snap)
;<  ~      bind:m  (poke-our:strandio %azimuth %azimuth-poke !>([%load snap]))
(pure:m !>(~))
