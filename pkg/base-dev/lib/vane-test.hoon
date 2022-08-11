/+  *test
/=  ames  /sys/vane/ames
/=  jael  /sys/vane/jael
::
|%
++  ames-nec-bud
  ::  create ~nec
  ::
  =/  nec  (ames ~nec)
  =.  now.nec        ~1111.1.1
  =.  eny.nec        0xdead.beef
  =.  life.ames-state.nec  2
  =.  rof.nec  |=(* ``[%noun !>(*(list turf))])
  =.  crypto-core.ames-state.nec  (pit:nu:crub:crypto 512 (shaz 'nec'))
  =/  nec-pub  pub:ex:crypto-core.ames-state.nec
  =/  nec-sec  sec:ex:crypto-core.ames-state.nec
  ::  create ~bud
  ::
  =/  bud  (ames ~bud)
  =.  now.bud        ~1111.1.1
  =.  eny.bud        0xbeef.dead
  =.  life.ames-state.bud  3
  =.  rof.bud  |=(* ``[%noun !>(*(list turf))])
  =.  crypto-core.ames-state.bud  (pit:nu:crub:crypto 512 (shaz 'bud'))
  =/  bud-pub  pub:ex:crypto-core.ames-state.bud
  =/  bud-sec  sec:ex:crypto-core.ames-state.bud
  ::
  =/  nec-sym  (derive-symmetric-key:ames bud-pub nec-sec)
  =/  bud-sym  (derive-symmetric-key:ames nec-pub bud-sec)
  ?>  =(nec-sym bud-sym)
  ::  tell ~nec about ~bud
  ::
  =.  peers.ames-state.nec
    %+  ~(put by peers.ames-state.nec)  ~bud
    =|  =peer-state:ames
    =.  -.peer-state
      :*  symmetric-key=bud-sym
          life=3
          rift=0
          public-key=bud-pub
          sponsor=~nec
      ==
    =.  route.peer-state  `[direct=%.y `lane:ames`[%& ~nec]]
    [%known peer-state]
  ::  tell ~bud about ~nec
  ::
  =.  peers.ames-state.bud
    %+  ~(put by peers.ames-state.bud)  ~nec
    =|  =peer-state:ames
    =.  -.peer-state
      :*  symmetric-key=nec-sym
          life=2
          rift=0
          public-key=nec-pub
          sponsor=~nec
      ==
    =.  route.peer-state  `[direct=%.y `lane:ames`[%| `@`%lane-bar]]
    [%known peer-state]
  ::  metamorphose
  ::
  =>  .(nec +:(call:(nec) ~[//unix] ~ %born ~))
  =>  .(bud +:(call:(bud) ~[//unix] ~ %born ~))
  ::
  [nec=nec bud=bud]
--
