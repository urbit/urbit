/-  ethereum
=,  ethereum
|%
::
::  shapes
::
++  registry  (map @p hull)
::
++  hull
  $:  pilot=address
      child-count=@ud
      pub-key=@
      key-rev=@ud
      sponsor=@p
      escape=(unit @p)
  ==
::
++  ship-state
  $%  [%locked until=@da completed=@da]
      [%living ~]
  ==
::
::  constants
::
::  hashes of ship event signatures
++  ships-events
  |%
  ::
  ::  ChangedPilot(uint32,address)
  ++  changed-pilot
    0xb041.b798.8638.1a51.f9c6.29fb.4afc.6ab2.
      5059.09f4.d12e.168d.0ffc.bcb9.d78c.9179
  ::
  ::  ChangedStatus(uint32,uint8,uint64)
  ++  changed-status
    0x7d33.b6e7.2395.c6e3.c518.9773.7331.77c1.
      5ba8.9ed5.0e0e.30ca.ebaa.3877.9a3e.1a79
  ::
  ::  ChangedEscape(uint32,uint32)
  ++  changed-escape
    0x7de2.bea0.d602.2858.c601.a403.71b6.3de0.
      2940.cda9.6fef.97e4.318b.65cf.de91.5d79
  ::
  ::  ChangedSponsor(uint32,uint32)
  ++  changed-sponsor
    0x7941.482b.dede.7ff1.c27c.f2c6.e768.2155.
      a893.029d.c4a6.c619.8279.28fe.6031.9db4
  ::
  ::  ChangedKey(uint32,bytes32,uint256)
  ++  changed-key
    0xadc9.fc32.173c.d091.e0d2.ee96.60b4.b67a.
      586f.eb5a.0a30.e62c.5e9d.cfa3.573d.f8e4
  --
--
