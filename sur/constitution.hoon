/-  ethereum
=,  ethereum
|%
::
::  #  shapes
::
++  hull
  $:  owner=address
      encryption-key=@
      authentication-key=@
      key-revision=@ud
      spawn-count=@ud
      spawned=(set @p)
      sponsor=@p
      escape=(unit @p)
      spawn-proxy=address
      transfer-proxy=address
  ==
::
++  eth-type
  |%
  ++  hull
    :~  %address        ::  owner
        %bool           ::  active
        [%bytes-n 32]   ::  encryptionKey
        [%bytes-n 32]   ::  authenticationKey
        %uint           ::  keyRevisionNumber
        %uint           ::  spawnCount
        %uint           ::  sponsor
        %bool           ::  escapeRequested
        %uint           ::  escapeRequestedTo
        %address        ::  spawnProxy
        %address        ::  transferProxy
    ==
  --
::
++  eth-noun
  |%
  ++  hull
    $:  owner=address
        active=?
        encryption-key=octs
        authentication-key=octs
        key-revision=@ud
        spawn-count=@ud
        sponsor=@ud
        escape-requested=?
        escape-to=@ud
        spawn-proxy=address
        transfer-proxy=address
    ==
  --
::
++  function
  |%
  ++  ships
    $%  [%ships who=@p]
        [%get-spawned who=@p]
        [%dns-domains ind=@ud]
    ==
  --
::
::  #  diffs
::
++  diff-hull
  $%  [%full new=hull]
      [%owner new=address]
      [%spawned who=@p]
      [%keys enc=@ aut=@ rev=@ud]
      [%sponsor new=@p]
      [%escape new=(unit @p)]
      [%spawn-proxy new=address]
      [%transfer-proxy new=address]
  ==
::
::  #  constants
::
::  contract addresses
++  contracts
  |%
  ++  ships
    0xe083.4579.269e.ac6b.eca2.
      882a.6a21.f6fb.0b1d.7196
  --
::
::  hashes of ship event signatures
++  ships-events
  |%
  ::
  ::  Transferred(uint32,address)
  ++  transferred
    0x9014.bd16.807a.ce11.f497.2993.3667.4031.
      8029.4d9f.0e4f.42a1.5be6.0d26.5369.171c
  ::
  ::  Activated(uint32,address)
  ++  activated
     0xe5a.2849.1af6.e9a4.a694.b3a0.fa1a.7ff7.
      3b8a.a7ce.fbf5.0808.a81c.d89e.dfeb.a20d
  ::
  ::  EscapeRequested(uint32,uint32)
  ++  escape-requested
    0xb4d4.850b.8f21.8218.141c.5665.cba3.79e5.
      3e9b.b015.b51e.8d93.4be7.0210.aead.874a
  ::
  ::  EscapeCanceled(uint32)
  ++  escape-canceled
    0x68f7.f1dc.6784.a962.1f80.dcbd.3e7f.ef93.
      f77c.9228.d5c5.3dc7.a56f.10e1.c445.705a
  ::
  ::  EscapeAccepted(uint32,uint32)
  ++  escape-accepted
    0x7e44.7c9b.1bda.4b17.4b07.96e1.00bf.7f34.
      ebf3.6dbb.7fe6.6549.0b1b.fce6.246a.9da5
  ::
  ::  ChangedKeys(uint32,bytes32,bytes32,uint32)
  ++  changed-keys
    0x6a39.f4e0.c935.b557.860d.3df3.9f1f.cb6b.
      d63c.5a23.2d9e.fc28.5388.2994.f60c.708a
  ::
  ::  ChangedSpawnProxy(uint32,address)
  ++  changed-spawn-proxy
    0x9027.36af.7b3c.efe1.0d9e.840a.ed0d.687e.
      35c8.4095.122b.2505.1a20.ead8.866f.006d
  ::
  ::  ChangedTransferProxy(uint32,address)
  ++  changed-transfer-proxy
    0xcfe3.69b7.197e.7f0c.f067.93ae.2472.a9b1.
      3583.fecb.ed2f.78df.a14d.1f10.796b.847c
  ::
  ::  ChangedDns(string,string,string)
  ++  changed-dns
    0xfafd.04ad.e1da.ae2e.1fdb.0fc1.cc6a.899f.
      d424.063e.d5c9.2120.e67e.0730.53b9.4898
  --
--
