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
    ==
  --
::
::  #  diffs
::
++  diff-hull
  $%  [%full new=hull]
      [%owner new=address]
      [%spawn-count ~]  ::  increments
      [%keys enc=@ aut=@]
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
  ::  Activated(uint32)
  ++  activated
    0xe74c.0380.9d07.69e1.b1f7.06cc.8414.258c.
      d1f3.b6fe.020c.d15d.0165.c210.ba50.3a0f
  ::
  ::  EscapeRequested(uint32,uint32)
  ++  escape-requested
    0xb4d4.850b.8f21.8218.141c.5665.cba3.79e5.
      3e9b.b015.b51e.8d93.4be7.0210.aead.874a
  ::
  ::  EscapeAccepted(uint32,uint32)
  ++  escape-accepted
    0x7e44.7c9b.1bda.4b17.4b07.96e1.00bf.7f34.
      ebf3.6dbb.7fe6.6549.0b1b.fce6.246a.9da5
  ::
  ::  ChangedKeys(uint32,bytes32,bytes32,uint32)
  ++  changed-key
    0x6a39.f4e0.c935.b557.860d.3df3.9f1f.cb6b.
      d63c.5a23.2d9e.fc28.5388.2994.f60c.708a
  --
--
