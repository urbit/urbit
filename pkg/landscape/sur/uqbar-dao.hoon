/-  tx
/-  meta=dao-metadata
/+  pull-hook
/+  resource
=<  dao
|%
+$  id  @ux
::
::  TODO: these should come from chain state
::    instead, just spam them to everyone
+$  update
  (pair id diff)
::
+$  diff
  $%  [%create ~]
      [%daoist p=daoist-update]
      [%delete ~]
      [%graphs p=update:graph]
      [%initial p=dao]
  ==
+$  daoist-update
  (pair ship daoist-diff)
::
+$  daoist-diff
  $%  [%join ~]
      [%leave ~]
      [%deputise =badge]
      [%undeputise =badge]
  ==
+$  daoist
  $:  =ship
      joined=time
      attest=hash:tx
      roles=(set badge)
  ==
+$  role
  $:  title=cord
      description=cord
      color=@ux
  ==
::
++  graph
  |%
  +$  create
    (trel id resource datum:meta)
  +$  state
    [=net =datum:meta]
  +$  update
    (pair resource diff)
  +$  diff
    $%  [%sub p=sub-diff]
        [%pub p=pub-diff]
    ==
  ::
  +$  net
    $%  [%pub p=(set badge)]
        [%sub p=status:pull-hook]
        [%tub ~]
    ==
  +$  pub-diff
    $%  [%attach p=(set badge)]
        [%detach ~]
    ==
  +$  sub-diff
    ?(%sync %stop)
  --
::
+$  badge  term
+$  dao
  $:  :: on-chain
      host=ship
      daoists=(map ship daoist)
      admins=(set ship)
      ::  off-chain
      roles=(map badge role)
      graphs=(map resource state:graph)
  ==
--
