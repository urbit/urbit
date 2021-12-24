/+  pull-hook
/+  resource
=<  dao
|%
+$  id  term
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
  ==
+$  daoist-update
  (pair ship daoist-diff)
::
+$  daoist-diff
  $%  [%join =tx]
      [%leave ~]
      [%deputise =badge]
      [%undeputise =badge]
  ==
+$  tx  @uvH
+$  daoist
  $:  =ship
      joined=time
      attest=tx
      roles=(set badge)
  ==
+$  role
  $:  title=cord
      description=cord
  ==
::
++  graph
  |%
  +$  update
    (pair resource diff)
  +$  diff
    $%  [%sub p=sub-diff]
        [%pub p=pub-diff]
    ==
  ::
  +$  status
    $%  [%pub p=(set badge)]
        [%sub p=status:pull-hook]
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
  $:  host=ship
      daoists=(map ship daoist)
      roles=(map badge role)
      graphs=(map resource status:graph)
  ==
--
