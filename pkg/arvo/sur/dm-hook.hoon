|%
++  action
  =<  action
  |%
  ::
  ++  action
    $%  accept
        decline
        pendings
        screen
    ==
  ::
  +$  accept
    [%accept =ship]
  ::
  +$  decline
    [%decline =ship]
  ::
  +$  pendings
    [%pendings ships=(set ship)]
  ::
  +$  screen
    [%screen screen=?]
  --
--
