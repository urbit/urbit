|%
+$  kind  ?(%black %white)
::
+$  permission-group-hook-action
  $%  ::  %associate: cause a group of ships to be mirrored onto some
      ::  set of permission paths.
      ::  note: this deletes any existing data at those permission paths first.
      ::
      [%associate group=path permissions=(set [path kind])]
      ::
      ::  %dissociate: stop mirroring between a group and a set
      ::  of permission paths.
      ::
      [%dissociate group=path permissions=(set path)]
  ==
--

