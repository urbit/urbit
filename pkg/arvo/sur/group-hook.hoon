|%
+$  group-hook-action
  $%  [%add =ship =path]             :: if ship is our, make the group publicly
                                     :: available for other ships to sync
                                     :: if ship is foreign, delete any local
                                     :: group at that path and mirror the
                                     :: foreign group at our local path
                                     ::
      [%remove =path]                :: remove the path.
  ==
--

