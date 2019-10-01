/+  *group-json
|_  upd=group-update
++  grab
  |%
  ++  noun  group-update
  --
++  grow
  |%
  ++  json
    =,  enjs:format
    ^-  ^json
    %+  frond  %group-update
    %-  pairs
    :~
      ::
      ::  %add
      ?:  =(%add -.upd)
        ?>  ?=(%add -.upd)
        :-  %add
        %-  pairs
        :~  [%members (set-to-array members.upd ship)]
            [%path (path pax.upd)]
        ==
      ::
      ::  %remove
      ?:  =(%remove -.upd)
        ?>  ?=(%remove -.upd)
        :-  %remove
        %-  pairs
        :~  [%members (set-to-array members.upd ship)]
            [%path (path pax.upd)]
        ==
      ::
      ::  %bundle
      ?:  =(%bundle -.upd)
        ?>  ?=(%bundle -.upd)
        [%bundle (pairs [%path (path pax.upd)]~)]
      ::
      ::  %unbundle
      ?:  =(%unbundle -.upd)
        ?>  ?=(%unbundle -.upd)
        [%unbundle (pairs [%path (path pax.upd)]~)]
      ::
      ::  %keys
      ?:  =(%keys -.upd)
        ?>  ?=(%keys -.upd)
        [%keys (pairs [%keys (set-to-array keys.upd path)]~)]
      ::
      ::  %path
      ?:  =(%path -.upd)
        ?>  ?=(%path -.upd)
        :-  %path
        %-  pairs
        :~  [%members (set-to-array members.upd ship)]
            [%path (path pax.upd)]
        ==
      ::
      ::  %noop
      [*@t *^json]
    ==
  --
--
