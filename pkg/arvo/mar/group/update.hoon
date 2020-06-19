/+  *group-json
|_  upd=group-update
++  grad  %noun
++  grab
  |%
  ++  noun  group-update
  --
++  grow
  |%
  ++  noun  upd
  ++  json
    =,  enjs:format
    ^-  ^json
    %+  frond  %group-update
    %-  pairs
    :~
      ?:  =(%initial -.upd)
        ?>  ?=(%initial -.upd)
        :-  %initial
        (groups-to-json groups.upd)
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
