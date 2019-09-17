/-  *group-store
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
        :~  [%members (sa members.upd ship:enjs:format)]
            [%path (pa pax.upd)]
        ==
      ::
      ::  %remove
      ?:  =(%remove -.upd)
        ?>  ?=(%remove -.upd)
        :-  %remove
        %-  pairs
        :~  [%members (sa members.upd ship:enjs:format)]
            [%path (pa pax.upd)]
        ==
      ::
      ::  %bundle
      ?:  =(%bundle -.upd)
        ?>  ?=(%bundle -.upd)
        [%bundle (pairs [%path (pa pax.upd)]~)]
      ::
      ::  %unbundle
      ?:  =(%unbundle -.upd)
        ?>  ?=(%unbundle -.upd)
        [%unbundle (pairs [%path (pa pax.upd)]~)]
      ::
      ::  %keys
      ?:  =(%keys -.upd)
        ?>  ?=(%keys -.upd)
        [%keys (pairs [%keys (sa keys.upd pa)]~)]
      ::
      ::  %path
      ?:  =(%path -.upd)
        ?>  ?=(%path -.upd)
        :-  %path
        %-  pairs
        :~  [%members (sa members.upd ship:enjs:format)]
            [%path (pa pax.upd)]
        ==
      ::
      ::  %noop
      [*@t *^json]
    ==
  --
--
